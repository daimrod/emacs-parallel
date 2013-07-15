;; -*- lexical-binding: t; -*-
;;; parallel.el ---

;; Copyright (C) 2013 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)

(defgroup parallel nil
  "Execute stuff in parallel"
  :group 'emacs)

(defcustom parallel-sleep 0.05
  "How many sec should we wait while polling."
  :type 'number
  :group 'parallel)

(defvar parallel--server nil)
(defvar parallel--tasks nil)

;; Declare external function
(declare-function parallel-send "parallel-remote")

(defun* parallel-start (exec-fun &key post-exec env timeout
                                 emacs-path library-path emacs-args
                                 graphical debug on-event
                                 config)
  ;; Be sure we have a running `parallel--server'
  (when (or (null parallel--server)
            (not (eq (process-status parallel--server)
                     'listen)))
    (parallel--init-server))

  ;; Initialize parameters
  (setq post-exec (or post-exec (plist-get config :post-exec))
        env (or env (plist-get config :env))
        timeout (or timeout (plist-get config :timeout))
        emacs-path (file-truename
                    (or emacs-path
                        (plist-get config :emacs-path)
                        (expand-file-name invocation-name
                                          invocation-directory)))
        library-path (or library-path
                         (plist-get config :library-path)
                         (find-library-name "parallel-remote"))
        emacs-args (or emacs-args (plist-get config :emacs-args))
        graphical (or graphical (plist-get config :graphical))
        debug (or debug (plist-get config :debug))
        on-event (or on-event (plist-get config :debug)))
  
  (let ((task (parallel--new-task))
        proc)
    (push task parallel--tasks)
    (put task 'initialized nil)
    (put task 'exec-fun exec-fun)
    (put task 'env env)
    (when (functionp post-exec)
      (put task 'post-exec post-exec))
    (when (functionp on-event)
      (put task 'on-event on-event))
    (put task 'results nil)
    (put task 'status 'run)
    (setq proc (apply #'start-process "emacs-parallel" nil emacs-path
                      (remq nil
                            (list* "-Q" "-l" library-path
                                   (if graphical nil "-batch")
                                   "--eval" (format "(setq parallel-service '%S)" (process-contact parallel--server :service))
                                   "--eval" (format "(setq parallel-task-id '%S)" task)
                                   "--eval" (format "(setq debug-on-error %s)" debug)
                                   "-f" "parallel-remote--init"
                                   emacs-args))))
    (put task 'proc proc)
    (set-process-sentinel (get task 'proc) #'parallel--sentinel)
    (when timeout
      (run-at-time timeout nil (lambda ()
                                 (when (memq (parallel-status task)
                                             '(run stop))
                                   (parallel-stop task)))))
    task))

(defun parallel--new-task ()
  "Generate a new task by enforcing a unique name."
  (let ((symbol-name (make-temp-name "emacs-parallel-task-")))
    (while (intern-soft symbol-name)
      (setq symbol-name (make-temp-name "emacs-parallel-task-")))
    (intern symbol-name)))

(defun parallel--init-server ()
  (setq parallel--server
        (make-network-process :name "emacs-parallel-server"
                              :buffer nil
                              :server t
                              :host "localhost"
                              :service t
                              :family 'ipv4
                              :filter #'parallel--filter
                              :filter-multibyte t)))

(defun parallel--get-task-process (proc)
  (find-if (lambda (task)
             (eq (get task 'proc) proc))
           parallel--tasks))

(defun parallel--sentinel (proc _event)
  "Sentinel to watch over the remote process.

This function do the necessary cleanup when the remote process is
finished."
  (when (memq (process-status proc) '(exit signal))
    (let* ((task (parallel--get-task-process proc))
           (results (get task 'results))
           (status (process-status proc)))
      ;; 0 means that the remote process has terminated normally (no
      ;; SIGNUM 0).
      (if (zerop (process-exit-status proc))
          (setq status 'success)
        ;; on failure, push the exit-code or signal number on the
        ;; results stack.
        (push (process-exit-status proc) results))
      (put task 'results results)
      (put task 'status status)

      (when (functionp (get task 'post-exec))
        (funcall (get task 'post-exec)
                 results status))
      (setq parallel--tasks (delq task parallel--tasks)))))

(defun parallel--call-with-env (fun env)
  "Return a string which can be READ/EVAL by the remote process
to `funcall' FUN with ENV as arguments."
  (format "(funcall (read %S) %s)"
          (prin1-to-string fun)
          (mapconcat (lambda (obj)
                       ;; We need to quote it because the remote
                       ;; process will READ/EVAL it.
                       (format "'%S" obj)) env " ")))

(defun parallel--filter (connection output)
  "Server filter used to retrieve the results send by the remote
process and send the code to be executed by it."
  (loop with output = (replace-regexp-in-string
                       "\\`[ \t\n]*" ""
                       (replace-regexp-in-string "[ \t\n]*\\'" "" output)) ; trim string
        with start = 0
        with end = (length output)
        for ret = (read-from-string output start end)
        for data = (first ret)
        do (setq start (rest ret))
        do (parallel--process-output connection (first data) (rest data))
        until (= start end)))

(defun parallel--process-output (connection task result)
  (cond ((and (not (get task 'initialized))
              (eq result 'code))
         (process-send-string connection
                              (parallel--call-with-env (get task 'exec-fun)
                                                       (get task 'env)))
         (put task 'initialized t))
        (t
         (push result (get task 'results))
         (if (functionp (get task 'on-event))
             (funcall (get task 'on-event) result)))))

(defun parallel-ready-p (task)
  "Determine whether TASK is finished and if the results are
available."
  (memq (parallel-status task) '(success exit signal)))

(defun parallel-get-result (task)
  "Return the last result send by the remote call, that is the
result returned by exec-fun."
  (first (parallel-get-results task)))

(defun parallel-get-results (task)
  "Return all results send during the call of exec-fun."
  (parallel-wait task)
  (get task 'results))

(defun parallel-success-p (task)
  "Determine whether TASK has ended successfully."
  (parallel-wait task)
  (eq (parallel-status task) 'success))

(defun parallel-status (task)
  "Return TASK status."
  (get task 'status))

(defun parallel-wait (task)
  "Wait for TASK."
  (while (not (parallel-ready-p task))
    (sleep-for parallel-sleep))
  t)                                    ; for REPL

(defun parallel-stop (task)
  "Stop TASK."
  (delete-process (get task 'proc)))

(provide 'parallel)

;;; parallel.el ends here
