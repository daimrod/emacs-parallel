;; -*- mode: emacs-lisp; lexical-binding: t; -*-
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

;; Declare external function
(declare-function parallel-send "parallel-remote")

(defun* parallel-start (exec-fun &key post-exec timeout env emacs-args no-batch debug)
  (let* ((serv (make-network-process :name "emacs-parallel"
                                     :buffer nil
                                     :server t
                                     :service (make-temp-name "/tmp/parallel-")
                                     :family 'local
                                     :filter-multibyte t))
         (proc (apply #'start-process "emacs-parallel" nil (file-truename
                                                   (expand-file-name invocation-name
                                                                     invocation-directory))
                      (remq nil
                            (list* "-Q" "-l" (find-library-name "parallel-remote")
                                   (if no-batch nil "-batch")
                                   "--eval" (format "(setq parallel-service %S)" (process-contact serv :service))
                                   "--eval" (format "(setq debug-on-error %s)" debug)
                                   "-f" "parallel--init"
                                   emacs-args)))))
    (process-put proc 'initialized nil)
    (set-process-filter serv (parallel--make-filter proc exec-fun env))
    (process-put proc 'server serv)
    (when (functionp post-exec)
      (process-put proc 'post-exec post-exec))
    (process-put proc 'results nil)
    (process-put proc 'status 'run)
    (set-process-sentinel proc #'parallel--sentinel)
    (when timeout
      (run-at-time timeout nil (lambda ()
                                 (when (memq (process-status proc)
                                             '(run stop))
                                   (kill-process proc)))))
    proc))

(defun parallel--sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let ((results (process-get proc 'results))
          (status (process-status proc)))
      (if (zerop (process-exit-status proc))
          (setq status 'success)
        (push (process-exit-status proc) results))
      (process-put proc 'results results)
      (process-put proc 'status status)
      (delete-process (process-get proc 'server))
      (when (functionp (process-get proc 'post-exec))
        (funcall (process-get proc 'post-exec)
                 results status)))))

(defun parallel--call-with-env (fun env)
  (format "(funcall (read %S) %s)"
          (prin1-to-string fun)
          (mapconcat (lambda (obj)
                       (format "'%S" obj)) env " ")))

(defun parallel--make-filter (master-proc exec-fun env)
  (lambda (proc output)
    (cond ((and (not (process-get master-proc 'initialized))
                (eq (read output) 'code)
                (eq (process-status proc) 'open))
           (process-send-string proc (parallel--call-with-env exec-fun env))
           (process-put master-proc 'initialized t))
          (t
           (loop with start = 0
                 with end = (length output)
                 with error = nil
                 for ret = (condition-case err
                               (read-from-string output start end)
                             (error (setq error err)))
                 do (process-put master-proc 'results
                                 (cons (or error
                                           (first ret))
                                       (process-get master-proc 'results)))
                 do (setq start (unless error
                                  (rest ret)))
                 until (or error (= start end)))))))

(defun parallel-ready (proc)
  (memq (process-get proc 'status) '(success exit signal)))

(defun parallel-get-result (proc)
  (first (parallel-get-results proc)))

(defun parallel-get-results (proc)
  (parallel-wait proc)
  (process-get proc 'results))

(defun parallel-success-p (proc)
  (parallel-wait proc)
  (eq (process-get proc 'status) 'success))

(defun parallel-wait (proc)
  (while (not (parallel-ready proc))
    (sleep-for parallel-sleep))
  t)                                    ; for REPL


(provide 'parallel)

;;; parallel.el ends here
