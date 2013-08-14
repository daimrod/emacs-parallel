;;; test-parallel.el ---

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
(require 'ert)
(require 'parallel)

(ert-deftest parallel-basic ()
  (should (= 42 (parallel-get-result (parallel-start (lambda () 42))))))

(ert-deftest parallel-remote-send ()
  (should (equal (parallel-get-results (parallel-start (lambda ()
                                                         (parallel-remote-send 12)
                                                         42)))
                 (list 42 12))))

(ert-deftest parallel-status ()
  (let ((proc (parallel-start (lambda ()
                                (sleep-for 42)))))
    (should (equal (parallel-status proc) 'run))
    (parallel-stop proc)
    (should (parallel-ready-p proc))
    (should (equal (parallel-status proc) 'signal))))

(ert-deftest parallel-timeout ()
  (let ((proc (parallel-start (lambda () (sleep-for 42))
                              :timeout 0)))
    (should (not (parallel-success-p proc)))
    (should (equal 'signal (parallel-status proc)))))

(ert-deftest parallel-error ()
  (let* ((fun (lambda () (+ 1 'foo))))
    (should (equal (parallel-get-result (parallel-start fun))
                   (condition-case err
                       (funcall fun)
                     (error err))))))

(ert-deftest parallel-on-event ()
  (let ((ret 0))
    (should (equal
             (apply #'+
                    (parallel-get-results
                     (parallel-start (lambda ()
                                       (parallel-remote-send 12)
                                       (parallel-remote-send 42)
                                       0)
                                     :on-event
                                     (lambda (data)
                                       (incf ret data)))))
             ret))))

(ert-deftest parallel-other-version ()
  "I use my own, compiled, emacs."
  (should (equal (emacs-version)
                 (parallel-get-result
                  (parallel-start (lambda ()
                                    (emacs-version))))))
  (should-not (equal (emacs-version)
                     (parallel-get-result
                      (parallel-start (lambda ()
                                        (emacs-version))
                                      :emacs-path "/usr/bin/emacs23")))))

(ert-deftest parallel-other-library ()
  (let ((library (make-temp-file "parallel-")))
    (with-temp-file library
      (insert-file-contents (locate-library "parallel-remote"))
      (insert "(defun parallel--test () 42)"))
    (should (equal 42
                   (parallel-get-result
                    (parallel-start (lambda ()
                                      (parallel--test))
                                    :library-path library))))))

(ert-deftest parallel-multiple-tasks ()
  (let ((t1 (parallel-start (lambda () (sleep-for (random 4)) 1)))
        (t2 (parallel-start (lambda () (sleep-for (random 4)) 2)))
        (t3 (parallel-start (lambda () (sleep-for (random 4)) 3)))
        (t4 (parallel-start (lambda () (sleep-for (random 4)) 4)))
        (t5 (parallel-start (lambda () (sleep-for (random 4)) 5))))
    (should (equal (list 1 2 3 4 5)
                   (list (parallel-get-result t1)
                         (parallel-get-result t2)
                         (parallel-get-result t3)
                         (parallel-get-result t4)
                         (parallel-get-result t5))))))

(ert-deftest parallel-zlist ()
  ;; HACKY ERT run tests in alphabetical order, hence the name.
  (should (eq nil parallel--tasks)))

(ert-deftest parallel-continue-when-executed ()
  (let ((task (parallel-start (lambda () 42) :continue-when-executed t :timeout 5)))
    (parallel-send task (lambda () (setq parallel-continue-when-executed nil)))
    (should (equal (list nil 42)
                   (parallel-get-results task)))))

(provide 'test-parallel)

;;; test-parallel.el ends here
