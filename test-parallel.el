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

(ert-deftest parallel-send ()
  (should (equal (parallel-get-results (parallel-start (lambda ()
                                                         (parallel-send 12)
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
                                       (parallel-send 12)
                                       (parallel-send 42)
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
      (insert-file-contents (find-library-name "parallel-remote"))
      (insert "(defun parallel--test () 42)"))
    (should (equal 42
                   (parallel-get-result
                    (parallel-start (lambda ()
                                      (parallel--test))
                                    :library-path library))))))


(provide 'test-parallel)

;;; test-parallel.el ends here
