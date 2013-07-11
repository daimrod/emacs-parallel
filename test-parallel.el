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

(ert-deftest test-parallel-basic ()
  (should (= 42 (parallel-get-result (parallel-start (lambda () 42))))))

(ert-deftest test-parallel-send ()
  (should (equal (parallel-get-results (parallel-start (lambda ()
                                                         (parallel-send 12)
                                                         42)))
                 (list 42 12))))

(ert-deftest test-parallel-status ()
  (let ((proc (parallel-start (lambda ()
                                (sleep-for 42)))))
    (should (equal (parallel-status proc) 'run))
    (parallel-stop proc)
    (should (parallel-ready-p proc))
    (should (equal (parallel-status proc) 'signal))))

(ert-deftest test-parallel-timeout ()
  (let ((proc (parallel-start (lambda () (sleep-for 42))
                              :timeout 0)))
    (should (not (parallel-success-p proc)))
    (should (equal 'signal (parallel-status proc)))))

(ert-deftest test-parallel-error ()
  (let* ((fun (lambda () (+ 1 'foo))))
    (should (equal (parallel-get-result (parallel-start fun))
                   (condition-case err
                       (funcall fun)
                     (error err))))))

(provide 'test-parallel)

;;; test-parallel.el ends here
