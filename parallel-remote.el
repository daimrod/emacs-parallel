;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; parallel-remote.el ---

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

(defvar parallel-service nil)
(defvar parallel-client nil)

(defun parallel-send (data)
  (process-send-string parallel-client
                       (format "%S" data)))

(defun parallel--init ()
  (setq parallel-client (make-network-process :name "emacs-parallel"
                                              :buffer nil
                                              :server nil
                                              :service parallel-service
                                              :family 'local)))



(provide 'parallel-remote)

;;; parallel-remote.el ends here
