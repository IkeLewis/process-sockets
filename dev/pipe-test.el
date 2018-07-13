;;; pipe-test.el --- Test Emacs-pipe implementations -*- lexical-binding: t -*-

;; Copyright (C) 2018 Isaac Lewis

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Version: 1.0.0
;; Keywords: comm

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO

;;; Code:

;;; Required Libraries

;; For '-take'
(require 'dash)
;; For 'should'
(require 'ert)
;; For 'pipe-make-pipe'
(require 'pipe)
;; For 'list-pipe-make-pipe'
(require 'list-pipe)

;;; Customization Variables

(defvar pt-debug nil "Whether or not to print debugging messages.")

;;; Utility Functions

(defun pt-next-n-values (data-src-fn n)
  "Return a list of the next n values returned by `data-src-fn'."
  (mapcar (lambda (x) (funcall data-src-fn 1)) (number-sequence 1 n)))

;;; String Generators

(defun pt-pseudo-random-ascii-string (n)
  "Return a pseudo-randomly generated ASCII string."
  (concat (mapcar (lambda (x) (random 128)) (number-sequence 1 n))))

(defun pt-pseudo-random-ascii-string-visible (n)
  "Return a pseudo-randomly generated ASCII string of visible
characters."
  (concat (mapcar (lambda (x)
		    (let ((char))
		      (while (or (not char) (< char 33) (= char 127))
			(setf char (random 128)))
		      char))
		  (number-sequence 1 n))))

(defun pt-null-string (n)
  "Return a string of length n containing only null bytes."
  (make-string n 0))

(provide 'pipe-test)
;; pipe-test.el ends here
