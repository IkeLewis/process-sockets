;;; list-pipe.el --- List-Pipes for Emacs -*- lexical-binding: t -*-

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

;;{{{
;; Code:

;;; Required Libraries
;; For 'pipe-default-newline-delim'
(require 'pipe)

;;{{{
;;; Customization Variables

(defvar list-pipe-debug nil "Whether or not to print debugging
messages.")

(defvar list-pipe-default-underflow-handler (lambda () (error "Underflow")))

;;}}}

;;{{{
;;; List Pipe Functions & Macros

(defun list-pipe-make-list-pipe ()
  "Make a list-based pipe."
  nil)

;;{{{
;;; Peeking Functions

(defun list-pipe-peek (list-pipe)
  "Return the next character to be read from pipe, but don't
modify the pipe."
  (car list-pipe))

(defun list-pipe-ln (list-pipe)
  "Return the next line to be read from pipe, but don't modify the
  pipe."
  (let ((line (list-pipe-read-ln! list-pipe)))
    (dolist (char (reverse line))
      ;; unread the character
      (list-pipe-read! list-pipe char))))

(defun list-pipe-sexp (list-pipe)
  "Return the next sexp to be read from pipe, but don't modify
  the pipe."
  (let ((sexp (list-pipe-read-sexp! list-pipe)))
    (dolist (char (reverse sexp))
      ;; unread the character
      (list-pipe-read! list-pipe char))))

(defun list-pipe-peek-all (list-pipe)
  "Return a string containing all of `list-pipe's currently
  available input, but don't modify `list-pipe'."
  (concat list-pipe))

;;}}}

;;}}}

;;}}}

(provide 'list-pipe)
;;; list-pipe.el ends here
