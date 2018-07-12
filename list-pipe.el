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

;;}}}

;;}}}

(provide 'list-pipe)
;;; list-pipe.el ends here
