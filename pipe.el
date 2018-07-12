;;; pipe.el --- Pipes for Emacs -*- lexical-binding: t -*-

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

;; Definitions
;; -----------
;;
;; An (Emacs) pipe is a buffer together with several operations
;; (pipe-read, pipe-write, etc) and has the following properties:
;;
;;     buf_size: the size of the pipe's buffer
;;
;;     num_writ: the number of characters written to the buffer that
;;     have yet to be read
;;
;;     num_read: the number of characters read from the buffer that
;;     have yet to be written over
;;
;;     read_pos: the position of the next character to be read; it
;;     must be an integer from 0 to buf_size-1
;;
;;     write_pos: the position of the next character to be written; it
;;     must be an integer from 0 to buf_size-1
;;
;; Buffer State
;; ------------
;;
;; Each position in the buffer may be in one of two states; that is,
;; at any point in time, each position in the buffer has either been
;; read from but not yet written over, or written over but not yet
;; read from.  Thus the following invariant holds:
;;
;;     num_read + num_writ = buf_size
;;
;; Initially, all values in the buffer are said to be read from but
;; not written to.
;;
;; Changes in Buffer State
;; -----------------------
;;
;; The state of the buffer can be changed in precisely two ways: by
;; writing characters to it or by reading characters from it.
;;
;; Initially, read_pos = write_pos = num_write = 0, and num_read =
;; buf_size.
;;
;; If c chars are to be read and c > num_writ, then an underflow
;; occurs.  Otherwise, num_read is incremented by c, num_writ is
;; decremented by c, and read_pos is incremented by c modulo buf_size.
;;
;; If a character is unread and 1 + num_writ > buf_size, then an
;; overflow occurs.  Otherwise, num_read is decremented by 1, num_writ
;; is incremented by 1, and read_pos is decremented by one modulo
;; buf_size.
;;
;; If c chars are to be written over and c + num_writ > buf_size, then
;; an overflow occurs.  Otherwise, num_writ is incremented by c,
;; num_read is decremented by c, and write_pos is incremented by c
;; modulo buf_size.
;;
;; Example
;; -------
;;
;; The columns below show the state of the buffer and its
;; corresponding variables immediately after the given event occurs.
;;
;; |-----------------+--------+----------+-----------+----------+----------|
;; | event           | buffer | num_writ | write_pos | num_read | read_pos |
;; |-----------------+--------+----------+-----------+----------+----------|
;; | initialization  | rrrrr  |        0 |         0 |        5 |        0 |
;; | writing 1 char  | wrrrr  |        1 |         1 |        4 |        0 |
;; | writing 1 char  | wwrrr  |        2 |         2 |        3 |        0 |
;; | reading 1 char  | rwrrr  |        1 |         2 |        4 |        1 |
;; | writing 3 chars | rwwww  |        4 |         0 |        1 |        1 |
;; |-----------------+--------+----------+-----------+----------+----------|
;;

;;{{{
;;; Code:

;; For 'defun*'
(require 'cl)

;;{{{
;;; Customizable Variables

(defvar pipe-debug nil "Set to true to log debugging info in the
*Messages* buffer.")

(defvar pipe-default-buf-size 65536 "The default buffer size for pipes.")

(defvar pipe-default-newline-delim "\n" "This should usually be set
to the default newline string used by the OS.")

;;}}}

(provide 'pipe)
;;; pipe.el ends here
