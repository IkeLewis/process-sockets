;;; buffered-streams.el --- Buffered streams for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

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
;; A buffered stream is a buffer together with several operations
;; (bs-read, bs-drain-input, and bs-write) and has the following
;; properties:
;;
;;     buf_size: the size of the stream's buffer
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

;;; Code:

;; For 'defun*'
(require 'cl)

;;; Customizable Variables

(defvar bs-debug nil "Set to true to log debugging info in the *Messages* buffer.")

(defvar bs-buf-size (* 64 1024) "The default buffer size for buffered streams.")

;;; First-class variables

(defun bs-make-var (val)
  (list val))

(defalias 'bs-var-ref 'car)

(defmacro bs-set-var! (var new-val)
  `(setf (car ,var) ,new-val))

(defmacro bs-inc-var! (var amt)
  `(setf (car ,var) (+ (car ,var) ,amt)))

(defmacro bs-dec-var! (var amt)
  `(setf (car ,var) (- (car ,var) ,amt)))

(defmacro bs-inc-var-mod-n! (var amt n)
  `(setf (car ,var) (mod (+ (car ,var) ,amt) ,n)))

(defmacro bs-dec-var-mod-n! (var amt n)
  `(setf (car ,var) (mod (- (car ,var) ,amt) ,n)))

;;; Utility functions

(defun bs-memcpy! (src dest dest-offset)
  "Copy `src' to `dest' starting at offset `dest-offset'; if the
length of `src' plus `dest-offset' is greater than the length of
`dest', then the writing wraps.  If `src' is longer then `dest'
then a buffer overflow error is thrown.  If `dest-offset' is out
of range, then an out-of-range error is thrown."
  (unless (<= (length src) (length dest))
    (error "Buffer overflow"))
  (unless (< dest-offset (length dest))
    (error "Invalid offset '%s'" dest-offset))

  (if (<= (+ dest-offset (length src)) (length dest))
      (store-substring dest dest-offset src)
    (progn (store-substring dest dest-offset (substring src 0 (- (length dest)
								 dest-offset)))
	   (store-substring dest 0 (substring src (- (length dest)
						     dest-offset))))))

;;; Debugging/logging functions

(defun bs-debug (fmt-str &rest args)
  (when bs-debug
    (apply 'message (concat "bs-debug: " fmt-str "\n") args)))

;;; Private Macros and Functions

(defmacro bs-with-buffered-stream (stream &rest body)
  "Evaluate `body' in the environment of `stream'."
  `(let* ((env (funcall ,stream 'env))
	  (num-writ (cdr (assoc 'num-writ env)))
	  (write-pos (cdr (assoc 'write-pos env)))
	  (num-read (cdr (assoc 'num-read env)))
	  (read-pos (cdr (assoc 'read-pos env)))
	  (buf (cdr (assoc 'buf env)))
	  (underflow-handler (cdr (assoc 'underflow-handler env))))
     ,@body))

;;; Public API

(defun* bs-make-buffered-stream (&optional (buf-size bs-buf-size)
					   (underflow-handler
					    (lambda ()
					      (error "Buffer underflow (plain)"))))
  "Create a new buffered stream."
  (let* ( ;; The environment
	 (env `((num-writ . ,(bs-make-var 0))
		(write-pos . ,(bs-make-var 0))
		(num-read . ,(bs-make-var buf-size))
		(read-pos . ,(bs-make-var 0))
		(buf . ,(make-string buf-size 0))
		(underflow-handler .,underflow-handler))))
    (lambda (fn-or-var &rest args)
      (case fn-or-var
	((env)
	 env)
	(t
	 (error "Invalid arguments"))))))

(defun bs-read (stream &optional unread)
  "Reads a character from `stream' if `unread' is nil.  Otherwise
unreads the character `unread' from `stream'."
  ;; This function must support two kinds of calls:
  ;;
  ;; • When it is called with no arguments, it should return the next
  ;;   character.
  ;;
  ;; • When it is called with one argument (always a character), it
  ;;   should save the argument and arrange to return the argument on
  ;;   the next call.  This is called “unreading” the character; it
  ;;   happens when the Lisp reader reads one character too many and
  ;;   wants to put it back where it came from.  In this case, it
  ;;   makes no difference what value is returned.
  (bs-with-buffered-stream
   stream
   (let ((buf-size (length buf)))
     (cond (unread
	    (bs-debug "unreading %s" unread)
	    (if (= (bs-var-ref num-writ) buf-size)
		(progn
		  (error "Buffer overflow (unread)"))
	      (prog1 unread
		(bs-inc-var! num-read  -1)
		(bs-inc-var! num-writ 1)
		;; unreading does not alter write-pos
		(bs-dec-var-mod-n! read-pos 1 buf-size))))
	   ((= (bs-var-ref num-read) buf-size)
	    (bs-debug "handling undeflow")
	    (bs-debug "got input %s"
		      (funcall underflow-handler))
	    (bs-read stream))
	   (t
	    (let ((res (prog1 (aref buf (bs-var-ref read-pos))
			 (bs-inc-var! num-read  1)
			 (bs-inc-var! num-writ -1)
			 ;; reading does not alter write-pos
			 (bs-inc-var-mod-n! read-pos 1 buf-size))))
	      (bs-debug "read %c" res)
	      res))))))

(defun bs-write (stream string)
  "Writes the `string' to the `stream'."
  (bs-with-buffered-stream
   stream
   (let ((buf-size (length buf)))
     (cond ((> (+ (bs-var-ref num-writ) (length string)) buf-size)
	    (error "Buffer overflow"))
	   (t
	    (bs-debug "wrote '%s'" string)
	    (prog1 (bs-memcpy! string buf (bs-var-ref write-pos))
	      (bs-inc-var! num-writ (length string))
	      (bs-dec-var! num-read (length string))
	      ;; writing does not alter the read position
	      (bs-inc-var-mod-n! write-pos (length string) buf-size)))))))

(defun bs-drain-input (stream)
  "Drains (reads all characters) currently in the stream's buffer
and returns the result as a string."
  (bs-with-buffered-stream
   stream
   (let* ((buf-size (length buf))
	  (after-last-pos (mod (+ (bs-var-ref read-pos)
				  (bs-var-ref num-writ))
			       buf-size)))
     ;; after-last-pos -- the position just after the last
     ;; character to be read
     (prog1
	 (if (< after-last-pos (bs-var-ref read-pos))
	     (concat (substring buf (bs-var-ref read-pos))
		     (substring buf 0 after-last-pos))
	   (substring buf (bs-var-ref read-pos) after-last-pos))
       (bs-inc-var-mod-n! read-pos (bs-var-ref num-writ) buf-size)
       (bs-set-var! num-read buf-size)
       ;; reading does not alter the write position
       (bs-set-var! num-writ 0)))))

(provide 'buffered-streams)
;;; buffered-streams.el ends here
