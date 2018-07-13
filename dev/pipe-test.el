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

;;; Pipe State Functions

(defun pt-pipe-state (pipe)
  "Return the state of `pipe'."
  (pipe-with-pipe pipe
   (list (pipe-var-ref read-pos) (pipe-var-ref num-writ) buf)))

(defun pt-pipe-state-p (x y buf-size)
  "Check that the argument list represents a valid pipe state."
  (unless (> buf-size 0)
    (error "buf-size must be positive"))
  (unless (and (<= 0 x) (< x buf-size))
    (error "First coordinate %s is not between 0 and %s" x (- buf-size 1)))
  (unless (and (<= 0 y) (<= y buf-size))
    (error "Second coordinate %s is not between 0 and %s" y buf-size)))

;;; Transformation Functions and Macros

(defun pt-pipe-transformation (from to buf-size data-src-fn)
  "Return a list of elementary transformations that will
 transform a pipe in state `from' to a pipe in state `to', where
 `data-src-fn' is called whenever data for a write operation is
 needed.  The function `data-src-fn' should have exactly one
 parameter `num' that specifies the length of the string to be
 returned."
  (cl-destructuring-bind (a b) from
    (cl-destructuring-bind (c d) to
      (pt-pipe-state-p a b buf-size)
      (pt-pipe-state-p c d buf-size)
      (let ((k (abs (- d b))))
	(if (< b d)
	    ;; Perform k writes
	    (list (cons 'w (cons k (pt-next-n-values data-src-fn k)))
		  ;; Perform k2 read-writes or write-reads
		  (let ((k2 (mod (- c a) buf-size)))
		    (cons (if (> d 0) 'rw 'wr)
			  (cons k2 (pt-next-n-values data-src-fn k2)))))
	  ;; Perform k reads
	  (list (list 'r k)
		;; Perform k2 read-writes or write-reads
		(let ((k2 (mod (- c (+ a k)) buf-size)))
		  (cons (if (> d 0) 'rw 'wr)
			(cons k2 (pt-next-n-values data-src-fn k2))))))))))

(defun pt-pipe-transform (pipe trans)
  "Apply each transformation in `trans' to `pipe'."
  (dolist (tran trans)
    (cond ((eq (car tran) 'rw)
	   (dolist (char (cddr tran))
	     (pipe-read! pipe)
	     (pipe-write! pipe char)))
	  ((eq (car tran) 'wr)
	   (dolist (char (cddr tran))
	     (pipe-write! pipe char)
	     (pipe-read! pipe)))
	  ((eq (car tran) 'r)
	   (dotimes (i (cadr tran))
	     (pipe-read! pipe)))
	  ((eq (car tran) 'w)
	   (dolist (char (cddr tran))
	     (pipe-write! pipe char))))))

(defmacro pt-list-pipe-transform (list-pipe trans)
  "Apply each transformation in `trans' to `list-pipe'."
  `(dolist (_tran ,trans)
     (cond ((eq (car _tran) 'rw)
	    (dolist (_char (cddr _tran))
	      (list-pipe-read! ,list-pipe)
	      (list-pipe-write! ,list-pipe _char)))
	   ((eq (car _tran) 'wr)
	    (dolist (_char (cddr _tran))
	      (list-pipe-write! ,list-pipe _char)
	      (list-pipe-read! ,list-pipe)))
	   ((eq (car _tran) 'r)
	    (dotimes (i (cadr _tran))
	      (list-pipe-read! ,list-pipe)))
	   ((eq (car _tran) 'w)
	    (dolist (_char (cddr _tran))
	      (list-pipe-write! ,list-pipe _char))))))

;;; Validation Macros

(defmacro pt-validate-pipes-core (_prev _pipe _list-pipe _next _tran)
  "Common/core functionality shared between the following
pt-validate-pipe macros."
  `(progn
     ;; Log the previous (current state), the next state, and the
     ;; transformation to be applied
     (when pt-debug
       (print (format "pipe: %s; list-pipe: %s"
		      (pt-pipe-state ,_pipe)
		      (list-pipe-peek-all ,_list-pipe)))
       (print (format "%s -> %s: %s" ,_prev ,_next ,_tran)))


     ;; Transform the pipes
     (should (equal (-take 2 (pt-pipe-state ,_pipe)) ,_prev))
     (pt-pipe-transform ,_pipe ,_tran)
     (should (equal (-take 2 (pt-pipe-state ,_pipe)) ,_next))
     (pt-list-pipe-transform ,_list-pipe ,_tran)

     (when pt-debug
       (print (format "pipe: %s; list-pipe: %s"
		      (pt-pipe-state ,_pipe)
		      (list-pipe-peek-all ,_list-pipe))))

     ;; Check that both pipes have the same content
     (should (equal (pipe-peek-all ,_pipe)
		    (list-pipe-peek-all ,_list-pipe)))))

(defmacro pt-validate-pipes (buf-size data-src-fn &rest body)
  "For a pipe with a buffer size of `buf-size' and `data-src-fn'
  validate `pipe-fn' against a list-based implementation of a
  pipe.  For each possible value of read-pos and num-writ, the
  pipe will assume at least one state of the form (read-pos,
  num-writ, buf)."
  `(let ((_prev (list 0 0))
	 (_pipe (pipe-make-pipe ,buf-size))
	 (_list-pipe (list-pipe-make-list-pipe)))
     (dotimes (_read-pos2 ,buf-size)
       (dotimes (_num-write2 (+ ,buf-size 1))
	 (let* ((_next (list _read-pos2 _num-write2))
		(_tran (pt-pipe-transformation _prev
					       _next
					       ,buf-size
					       ,data-src-fn)))

	   (pt-validate-pipe-core _prev _pipe _list-pipe _next _tran)

	   ,@body

	   (setq _prev _next))))))


(defmacro pt-pseudo-randomly-validate-pipes
    (buf-size data-src-fn max-times &rest body)
  "For a pipe with a buffer size of `buf-size' and `data-src-fn'
  validate `pipe-fn' against a list-based implementation of a
  pipe.  Transitions between different pipe states occur pseudo
  randomly."
  `(let ((_curr (list 0 0))
	 (_pipe (pipe-make-pipe ,buf-size))
	 (_list-pipe (list-pipe-make-list-pipe)))
     (dotimes (_i ,max-times)
       (let* ((_next (list (random ,buf-size) (random (+ ,buf-size 1))))
	      (_tran (pt-pipe-transformation _curr
					     _next
					     ,buf-size
					     ,data-src-fn)))

	 (pt-validate-pipe-core _curr _pipe _list-pipe _next _tran)

	 ,@body

	 (setq _curr _next)))))

(defun pt-pipes-write-read ()
  (pt-validate-pipe 1024 'pt-pseudo-random-ascii-string-visible))

(defun pt-pipes-write-ln-read-ln ()
  (let* ((pipe (pipe-make-pipe))
	 (list-pipe (list-pipe-make-list-pipe))
	 (string-length 1023)
	 (num-strings 10)
	 (strings (pt-next-n-values
		   (lambda (i)
		     (apply 'concat (pt-next-n-values
				     'pt-pseudo-random-ascii-string-visible
				     string-length)))
		   num-strings)))
    (dolist (str strings)
      (pipe-write-ln! pipe str)
      (list-pipe-write-ln! list-pipe str))
    (dolist (str strings)
      (should (equal (concat str pipe-default-newline-delim)
		     (pipe-read-ln! pipe)))
      (should (equal (concat str pipe-default-newline-delim)
		     (list-pipe-read-ln! list-pipe))))))

(defun pt-pipes-write-read-all ()
  (let* ((pipe (pipe-make-pipe))
	 (list-pipe (list-pipe-make-list-pipe))
	 (string-length 1023)
	 (num-strings 10)
	 (strings (pt-next-n-values
		   (lambda (i)
		     (apply 'concat (pt-next-n-values
				     'pt-pseudo-random-ascii-string-visible
				     string-length)))
		   num-strings)))
    (dolist (str strings)
      (pipe-write! pipe str)
      (list-pipe-write! list-pipe str))
    (should (equal (apply 'concat strings)
		   (pipe-read-all! pipe)))
    (should (equal (apply 'concat strings)
		   (list-pipe-read-all! list-pipe)))))

(defun pt-pipes-write-sexp-read-sexp ()
  (let* ((pipe (pipe-make-pipe 200))
	 (list-pipe (list-pipe-make-list-pipe))
	 (sexps '("abc" def\ ghi 1 21.3 '(j k lm)
		  [1 two '(three) "four" [five]])))

    (dolist (sexp sexps)
      (pipe-write-sexp! pipe sexp)
      (list-pipe-write-sexp! list-pipe sexp))

    (dolist (sexp sexps)
      (should (equal sexp
		     (pipe-read-sexp! pipe)))
      (should (equal sexp
		     (list-pipe-read-sexp! list-pipe))))))

(provide 'pipe-test)
;; pipe-test.el ends here
