;;; process-sockets.el --- Process sockets for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Isaac Lewis

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Version: 1.0.0
;; Keywords: processes, comm

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

;; Process sockets for Emacs
;;
;; See the documentation at https://github.com/IkeLewis/process-sockets
;;

;;; Code:

;;; Requirements

;; For 'defun*'
(require 'cl)
;; For 'pipe-make-pipe'
(require 'pipe)

;;; API Functions

;;; Constructor(s)

(defun* ps-make-process-socket (process &optional (pipe-buf-size pipe-default-buf-size))
  "Creates a socket for communicating with `process'."
  (unless (>= emacs-major-version 26)
    (error "Emacs 26+ is required"))
  (let* ((process process)
	 (sock-mutex (make-mutex))
	 (sock-output-ready nil)
	 (sock-cv-output-ready (make-condition-variable sock-mutex "sock-cv-output-ready"))
	 (cv-output-ready (make-condition-variable sock-mutex "cv-output-socket"))
	 (input-pipe (pipe-make-pipe
		      pipe-buf-size
		      (lambda ()
			(if (equal (current-thread)
				   (process-thread process))
			    (catch 'done
			      (while t
				(with-mutex sock-mutex
				  (when sock-output-ready
				    (setq sock-output-ready nil)
				    (throw 'done t)))
				(sleep-for 0 1)))
			  (with-mutex sock-mutex
			    (while (not sock-output-ready)
			      (condition-wait sock-cv-output-ready))
			    (setq sock-output-ready nil))))))
	 (output-pipe (pipe-make-pipe pipe-buf-size))
	 (auto-flush nil))
    ;; Write output from the process to the socket's input stream.
    (set-process-filter process (lambda (process string)
				  (with-mutex sock-mutex
				    (pipe-write! input-pipe string)
				    (setq sock-output-ready t)
				    (condition-notify sock-cv-output-ready t))))
    (lambda (fn-or-var &rest args)
      (case fn-or-var
	;; Accessors
	((process)
	 process)
	((input-pipe)
	 input-pipe)
	((output-pipe)
	 output-pipe)
	((auto-flush)
	 auto-flush)
	;; Mutators
	((set-auto-flush!)
	 (setf auto-flush (car args)))
	(t
	 (error "Invalid arguments"))))))

(defun ps-input-stream (ps)
  (funcall ps 'input-stream))

(defun ps-output-stream (ps)
  (funcall ps 'output-stream))

(defun ps-read-char (ps)
  (funcall ps 'read-char))

(defun ps-read-sexp (ps)
  (funcall ps 'read-sexp))

(defun ps-write-string (ps string)
  (funcall ps 'write-string string))

(defun ps-write-sexp (ps sexp)
  (funcall ps 'write-sexp sexp))

(defun ps-drain-input (ps)
  (funcall ps 'drain-input))

(defun ps-close (ps)
  (funcall ps 'close))

(provide 'process-sockets)
;;; process-sockets.el ends here
