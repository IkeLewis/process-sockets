;;; process-sockets-test.el --- Tests for process sockets -*- lexical-binding: t -*-

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

;; For 'should'
(require 'ert)
;; For 'ps-make-process-socket'
(require 'process-sockets)

;;; Debugging and Logging

(defvar pst-debug nil "Whether or not to print debugging messages.")

(defun pst-debug (fmt-str &rest args)
  "Print a formatted debugging message, where `fmt-str' is a
format string with arguments `args'."
  (when pst-debug
    (print (concat "pst-debug: "
		   (apply 'format message args)
		   "\n"))))

;;; Preparation Functions

(defun pst-prepare-socket (&optional auto-flush)
  "Prepare a socket for testing.  If `auto-flush' is set to t,
then all socket operations automatically flush."
  (let* ((my-proc (start-process "bash-proc1"
				 (get-buffer-create "bash-proc1")
				 "/bin/bash"))
	 (my-sock (ps-make-process-socket my-proc)))

    (ps-set-auto-flush! my-sock auto-flush)
    (pst-debug "auto-flush: %s" (ps-auto-flush my-sock))

    ;; Wait until the prompt is received
    (ps-read! my-sock)
    ;; Discard the prompt characters
    (ps-read-all! my-sock)
    ;; Set an empty prompt
    (ps-write-ln! my-sock "export PS1=\"\"")
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))

    my-sock))

;;; Individual Tests

(defun pst-output-stream-input-stream! (my-sock)
  (let ((test-sexp '(PST-OUTPUT-STREAM-INPUT-STREAM!)))
    (ps-write! my-sock "echo -n '")
    (prin1 test-sexp (ps-output-stream my-sock))
    (ps-write-ln! my-sock "'")
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))
    (let ((recv (read (ps-input-stream my-sock))))
      (pst-debug "pst-output-stream-input-stream!: %s" recv)
      (should (equal recv test-sexp)))))

(defun pst-write!-read! (my-sock)
  (let ((test-str "PST-WRITE!-READ!"))
    (ps-write! my-sock (concat "echo -n 'PST-WRITE!-READ!'"
			       pipe-default-newline-delim))
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))
    (dotimes (i (length test-str))
      (let ((char (ps-read! my-sock)))
	(pst-debug "pst-write!-read!: %c" char)
	(should (equal (elt test-str i) char))))))

(defun pst-write!-read-all! (my-sock)
  (let ((test-str "PST-WRITE!-READ-ALL!"))
    (ps-write! my-sock "echo -n 'Hello world!'\n")
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))
    ;; wait for the input to arrive
    (let* ((recv (concat (ps-read! my-sock) (ps-read-all! my-sock))))
      (pst-debug "pst-write!-read-all!: %c%s" )
      (should (equal recv test-str)))))

(defun pst-write-ln!-read-ln! (my-sock)
  (let ((test-str "PST-WRITE-LN!-READ-LN!"))
    (ps-write-ln! my-sock (format "echo '%s'" test-str))
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))
    (let ((recv (ps-read-ln! my-sock)))
      (pst-debug "pst-write-ln!-read-ln!: %s" recv)
      (should (equal recv (concat test-str pipe-default-newline-delim))))))

(defun pst-write-sexp!-read-sexp! (my-sock)
  (let ((test-sexp '(PST-WRITE-SEXP!-READ-SEXP!)))
    (ps-write! my-sock "echo -n '")
    (ps-write-sexp! my-sock test-sexp)
    (ps-write-ln! my-sock "'")
    (unless (ps-auto-flush my-sock)
      (ps-flush! my-sock))
    (let ((recv (ps-read-sexp! my-sock)))
      (pst-debug "write-sexp!-read-sexp!: %s" recv)
      (should (equal recv test-sexp)))))

(provide' process-sockets-test)
;; process-sockets-test.el ends here
