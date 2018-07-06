[1mdiff --git a/process-sockets.el b/process-sockets.el[m
[1mindex 42f0512..d2468f2 100755[m
[1m--- a/process-sockets.el[m
[1m+++ b/process-sockets.el[m
[36m@@ -76,11 +76,11 @@[m
 	 (read-sexp (lambda ()[m
 		      (read input-stream)))[m
 	 (write-sexp (lambda (sexp)[m
[31m-		       (fc output-stream (format "%s\n" sexp))))[m
[32m+[m		[32m       (funcall output-stream (format "%s\n" sexp))))[m
 	 (read-char (lambda ()[m
[31m-		      (fc input-stream)))[m
[32m+[m		[32m      (funcall input-stream)))[m
 	 (write-string (lambda (str)[m
[31m-			 (fc output-stream str)))[m
[32m+[m			[32m (funcall output-stream str)))[m
 	 (drain-input (lambda ()[m
 			(bs-drain-input bis))))[m
     ;; Write output from the process to the socket's input[m
[36m@@ -118,28 +118,28 @@[m
 	 (error "Invalid arguments"))))))[m
 [m
 (defun ps-input-stream (ps)[m
[31m-  (ps 'input-stream))[m
[32m+[m[32m  (funcall ps 'input-stream))[m
 [m
 (defun ps-output-stream (ps)[m
[31m-  (ps 'output-stream))[m
[32m+[m[32m  (funcall ps 'output-stream))[m
 [m
 (defun ps-read-char (ps)[m
[31m-  (ps 'read-char))[m
[32m+[m[32m  (funcall ps 'read-char))[m
 [m
 (defun ps-read-sexp (ps)[m
[31m-  (ps 'read-sexp))[m
[32m+[m[32m  (funcall ps 'read-sexp))[m
 [m
 (defun ps-write-string (ps string)[m
[31m-  (ps 'write-string string))[m
[32m+[m[32m  (funcall ps 'write-string string))[m
 [m
 (defun ps-write-sexp (ps sexp)[m
[31m-  (ps 'write-sexp sexp))[m
[32m+[m[32m  (funcall ps 'write-sexp sexp))[m
 [m
 (defun ps-drain-input (ps)[m
[31m-  (ps 'drain-input))[m
[32m+[m[32m  (funcall ps 'drain-input))[m
 [m
 (defun ps-close (ps)[m
[31m-  (ps 'close))[m
[32m+[m[32m  (funcall ps 'close))[m
 [m
 (provide 'process-sockets)[m
 ;;; process-sockets.el ends here[m
