;;; Currently, this function is intended to only be called by the
;;; build-docs.sh script.

(defun build-docs ()
  (with-temp-file "./process-sockets.info"
    (insert-file-contents-literally "./process-sockets-temp.info")
    (goto-char 1)
    (set-mark (point))
    (search-forward "1 Process Sockets\n")
    (forward-line -1)
    (backward-char)
    (delete-region 1 (point))
    (insert-file-contents-literally "./prologue.txt")))

;;; build-docs.el ends here
