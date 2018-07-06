Process Sockets
===============

If you've ever had to perform process I/O in Emacs, you may have found
it to be quite unwieldy; I often found myself wanting just a simple
socket with buffered streams.  Now that Emacs 26 supports multiple
threads, such sockets are easier to implement.

Pull Requests!
--------------

Pull requests are greatly appreciated.

Process Sockets API
-------------------

 Function         | Short Description
------------------|------------------------------------------------------
 ps-make-socket   | Make a new process socket.
 ps-close         | Close the socket.
 ps-input-stream  | Return the socket's buffered input stream.
 ps-output-stream | Return the socket's buffered output stream.
 ps-read-sexp     | Read an sexp from the socket.
 ps-write-sexp    | Write an sexp to the socket.
 ps-read-char     | Read a character from the socket.
 ps-write-string  | Write a string to the socket.
 ps-drain-input   | Return all available characters from the socket's input stream as a string.

Buffered Streams API
--------------------

 Function                | Short Description
 ------------------------|------------------------------------------------
 bs-make-buffered-stream | Make a new buffered stream.
 bs-read                 | Read/unread a character from the stream.
 bs-write                | Write a string to the stream.
 bs-drain-input          | Return all available characters from the stream's buffer as a string.

Example
-------

Open up an ELISP REPL in Emacs, e.g. via M-x ielm.

```el
ELISP> (defvar my-proc (start-process "bash-proc1" (get-buffer-create "bash-proc1") "/bin/bash"))
my-proc

ELISP> (defvar my-sock (ps-make-socket my-proc))
my-sock

ELISP> (ps-write-string my-sock "PS1=\"\"; echo Hello world!\n")
nil

ELISP> (ps-drain-input my-sock)
"Hello world!\n"

ELISP>
```
