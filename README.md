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

|------------------+-------------------------------------------------------|
| ps-make-socket   | Make a new process socket.                            |
|------------------+-------------------------------------------------------|
| ps-close         | Close the socket.                                     |
|------------------+-------------------------------------------------------|
| ps-input-stream  | Return the socket's buffered input stream.            |
|------------------+-------------------------------------------------------|
| ps-output-stream | Return the socket's buffered output stream.           |
|------------------+-------------------------------------------------------|
| ps-read-sexp     | Read an sexp from the socket.                         |
|------------------+-------------------------------------------------------|
| ps-write-sexp    | Write an sexp to the socket.                          |
|------------------+-------------------------------------------------------|
| ps-read-char     | Read a character from the socket.                     |
|------------------+-------------------------------------------------------|
| ps-write-string  | Write a string to the socket.                         |
|------------------+-------------------------------------------------------|
| ps-drain-input   | Read all available characters from the socket's input |
|                  | stream and return the result as a string.             |
|------------------+-------------------------------------------------------|
