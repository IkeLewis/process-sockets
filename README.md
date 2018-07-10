Process Sockets
===============

If you've ever had to perform process I/O in Emacs, you may have found
it to be quite unwieldy; I often found myself wanting just a simple
socket with buffered streams.  Now that Emacs 26 supports multiple
threads, such sockets are easier to implement.

Installation
------------

Once the API becomes stable, a recipe will likely be added to MELPA.
In that case, MELPA will be the preferred way to install.

For now, Cask may be used to manually build a bleeding edge version of
the package that can be locally installed by Emacs.

  1. Run cask package in a terminal to build the package.
  2. Run package-install-file from Emacs.

Alternatively, old school manual installation may be used.

  1.  In your Emacs init file:

      a. Add the process-sockets directory to your load path.<br>
      b. Include a line containing (require 'procces-sockets)

  2. Install the doc/process-sockets.info file.  See [https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html)

Quickstart :rocket:
----------

1.  Install via MELPA (or manually).
2.  Open up a REPL in Emacs via M-x ielm and try it out:

    ```el
    ELISP> (require 'process-sockets)
    process-sockets

    ELISP> (defvar my-proc (start-process "bash-proc1"
                                          (get-buffer-create "bash-proc1")
                                          "/bin/bash"))
    my-proc

    ELISP> (defvar my-sock (ps-make-socket my-proc))
    my-sock

    ELISP> (ps-write-string my-sock "PS1=\"\"; echo Hello world!\n")
    nil

    ELISP> (ps-drain-input my-sock)
    "Hello world!\n"

    ELISP>
    ```

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

Pull Requests!
--------------

Pull requests are greatly appreciated.
