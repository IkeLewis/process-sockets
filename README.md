Process Sockets
===============

provides sockets for performing process I/O in Emacs.  The socket
implementation is based on pipes (see below).

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

    ELISP> (defvar my-sock (ps-make-process-socket my-proc))
    my-sock

    ELISP> (ps-write! my-sock "PS1=\"\"; echo Hello world!\n")
    nil

    ELISP> (ps-flush! my-sock)

    ELISP> (ps-read-all! my-sock)
    "Hello world!\n"

    ELISP>
    ```

Process Sockets API
-------------------

 Function         | Short Description
------------------|------------------------------------------------------
 ps-make-socket   | Make a new process socket.
 ps-input-stream  | Return the socket's buffered input stream.
 ps-output-stream | Return the socket's buffered output stream.
 ps-write!        | Write a character or string to the socket.
 ps-write-ln!     | Write a line to the socket.
 ps-write-sexp!   | Write an sexp to the socket.
 ps-read!         | Read a character from the socket.
 ps-read-ln!      | Read a line from the socket.
 ps-read-sexp!    | Read an sexp from the socket.
 ps-read-all!     | Read all currently available characters into a string.
 ps-close!        | Close the socket.

Traditional sockets usually have a connect method.  In this
implementation, process sockets are (currently) "connected" at the
time they are created.

Pipe API
--------

Nearly all the functions (read, print, etc) that work with streams
should work with a pipe's I/O streams, as well.

The default buffer size is 65,536 bytes; that value was selected to
agree with the default Linux pipe capacity and should rarely need to
be changed.

 Function           | Short Description
 -------------------|------------------------------------------------
 pipe-make-pipe     | Make a new pipe.
 pipe-input-stream  | Return the pipe's input stream.
 pipe-output-stream | Return the pipe's output stream.
 pipe-write!        | Write a character or string to the pipe.
 pipe-write-ln!     | Write a line to the pipe.
 pipe-write-sexp!   | Write an sexp to the pipe.
 pipe-read!         | Read/unread a character from the pipe.
 pipe-read-ln!      | Read a line from the pipe.
 pipe-read-sexp!    | Read an sexp from the pipe.
 pipe-read-all!     | Read all currently available characters into a string.

Unit testing
------------

TODO

Benchmarking
------------

TODO

Pull Requests!
--------------

Pull requests are greatly appreciated.
