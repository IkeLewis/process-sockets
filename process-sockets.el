;;; process-sockets.el --- Process sockets for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Version: 1.0.0
;; Keywords: process, socket, buffered, stream

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

(require 'cl)
(require 'buffered-streams)

;;; Customizable Variables

(defvar ps-stream-buf-size (* 64 1024) "The default buffer size for the socket's streams.")


(provide 'process-sockets)
;;; process-sockets.el ends here
