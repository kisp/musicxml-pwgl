;;; This file is part of MusicXML-PWGL.

;;; Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(require "ASDF")

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload "s-xml")
(ql:quickload "cxml")
(ql:quickload "myam")
(ql:quickload "sqlite-orm")
(ql:quickload "drakma")

;; (require 'arnesi)
;; (asdf:oos 'arnesi:clean-op :musicxml-pwgl)

(pushnew '*default-pathname-defaults* asdf:*central-registry*)

(require 'musicxml-pwgl)

(defun main ()
  (let ((input (if (second sb-ext:*posix-argv*)
                   (with-open-file (in (second sb-ext:*posix-argv*))
                     (read in nil))
                   (read *standard-input* nil))))
    (when input
      (musicxml-pwgl.musicxml:print-musicxml
       (musicxml-pwgl.enp2musicxml:enp2musicxml input)
       :stream *standard-output*)
      (terpri *standard-output*))))

(sb-ext:save-lisp-and-die "enp2mxml"
                          :toplevel 'main
                          :executable t)
