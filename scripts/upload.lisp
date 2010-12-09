;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; This file is part of MusicXML-PWGL.

;;; Copyright (c) 2010, Kilian Sprotte. All rights reserved.

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

(require :asdf)
(require :sb-posix)

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(require :alexandria)
(require :cl-fad)
(require :drakma)
(require :cl-ppcre)

(defpackage #:upload
  (:use #:cl #:alexandria))

(in-package #:upload)

(defvar *tarball*)
(defvar *zip*)

(let ((tarballs (directory "*.tgz"))
      (zips (directory "*.zip")))
  (assert (= 1 (length tarballs)))
  (assert (= 1 (length zips)))
  (setq *tarball* (enough-namestring (first tarballs))
        *zip* (enough-namestring (first zips))))

(defvar *version*
  (first (ppcre:all-matches-as-strings "\\d+(\\.\\d+)+" (file-namestring *tarball*))))

(sb-ext:run-program "/usr/bin/scp" (list *tarball* "lisp.homelinux.net:www/musicxml-pwgl/tarballs"))
(sb-ext:run-program "/usr/bin/scp" (list *zip* "lisp.homelinux.net:www/musicxml-pwgl/tarballs"))
(sb-ext:run-program "/usr/bin/scp" (list "CHANGES" "lisp.homelinux.net:www/musicxml-pwgl"))

(drakma:http-request "http://lisp.homelinux.net/musicxml-pwgl/set-version"
                     :parameters `(("version" . ,*version*))
                     :method :post)
