;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

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

(require :asdf)
(require :sb-posix)

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(require :alexandria)
(require :cl-fad)

(defpackage #:dist
  (:use #:cl #:alexandria))

(in-package #:dist)

(mapc #'delete-file (directory "*.tgz"))
(mapc #'delete-file (directory "*.zip"))

(defvar *tmp-dir* (ensure-directories-exist
                   (format nil "/tmp/musicxml-~A/" (sb-posix:getpid))))

(push (lambda () (cl-fad:delete-directory-and-files *tmp-dir*))
      sb-ext:*exit-hooks*)

(defvar *version* (asdf:component-version (asdf:find-system :musicxml-pwgl)))
(defvar *name* "musicxml-pwgl")
(defvar *name-version* (format nil "~A-~A" *name* *version*))
(defvar *dir* (ensure-directories-exist
               (merge-pathnames
                (make-pathname :directory `(:relative ,*name*))
                *tmp-dir*)))

(defun src (path)
  (merge-pathnames path))

(defun dist (path)
  (merge-pathnames path *dir*))

(defun copy-src-dir (path)
  (copy-file (src path) (dist path)))

;;; copy files
(copy-src-dir "packages.lisp")
(copy-src-dir "mapcar-state.lisp")
(copy-src-dir "pprint-xml.lisp")
(copy-src-dir "musicxml.lisp")
(copy-src-dir "enp2musicxml.lisp")

(copy-file "dist/musicxml-pwgl.noasd" (dist "musicxml-pwgl.asd"))
(copy-file "dist/simple-http.lisp" (dist "simple-http.lisp"))
(copy-file "dist/pwgl.lisp" (dist "pwgl.lisp"))

(ensure-directories-exist (dist "tutorial/"))
(copy-file "dist/tutorial/intro.pwgl" (dist "tutorial/intro.pwgl"))

;;; create tarball
(let ((cwd (sb-posix:getcwd)))
  (unwind-protect
       (progn
         (sb-posix:chdir *tmp-dir*)
         (sb-ext:run-program
          "/bin/tar"
          (list "cfz" (format nil "~A.tgz" *name-version*)
                (enough-namestring *dir* *tmp-dir*)))
         (sb-ext:run-program
          "/usr/bin/zip"
          (list "-r" (format nil "~A.zip" *name-version*)
                (enough-namestring *dir* *tmp-dir*))))
    (sb-posix:chdir cwd)))

(copy-file (merge-pathnames (format nil "~A.tgz" *name-version*) *tmp-dir*)
           (merge-pathnames (format nil "~A.tgz" *name-version*)))
(copy-file (merge-pathnames (format nil "~A.zip" *name-version*) *tmp-dir*)
           (merge-pathnames (format nil "~A.zip" *name-version*)))
