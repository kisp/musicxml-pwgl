;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(require :asdf)
(require :sb-posix)

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(require :alexandria)

(defpackage #:dist
  (:use #:cl #:alexandria))

(in-package #:dist)

(defvar *tmp-dir* (ensure-directories-exist
                   (format nil "/tmp/musicxml-~A/" (sb-posix:getpid))))

(push (lambda () (sb-ext:delete-directory *tmp-dir* :recursive t))
      sb-ext:*exit-hooks*)

(defvar *version* (asdf:component-version (asdf:find-system :musicxml)))
(defvar *name* "musicxml-pwgl")
(defvar *name-version* (format nil "~A-~A" *name* *version*))
(defvar *dir* (ensure-directories-exist
               (merge-pathnames
                (make-pathname :directory `(:relative ,*name-version*))
                *tmp-dir*)))

(defun src (path)
  (merge-pathnames path))

(defun dist (path)
  (merge-pathnames path *dir*))

(defun copy-src-dir (path)
  (copy-file (src path) (dist path)))

;;; copy files
(copy-src-dir "pprint-xml.lisp")
(copy-src-dir "musicxml.lisp")
(copy-src-dir "enp2musicxml.lisp")

(copy-file "dist/musicxml-pwgl.noasd" (dist "musicxml-pwgl.asd"))
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
                (enough-namestring *dir* *tmp-dir*))))
    (sb-posix:chdir cwd)))

(copy-file (merge-pathnames (format nil "~A.tgz" *name-version*) *tmp-dir*)
           (merge-pathnames (format nil "~A.tgz" *name-version*)))
