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
(require :cl-ppcre)

(defpackage #:bump-version
  (:use #:cl #:alexandria))

(in-package #:bump-version)

(defun parse-version (string)
  (let ((pos (position #\. string)))
    (if pos
        (cons (parse-integer (subseq string 0 pos))
              (parse-version (subseq string (1+ pos))))
        (list (parse-integer string)))))

(defvar *version* (parse-version (asdf:component-version (asdf:find-system :musicxml-pwgl))))

(incf (car (last *version*)))

(defvar *new-version* (format nil "~{~A~^.~}" *version*))

(defvar *scanner* (ppcre:create-scanner (ppcre:quote-meta-chars (asdf:component-version (asdf:find-system :musicxml-pwgl)))))

(defun file-replace (path scanner string)
  (let ((content (read-file-into-string path)))
    (setq content
          (ppcre:regex-replace-all scanner content string))
    (write-string-into-file content path
                            :if-exists :supersede)))

(defun update-version (path)
  (file-replace path *scanner* *new-version*))

(update-version "musicxml-pwgl.asd")
(update-version "dist/musicxml-pwgl.noasd")
