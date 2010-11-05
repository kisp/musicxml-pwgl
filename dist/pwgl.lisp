;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:musicxml-pwgl
  (:use #:cl #:ompw)
  (:export #:export-musicxml))

(in-package #:musicxml-pwgl)

(define-menu musicxml-pwgl :print-name "MusicXML-PWGL")
(in-menu musicxml-pwgl)

(defvar *xml-path* (user-homedir-pathname))

(defun prompt-for-xml-path (message)
  (let ((path (capi:prompt-for-file
               message
               :pathname *xml-path*
               :filters '("xml" "*.xml") :operation :save)))
    (when path
      (setq *xml-path* path))))

(define-box export-musicxml (score &optional path)
  (let ((enp (ccl::enp-score-notation score :exclude nil)))
    (unless path
      (setq path (prompt-for-xml-path "Export MusicXML to:"))
      (unless path
        (return-from export-musicxml)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (mxml:print-musicxml (e2m:enp2musicxml enp) :stream out))))

(install-menu musicxml-pwgl)
