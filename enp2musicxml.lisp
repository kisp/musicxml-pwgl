;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

(defun enp2musicxml (enp)
  `((:|score-partwise| #+nil :|version| #+nil "2.0")
    (:|identification|
      (:|encoding| (:|encoding-date| "2010-10-12")
	(:|software| "FOMUS v0.2.12")))
    (:|part-list|
      ((:|score-part| :|id| "P1")
       (:|part-name| "Violin")))
    ((:|part| :|id| "P1")
     ((:|measure| :|number| "1")
      (:|attributes|
	(:|divisions| "1")
	(:|time| (:|beats| "4") (:|beat-type| "4"))
	(:|clef| (:|sign| "G") (:|line| "2")))
      ,(note (pitch 'c 0 4) 1 'quarter nil)
      ,(note (pitch 'c 0 4) 1 'quarter nil)
      ,(note (pitch 'c 0 4) 1 'quarter nil)
      ,(note (pitch 'c 0 4) 1 'quarter nil)
      (:|barline| (:|bar-style| "light-heavy"))))))