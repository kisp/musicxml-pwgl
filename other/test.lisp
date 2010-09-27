;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:foo
  (:use #:cl #:mxml))

(in-package #:foo)

(with-open-file (out "/tmp/ks.xml" :direction :output :if-exists :supersede)
  (print-musicxml
   `((:|score-partwise| :|version| "2.0")
     (:|identification|
       (:|encoding| (:|encoding-date| "2010-09-26")
	 (:|software| "FOMUS 0.1.12-alpha-rc5")))
     (:|part-list|
       ((:|score-part| :|id| "P1") :|part-name| :|part-abbreviation|))
     ((:|part| :|id| "P1")
      ((:|measure| :|number| "1")
       (:|attributes| (:|divisions| "4")
	 (:|key| (:|fifths| "0"))
	 (:|time| (:|beats| "1") (:|beat-type| "4"))
	 (:|staves| "2")
	 ((:|clef| :|number| "1") (:|sign| "G"))
	 ((:|clef| :|number| "2") (:|sign| "F")))
       
       ,(note (pitch 'd -1 4) 1 nil 'flat)
       ,(note (pitch 'd 0 4) 1 nil nil)
       ,(note (pitch 'd -1 4) 1 nil nil)
       ,(note (pitch 'd -1 4) 1 nil nil)
       
       (:|barline| (:|bar-style| "light-heavy")))))
   :stream out))
