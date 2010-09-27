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
	 (:|time| (:|beats| "2") (:|beat-type| "4"))
	 (:|staves| "2")
	 ((:|clef| :|number| "1") (:|sign| "G"))
	 ((:|clef| :|number| "2") (:|sign| "F")))

       ,(note (pitch 'd -1 4) 1 nil 'flat
	      :beam-begin '(1 2))
       ,(note (pitch 'd 0 4) 1 nil 'mxml::natural
	      :beam-continue '(1 2))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-continue '(1 2))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-end '(1 2))

       ,(note (pitch 'd -1 5) 1 nil 'flat
	      :beam-begin '(1 2)
	      :time-modification (time-modification 5 4 'mxml::16th))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-continue '(1 2)
	      :time-modification (time-modification 5 4 'mxml::16th))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-continue '(1 2)
	      :time-modification (time-modification 5 4 'mxml::16th))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-continue '(1 2)
	      :time-modification (time-modification 5 4 'mxml::16th))
       ,(note (pitch 'd -1 4) 1 nil nil
	      :beam-end '(1 2)
	      :time-modification (time-modification 5 4 'mxml::16th))

       (:|barline| (:|bar-style| "light-heavy")))))
   :stream out))
