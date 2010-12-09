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
       (:|attributes| (:|divisions| "180")
         (:|key| (:|fifths| "0"))
         (:|time| (:|beats| "2") (:|beat-type| "4"))
         (:|staves| "2")
         ((:|clef| :|number| "1") (:|sign| "G"))
         ((:|clef| :|number| "2") (:|sign| "F")))

       ,(note (pitch 'c 0 4) 60 'eighth nil
              :beam-begin '(1)
              :time-modification (time-modification 3 2 'eighth)
              :notations (list (tuplet 'start 1 3 'eighth 2 'eighth :bracket 'yes)))

       ,(note (pitch 'c 0 4) 60 '32nd nil
              :beam-continue '(1)
              :time-modification (time-modification 15 8 nil)
              :notations (list (tuplet 'start 2 5 '32nd 4 '32nd)))
       ,(note (pitch 'd 0 4) 60 '32nd nil
              :beam-continue '(1)
              :time-modification (time-modification 15 8 nil))
       ,(note (pitch 'e 0 4) 60 '32nd nil
              :beam-continue '(1)
              :time-modification (time-modification 15 8 nil))
       ,(note (pitch 'f 0 4) 60 '32nd nil
              :beam-continue '(1)
              :time-modification (time-modification 15 8 nil))
       ,(note (pitch 'g 0 4) 60 '32nd nil
              :beam-continue '(1)
              :time-modification (time-modification 15 8 nil)
              :notations (list (tuplet 'stop 2 5 '32nd 4 '32nd)))

       ,(note (pitch 'e 0 4) 60 'eighth nil
              :beam-end '(1)
              :time-modification (time-modification 3 2 'eighth)
              :notations (list (tuplet 'stop 1 3 'eighth 2 'eighth :bracket 'yes)))

       ,(note (pitch 'd -1 5) 36 '16th 'flat
              :beam-begin '(1 2)
              :time-modification (time-modification 5 4 '16th)
              :notations (list (tuplet 'start 1 5 '16TH 4 '16TH)))
       ,(note (pitch 'd -1 4) 36 '16th 'flat
              :beam-continue '(1 2)
              :time-modification (time-modification 5 4 '16th))
       ,(note (pitch 'd -1 4) 36 '16th nil
              :beam-continue '(1 2)
              :time-modification (time-modification 5 4 '16th))
       ,(note (pitch 'd -1 4) 36 '16th nil
              :beam-continue '(1 2)
              :time-modification (time-modification 5 4 '16th))
       ,(note (pitch 'd -1 4) 36 '16th nil
              :beam-end '(1 2)
              :time-modification (time-modification 5 4 '16th)
              :notations (list (tuplet 'stop 1 5 '16TH 4 '16TH)))
       )

      ))
   :stream out))
