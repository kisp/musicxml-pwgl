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

(defpackage #:musicxml-pwgl.test
  (:use #:cl #:myam #:alexandria
        #:musicxml-pwgl.musicxml
        #:musicxml-pwgl.test-db
        #:musicxml-pwgl.enp2musicxml
        #:musicxml-pwgl.xml-filter)
  (:export #:run-tests)
  (:import-from #:musicxml-pwgl.enp2musicxml
                #:%chordp
                #:%divp
                #:*accidental-store*
                #:abs-dur-name
                #:append-list-plist
                #:chord-dur
                #:convert-note
                #:decode-midi
                #:div-dur
                #:div-items
                #:enp-note-accidental
                #:enp-parts
                #:info-abs-dur
                #:info-beaming
                #:info-chord
                #:info-ending-tuplets
                #:info-pointers
                #:info-starting-tuplets
                #:info-tuplet-ratios
                #:list2ratio
                #:make-accidental-store
                #:measure-infos
                #:measure-quarter-division
                #:register-accidental
                #:split-list-plist
                #:split-plist-list
                #:tuplet-eql
                #:tuplet-key
                #:tuplet-ratio
                #:tuplet-tuplet-ratio
                )
  (:import-from #:musicxml-pwgl.mapcar-state
                #:make-mapcar-state))

(in-package #:musicxml-pwgl.test)

(defsuite :musicxml-pwgl)
(defsuite :musicxml-pwgl.base :in :musicxml-pwgl)

(in-suite :musicxml-pwgl.base)

(defun files-eql-p (a b)
  ;; @todo #3:30m needs better exit code checking of diff
  (let ((process (sb-ext:run-program
                  "/usr/bin/diff"
                  (list "-q" (namestring a) (namestring b)))))
    (zerop (sb-ext:process-exit-code process))))

(defun diff (a b)
  ;; @todo #3:30m needs better exit code checking of diff 2
  (with-output-to-string (out)
    (sb-ext:run-program
     "/usr/bin/diff"
     (list "-u" (namestring a) (namestring b)) :output out)))

(defun canonicalise (path new-path)
  ;; @todo #3:30m needs better exit code checking of Canonicalise
  (sb-ext:run-program
   "/bin/bash"
   (list "-c"
         (format nil "xmllint --format <(xmllint --exc-c14n '~A') >'~A'"
                 (namestring path) (namestring new-path)))))

(defun string-remove-first-n-lines (n string)
  (if (zerop n)
      string
      (let ((pos (position #\newline string)))
        (assert pos)
        (string-remove-first-n-lines
         (1- n)
         (subseq string (1+ pos))))))

;;; cxml ext
(in-package #:cxml)

(defclass whitespace-trimmer (sax-proxy)
  ())

(defun make-whitespace-trimmer (chained-handler)
  (make-instance 'whitespace-trimmer
                 :chained-handler chained-handler))

(defmethod sax:characters ((handler whitespace-trimmer) data)
  (call-next-method handler (string-trim '(#\space #\newline #\tab #\page) data)))

;; (dom:map-document
;;  (cxml:make-string-sink)
;;  (cxml:parse "<foo>
;;                    a b c d
;;                 </foo>" (make-whitespace-trimmer (cxml-dom:make-dom-builder))))

(defun trim-xml-file (src-path out-path)
  (with-open-file (out out-path
                       :direction :output
                       :if-exists :supersede)
    (dom:map-document
     (cxml:make-character-stream-sink out)
     (cxml:parse-file src-path
                      (cxml::make-whitespace-trimmer (rune-dom:make-dom-builder))))))

;;; tests
(in-package #:musicxml-pwgl.test)

(deftest s-xml-read-write
  (dolist (xml (directory "any-xmls/*.xml"))
    (with-open-file (out "/tmp/foo.xml"
                         :direction :output
                         :if-exists :supersede)
      (s-xml:print-xml (parse-xml-file-via-cxml xml) :stream out))
    (canonicalise "/tmp/foo.xml" "/tmp/fooc.xml")
    (canonicalise xml "/tmp/origc.xml")
    (is (files-eql-p "/tmp/origc.xml" "/tmp/fooc.xml")
        "~A failed~%~A"
        (file-namestring xml)
        (diff "/tmp/origc.xml" "/tmp/fooc.xml"))))

(defun parse-xml-file-via-cxml (path)
  (xmls2lxml (cxml:parse-file path (cxml-xmls:make-xmls-builder))))

(defun xmls2lxml (node)
  (cond ((and (consp node)
              (null (cxml-xmls:node-attrs node))
              (null (cxml-xmls:node-children node)))
         (intern (cxml-xmls:node-name node) "KEYWORD"))
        ((consp node)
         `((,(intern (cxml-xmls:node-name node) "KEYWORD")
            ,@(mapcan (lambda (pair)
                        (list (intern (first pair) "KEYWORD")
                              (second pair)))
                      (reverse (cxml-xmls:node-attrs node))))
           ,@(mapcar #'xmls2lxml (cxml-xmls:node-children node))))
        (t
         node)))

(deftest ppxml-read-write
  (dolist (xml (directory "any-xmls/*.xml"))
    (with-open-file (out "/tmp/foo.xml"
                         :direction :output
                         :if-exists :supersede)
      (musicxml-pwgl.pprint-xml:pprint-xml (parse-xml-file-via-cxml xml) :stream out))
    (canonicalise "/tmp/foo.xml" "/tmp/fooc.xml")
    (canonicalise xml "/tmp/origc.xml")
    (is (files-eql-p "/tmp/origc.xml" "/tmp/fooc.xml")
        "~A failed~%~S~%~A"
        (file-namestring xml)
        (s-xml:parse-xml-file xml)
        (diff "/tmp/origc.xml" "/tmp/fooc.xml"))))

(deftest lxml
  (dolist (xml (directory "fomus-xmls/*.xml"))
    (let ((lxml (s-xml:parse-xml-file xml)))
      (is (equal lxml
                 (to-lxml (from-lxml lxml)))))))

(deftest note
  (dolist (lxml '((:|note| :|rest| (:|duration| "2"))
                  (:|note| :|rest| (:|duration| "1"))
                  (:|note| :|chord| :|rest| (:|duration| "1"))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1"))
                  (:|note|
                   :|chord|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1"))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   (:|staff| "1"))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   (:|accidental| "flat"))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   (:|type| "quarter")
                   (:|accidental| "flat"))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   ((:|tie| :|type| "start"))
                   (:|type| "quarter")
                   (:|accidental| "flat")
                   (:|notations|
                    ((:|tied| :|type| "start"))))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   (:|type| "quarter")
                   (:|dot|))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   (:|type| "eighth")
                   (:|dot|)
                   (:|dot|))
                  (:|note|
                   (:|pitch| (:|step| "C") (:|octave| "4"))
                   (:|duration| "1")
                   ((:|tie| :|type| "stop"))
                   ((:|tie| :|type| "start"))
                   (:|notations|
                    ((:|tied| :|type| "stop"))
                    ((:|tied| :|type| "start"))))))
    (is (equal lxml (to-lxml (from-lxml lxml))))
    (is (equal lxml (to-lxml
                     (eval (make-constructor-form (from-lxml lxml))))))))

(deftest time-modification
  (dolist (lxml '((:|time-modification| (:|actual-notes| "5") (:|normal-notes| "4")
                   (:|normal-type| "quarter"))))
    (is (equal lxml (to-lxml (from-lxml lxml))))
    (is (equal lxml (to-lxml
                     (eval (make-constructor-form (from-lxml lxml))))))))

(deftest attributes
  (is (null (to-lxml (attributes))))
  (dolist (lxml '((:|attributes|
                   (:|divisions| "1")
                   (:|time|
                    (:|beats| "5")
                    (:|beat-type| "4"))
                   (:|clef| (:|sign| "G") (:|line| "2")))
                  (:|attributes|
                   (:|divisions| "1"))))
    (is (equal lxml (to-lxml (from-lxml lxml))))
    (is (equal lxml (to-lxml
                     (eval (make-constructor-form (from-lxml lxml))))))))

(deftest tuplet
  (dolist (lxml '(((:|tuplet| :|type| "start" :|number| "1")
                   (:|tuplet-actual| (:|tuplet-number| "5")))
                  ((:|tuplet| :|type| "stop" :|number| "1"))
                  ((:|tuplet| :|type| "start" :|number| "1")
                   (:|tuplet-actual| (:|tuplet-number| "5") (:|tuplet-type| "16th"))
                   (:|tuplet-normal| (:|tuplet-number| "4") (:|tuplet-type| "16th")))
                  ((:|tuplet| :|type| "stop" :|number| "1")
                   (:|tuplet-actual| (:|tuplet-number| "5") (:|tuplet-type| "16th"))
                   (:|tuplet-normal| (:|tuplet-number| "4") (:|tuplet-type| "16th")))
                  ((:|tuplet| :|type| "stop" :|number| "1")
                   (:|tuplet-actual| (:|tuplet-number| "7") (:|tuplet-type| "16th"))
                   (:|tuplet-normal| (:|tuplet-number| "4") (:|tuplet-type| "16th")))
                  ((:|tuplet| :|type| "start" :|number| "3")
                   (:|tuplet-actual| (:|tuplet-number| "3") (:|tuplet-type| "quarter"))
                   (:|tuplet-normal| (:|tuplet-number| "2") (:|tuplet-type| "quarter")))))
    (is (equal lxml (to-lxml (from-lxml lxml))))
    (is (equal lxml (to-lxml
                     (eval (make-constructor-form (from-lxml lxml))))))))

(defmacro with-test-case-env (test-case &body body)
  `(let ((alist (plist-alist (env ,test-case))))
     (progv
         (mapcar #'car alist)
         (mapcar #'cdr alist)
       ,@body)))

(defun check-test-db-test-case (test-case &optional filtered-elements)
  (with-open-file (out "/tmp/res.xml"
                       :direction :output
                       :if-exists :supersede)
    (write-line "<?xml version='1.0' encoding='UTF-8' ?>" out)
    (print-musicxml (with-test-case-env test-case (enp2musicxml (enp test-case)))
                    :stream out :no-header t))
  (when filtered-elements
    (filter-file "/tmp/res.xml" "/tmp/res.xml" filtered-elements))
  (canonicalise "/tmp/res.xml" "/tmp/resc.xml")
  (alexandria:write-string-into-file
   (with-output-to-string (out)
     (write-string (string-remove-first-n-lines 3 (musicxml test-case))
                   out))
   "/tmp/exp-o.xml" :if-exists :supersede)
  (cxml::trim-xml-file "/tmp/exp-o.xml" "/tmp/exp.xml")
  (when filtered-elements
    (filter-file "/tmp/exp.xml" "/tmp/exp.xml" filtered-elements))
  (canonicalise "/tmp/exp.xml" "/tmp/expc.xml")
  (files-eql-p "/tmp/resc.xml" "/tmp/expc.xml"))

(macrolet ((frob ()
             (assert (list-test-cases))
             (cons 'progn
                   (mapcar
                    (lambda (tc)
                      `(frob-tc
                        ,(sqlite-orm:store-object-id tc)
                        ,(intern (substitute
                                  #\_ #\space
                                  (string-upcase (format nil "test-db-~A-~A"
                                                         (sqlite-orm:store-object-id tc) (name tc)))))))
                    (list-test-cases))))
           (frob-tc (id name)
             `(deftest ,name
                (let* ((id ,id)
                       (test-case (find id (list-test-cases) :key #'sqlite-orm:store-object-id)))
                  (ecase (status test-case)
                    (:skip (skip "~A -- ~A" (name test-case) (description test-case)))
                    (:run
                     (is-true (check-test-db-test-case test-case '(:|identification|))
                              "\"~A\" failed~%~A"
                              (name test-case)
                              (diff "/tmp/resc.xml" "/tmp/expc.xml"))))))))
  (frob))

(deftest pprint-xml-nil
  (is
   (string= "
<huhu>123<zzz></zzz></huhu>"
            (with-output-to-string (out)
              (musicxml-pwgl.pprint-xml:pprint-xml '(:|huhu| "123" (:|zzz| nil)) :stream out)))))

(defun gen-keyword ()
  (lambda ()
    (intern (string-upcase (funcall (gen-string :elements (gen-character :alphanumericp t :code-limit 120))))
            "KEYWORD")))

(defun gen-plist (&key (length (gen-integer :min 0 :max 10))
                    (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (loop with keyword = (gen-keyword)
          repeat (funcall length)
          collect (funcall keyword)
          collect (funcall elements))))

(deftest split-list-plist
  (for-all ((list (gen-list))
            (plist (gen-plist)))
    (multiple-value-bind (new-list new-plist)
        (split-list-plist
         (append-list-plist list plist))
      (is (equal list new-list))
      (is (equal plist new-plist)))))

(deftest chordp
  (is-true (%chordp '(1 :START-TIME 4.0 :NOTES (60))))
  (is-false (%chordp '(1 ((1 :START-TIME 4.0 :NOTES (60)))))))

(deftest div-dur-div-items
  (is (= 10 (div-dur '(10 ((1 :START-TIME 4.0 :NOTES (60)))))))
  (is (equal
       '((1 :START-TIME 4.0 :NOTES (60)))
       (div-items '(10 ((1 :START-TIME 4.0 :NOTES (60)))))))
  (signals error (div-dur '(1 :START-TIME 4.0 :NOTES (60))))
  (signals error (div-items '(1 :START-TIME 4.0 :NOTES (60)))))

(deftest chord-dur
  (is (= 1 (chord-dur '(1 :START-TIME 4.0 :NOTES (60)))))
  (signals error (chord-dur '(10 ((1 :START-TIME 4.0 :NOTES (60)))))))

(deftest tuplet-ratio
  (flet ((check (expected dur div)
           (let ((enp `(,dur ,(loop repeat div collect '(1 :NOTES (60))))))
             (is (equal expected (tuplet-ratio enp))
                 "~S ~S should be ~S" dur div expected))))
    (check '(2 2) 1 2)
    (check '(3 2) 1 3)
    (check '(4 4) 1 4)
    (check '(5 4) 1 5)
    (check '(6 4) 1 6)
    (check '(7 4) 1 7)
    (check '(8 8) 1 8)
    (check '(9 8) 1 9)
    (check '(1 1) 1 1)
    (check '(1 1) 2 1)
    (check '(2 3) 3 2)
    (check '(5 7) 7 5)))

(deftest info-tuplet-ratios
  (is (equal '((1 1) (1 1))
             (info-tuplet-ratios
              (first
               (measure-infos
                '((1 ((1 :NOTES (60))))
                  :TIME-SIGNATURE (1 4)))))))
  (is (equal '((3 2) (1 1))
             (info-tuplet-ratios
              (first
               (measure-infos
                '((1 ((1 :NOTES (60)) (1 :NOTES (60)) (1 :NOTES (60))))
                  :TIME-SIGNATURE (1 4))))))))

(deftest abs-dur-name
  (is (equal '(quarter 1) (multiple-value-list (abs-dur-name 3/8))))
  (is (equal '(quarter 2) (multiple-value-list (abs-dur-name 7/16))))
  (is (equal '(half 3) (multiple-value-list (abs-dur-name 15/16)))))

(defun step2pc (step)
  (ecase step
    (c 0) (d 2) (e 4)
    (f 5) (g 7) (a 9) (b 11)))

(deftest decode-midi
  (for-all ((exp-step (gen-one-element 'c 'd 'e 'f 'g 'a 'b))
            (exp-alter (gen-one-element 0 1 -1))
            (exp-octave (gen-integer :min 0 :max 8)))
    (let ((midi (+ exp-alter (step2pc exp-step)
                   (* (+ exp-octave 1) 12))))
      (multiple-value-bind (step alter octave)
          (decode-midi midi (ecase exp-alter
                              (0 'natural)
                              (1 'sharp)
                              (-1 'flat)))
        (is (eql exp-step step))
        (is (= exp-alter alter))
        (is (= exp-octave octave))))))

(deftest mxml-equal
  (is (mxml-equal (pitch 'c 0 4)
                  (pitch 'c 0 4)))
  (is (not (mxml-equal (pitch 'c 0 4)
                       (pitch 'c 1 4)))))

(deftest note-ties
  (let ((*accidental-store* (make-accidental-store))
        (state (make-mapcar-state :index 1)))
    (labels ((info (chord-dur notes)
               (first (measure-infos `((1 ((,chord-dur :notes ',notes))) :time-signature (1 4)))))
             (convert (chord-dur notes next-chord)
               (let ((info (info chord-dur notes)))
                 (convert-note info 1/4 (info-chord info) next-chord state (first notes)))))
      ;; stop
      (let ((note (convert 1 '(60) nil)))
        (is-false (note-tie-stop note))
        (is-false (note-tie-start note)))
      (let ((note (convert 1.0 '(60) nil)))
        (is-true (note-tie-stop note))
        (is-false (note-tie-start note)))
      (let ((note (convert 1.0 '((60 :attack-p t)) nil)))
        (is-false (note-tie-stop note))
        (is-false (note-tie-start note)))
      ;; start
      (let ((note (convert 1 '(60) '(1.0 :notes (60)))))
        (is-false (note-tie-stop note))
        (is-true (note-tie-start note)))
      (let ((note (convert 1 '(60) '(1.0 :notes ((60 :attack-p t))))))
        (is-false (note-tie-stop note))
        (is-false (note-tie-start note)))
      (let ((note (convert 1 '(60) '(1.0 :notes (61)))))
        (is-false (note-tie-stop note))
        (is-false (note-tie-start note)))
      (let ((note (convert 1 '(60) '(-1 :notes (60)))))
        (is-false (note-tie-stop note))
        (is-false (note-tie-start note))))))

(deftest info-beaming
  (let ((infos (measure-infos
                '((1 ((1 :notes (60)) (1 :notes (60)))) :time-signature (1 4)))))
    (is (equal '(1/8 1/8)
               (mapcar #'info-abs-dur infos)))
    (is (equal '((0 1) (1 0))
               (mapcar #'info-beaming infos))))
  (let ((infos (measure-infos
                '((1 ((2 :notes (60))
                      (1 :notes (60))
                      (1 :notes (60)))) :time-signature (1 4)))))
    (is (equal '(1/8 1/16 1/16)
               (mapcar #'info-abs-dur infos)))
    (is (equal '((0 1) (1 2) (2 0))
               (mapcar #'info-beaming infos)))))

(deftest info-beaming.2
  (let ((infos (measure-infos
                '((1 ((1 :notes (60)) (1 :notes (60))))
                  (1 ((1 :notes (60)) (1 :notes (60)))) :time-signature (2 4)))))
    (is (equal '(1/8 1/8 1/8 1/8)
               (mapcar #'info-abs-dur infos)))
    (is (equal '((0 1) (1 0) (0 1) (1 0))
               (mapcar #'info-beaming infos)))))

(deftest info-beaming.3
  (let ((infos (measure-infos
                '((1 ((1 :NOTES (60)) (1 :START-TIME 0.5 :NOTES (60))))
                  (1
                   ((1 ((1 :START-TIME 0.95 :NOTES (85))) :CLASS :GRACE-BEAT)
                    (1 :START-TIME 1.0 :NOTES (60)) (1 :START-TIME 1.5 :NOTES (60))))
                  :TIME-SIGNATURE (2 4)))))
    (is (equal '(1/8 1/8 NIL 1/8 1/8)
               (mapcar #'info-abs-dur infos)))
    (is (equal '((0 1) (1 0) NIL (0 1) (1 0))
               (mapcar #'info-beaming infos)))))

(deftest enp-note-accidental
  (is (eql 'natural (enp-note-accidental 60)))
  (is (eql 'natural (enp-note-accidental '(60 :enharmonic nil))))
  (is (eql 'sharp (enp-note-accidental 61)))
  (is (eql 'sharp (enp-note-accidental '(61 :enharmonic nil)))))

(deftest accidental-store
  (let ((store (make-accidental-store)))
    (is-true (register-accidental store 'c 4 1))
    (is-false (register-accidental store 'c 4 1))
    (is-false (register-accidental store 'd 4 0))
    (is-false (register-accidental store 'd 4 0))
    (is-true (register-accidental store 'd 4 1))
    (is-false (register-accidental store 'd 4 1))))

(defun get-bad-scores ()
  (let ((*read-eval* nil))
    (with-input-from-string
        (in
         (drakma:http-request "http://lisp.homelinux.net/musicxml-pwgl/bad-scores"))
      (loop for (id enp) = (read in nil (list nil nil))
            while enp collect (list id enp)))))

(deftest bad-scores
  (skip*)
  (dolist (list (get-bad-scores))
    (destructuring-bind (id score)
        list
      (unless (or (member id '(3 16 20 25
                               61 62 81
                               125 129 132 133
                               252 255 257 260 261 262 266 279
                               280))
                  (> id 313))
        (let (e)
          (is (eql t
                   (handler-case
                       (progn (enp2musicxml score) t)
                     (error (c) (setq e c))))
              "failed score ID ~A~%~A" id e))))))

(deftest score-can-have-keyword/value-pairs-in-the-beginning-of-the-form.1
  (is (equalp (enp2musicxml
               '(                       ;without keyword/value pairs
                 #1=((((1 ((1 :notes (65))))
                       :time-signature (1 4) :metronome (4 110)))
                     :instrument nil :staff :treble-staff)))
              (enp2musicxml
               '(:spacing 0.88485d0     ;<- there can be keyword/value
                                        ;pairs here
                 #1#)))))

(deftest score-can-have-keyword/value-pairs-in-the-beginning-of-the-form.2
  (skip*)
  (is (equalp (enp2musicxml
               '(#1=((((1 ((1
                            ((1 :notes (55))
                             (1 :start-time 0.091 :notes (55))
                             (1 :start-time 0.182 :notes (55))
                             (1 :start-time 0.272 :notes (55))
                             (1 :start-time 0.363 :notes (55))
                             (1 :start-time 0.454 :notes (55))))))
                       :time-signature (1 4) :metronome (4 110)))
                     :instrument nil :staff :treble-staff)
                 #2=((((1 ((1
                            ((1 :notes (55))
                             (1 :start-time 0.136 :notes (55))
                             (1 :start-time 0.272 :notes (55))
                             (1 :start-time 0.409 :notes (55))))))
                       :time-signature (1 4) :metronome (4 110)))
                     :instrument nil :staff :treble-staff)))
              (enp2musicxml
               '(:spacing 0.8389395449778877d0
                 #1#
                 #2#)))))

(deftest enp-parts.1
  (is (equal '((1) (2)) (enp-parts '((1) (2)))))
  (is (equal '((1) (2)) (enp-parts '(:foo 1 (1) (2)))))
  (is (equal '((1) (2)) (enp-parts '(:foo 1 :bar 4 (1) (2))))))

(deftest split-plist-list.1
  ;; FIXME: make this one test with MULTIPLE-VALUE-LIST
  (is (equal '(:foo 1) (split-plist-list '(:foo 1))))
  (is (equal '() (split-plist-list '())))
  (is (equal '(:foo 1) (split-plist-list '(:foo 1 7))))
  (is (equal '() (split-plist-list '(:foo)))))

(deftest split-plist-list.2
  (is (equal '() (nth-value 1 (split-plist-list '(:foo 1)))))
  (is (equal '() (nth-value 1 (split-plist-list '()))))
  (is (equal '(7) (nth-value 1 (split-plist-list '(:foo 1 7)))))
  (is (equal '(:foo) (nth-value 1 (split-plist-list '(:foo))))))

(defsuite :musicxml-pwgl.tuplet :in :musicxml-pwgl)

(in-suite :musicxml-pwgl.tuplet)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-measures* nil)

  (defmacro define-test-measure (name enp &optional doc)
    `(progn
       ;; the copy-tree avoids constant coalescing, which is undesired
       ;; for the ENP tree
       (defparameter ,name (copy-tree ,enp) ,doc)
       (push (list ,name ,doc ',name) *test-measures*)))

  (define-test-measure *test-measure1*
      '((1 ((1 :notes (60)))) :time-signature (1 4))
    "a quarter beat")

  (define-test-measure *test-measure2*
      '((1 ((1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        :time-signature (1 4))
    "a triplet")

  (define-test-measure *test-measure3*
      '((1 ((1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        (1 ((1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        :time-signature (2 4))
    "two triplets")

  (define-test-measure *test-measure4*
      '((1 ((1 :notes (60))
            (1 :notes (60))))
        (1 ((1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        :time-signature (2 4))
    "2 quavers, a triplet")

  (define-test-measure *test-measure5*
      '((1 ((1 :notes (60))))
        (1 ((1 :notes (60))
            (1 ((1 :notes (60))
                (1 :notes (60))
                (1 :notes (60))))))
        :time-signature (2 4))
    "inner tuplet")

  (define-test-measure *test-measure6*
      '((1 ((1 :notes (60))
            (1 :notes (60))))
        (1 ((1 ((1 :notes (60))) :class :grace-beat)
            (1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        :time-signature (2 4))
    "2 quavers, a triplet with grace")

  (define-test-measure *test-measure7*
      '((1 ((1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))
            (1 :notes (60))))
        :time-signature (1 4))
    "quintuplet")

  (defmacro domeasures ((m d s) &body body)
    (with-gensyms (x)
      `(dolist (,x *test-measures*)
         (destructuring-bind (,m ,d ,s) ,x
           ,@body))))

  (defun map-measures (fn)
    (let (res)
      (domeasures (m d s)
        (push (funcall fn m d s)
              res))
      (nreverse res))))

(defmacro deftest-measures (name &body body)
  (let ((fn-name (symbolicate name ".FN")))
    `(progn
       (defun ,fn-name (m) ,@body)
       ,@(let ((n 0))
           (map-measures
            (lambda (m d s)
              (declare (ignore m))
              (let ((name* (symbolicate name "."
                                        (princ-to-string (incf n)))))
                `(deftest ,name* ,d (,fn-name ,s)))))))))

(defun number-of-tuples (infos)
  (let ((count 0)
        (hash (make-hash-table)))
    (labels ((visit (x)
               (prog1
                   (not (gethash x hash))
                 (setf (gethash x hash) t))))
      (dolist (info infos count)
        (loop for pointer in (rest (info-pointers info))
              for tuplet-ratio in (info-tuplet-ratios info)
              do (when (/= 1 (list2ratio tuplet-ratio))
                   (when (visit pointer)
                     (incf count))))))))

(deftest tuplet.1
  (let ((infos (measure-infos *test-measure1*)))
    (is (equal '(nil)
               (mapcar #'info-starting-tuplets infos)))
    (is (equal '(nil)
               (mapcar #'info-ending-tuplets infos)))))

(deftest tuplet.2
  (let ((infos (measure-infos *test-measure2*)))
    (is (equal '(1 0 0)
               (mapcar #'length (mapcar #'info-starting-tuplets infos))))
    (is (equal '(0 0 1)
               (mapcar #'length (mapcar #'info-ending-tuplets infos))))))

(deftest-measures tuplet-setp
  (let ((infos (measure-infos m)))
    (is (setp (mapcan #'info-starting-tuplets infos) :test #'tuplet-eql))
    (is (setp (mapcan #'info-ending-tuplets infos) :test #'tuplet-eql))))

(deftest-measures tuplet-starting-ending-set-equal
  (let ((infos (measure-infos m)))
    (is (set-equal (mapcan #'info-starting-tuplets infos)
                   (mapcan #'info-ending-tuplets infos)
                   :test #'tuplet-eql))))

(deftest-measures number-of-tuples
  (let* ((infos (measure-infos m))
         (num (number-of-tuples infos)))
    (is (= num (length (mapcan #'info-starting-tuplets infos))))
    (is (= num (length (mapcan #'info-ending-tuplets infos))))))

(deftest tuplet.3
  (let ((infos (measure-infos *test-measure2*)))
    (is (equal '((3 2))
               (mapcar #'tuplet-tuplet-ratio
                       (mapcan #'info-starting-tuplets infos))))))

(deftest tuplet.4
  (let ((infos (measure-infos *test-measure7*)))
    (is (equal '((5 4))
               (mapcar #'tuplet-tuplet-ratio
                       (mapcan #'info-starting-tuplets infos))))))

(deftest measure-quarter-division.1
  (is (eql 1 (measure-quarter-division
              '((1 ((1 :START-TIME 4.0 :NOTES (60))))
                (1 ((1 :START-TIME 5.0 :NOTES (60))))
                (1 ((1 :START-TIME 6.0 :NOTES (60))))
                (1 ((1 :START-TIME 7.0 :NOTES (60))))
                :TIME-SIGNATURE (4 4)))))
  (is (eql 1 (measure-quarter-division
              '((1 ((1 :NOTES (60))))
                (1 ((1 :NOTES (60))))
                :TIME-SIGNATURE (2 4)))))
  (is (eql 2 (measure-quarter-division
              '((1 ((1 :NOTES (60))
                    (1 :NOTES (60))))
                (1 ((1 :NOTES (60))))
                :TIME-SIGNATURE (2 4)))))
  (is (eql 3 (measure-quarter-division
              '((1 ((1 :NOTES (60))
                    (1 :NOTES (60))
                    (1 :NOTES (60))))
                (1 ((1 :NOTES (60))))
                :TIME-SIGNATURE (2 4))))))

(deftest measure-quarter-division.2
  (is (eql 4 (measure-quarter-division
              '((5 ((-1  :NOTES (60)))) :TIME-SIGNATURE (5 16)))))
  (is (eql 2 (measure-quarter-division
              '((5 ((-1  :NOTES (60)))) :TIME-SIGNATURE (5 8))))))

(defun run-tests ()
  (run! :musicxml-pwgl))
