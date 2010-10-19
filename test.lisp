;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:test
  (:use #:cl #:myam #:mxml #:test-db)
  (:export
   #:run-tests))

(in-package #:test)

(defsuite* :musicxml)

(defun files-eql-p (a b)
  (let ((process (sb-ext:run-program
		  "/usr/bin/diff"
		  (list "-q" (namestring a) (namestring b)))))
    (zerop (sb-ext:process-exit-code process))))

(defun diff (a b)
  (with-output-to-string (out)
    (sb-ext:run-program
     "/usr/bin/diff"
     (list "-u" (namestring a) (namestring b)) :output out)))

(defun canonicalise (path new-path)
  (sb-ext:run-program
   "/bin/bash"
   (list "-c"
	 (format nil "Canonicalise <~A >~A"
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
(in-package #:test)

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
      (ppxml:pprint-xml (parse-xml-file-via-cxml xml) :stream out))
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
		   (:|accidental| "flat"))))
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

(deftest test-db
  (assert (list-test-cases))
  (dolist (test-case (list-test-cases))
    (ecase (status test-case)
      (:skip #+nil(skip "~A -- ~A" (name test-case) (description test-case)))
      (:run
       (with-open-file (out "/tmp/res.xml"
			    :direction :output
			    :if-exists :supersede)
	 (write-line "<?xml version='1.0' encoding='UTF-8' ?>" out)
	 (print-musicxml (e2m:enp2musicxml (enp test-case)) :stream out :no-header t))
       (canonicalise "/tmp/res.xml" "/tmp/resc.xml")
       (alexandria:write-string-into-file
	(with-output-to-string (out)
	  (write-string (string-remove-first-n-lines 3 (musicxml test-case))
			out))
	"/tmp/exp-o.xml" :if-exists :supersede)
       (cxml::trim-xml-file "/tmp/exp-o.xml" "/tmp/exp.xml")
       (canonicalise "/tmp/exp.xml" "/tmp/expc.xml")
       (is (files-eql-p "/tmp/resc.xml" "/tmp/expc.xml")
	   "~A failed~%~A"
	   (name test-case)
	   (diff "/tmp/resc.xml" "/tmp/expc.xml"))))))

(deftest pprint-xml-nil
  (is
   (string= "
<huhu>123<zzz></zzz></huhu>"
	    (with-output-to-string (out)
	      (ppxml:pprint-xml '(:|huhu| "123" (:|zzz| nil)) :stream out)))))


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
	(enp2musicxml::split-list-plist
	 (enp2musicxml::append-list-plist list plist))
      (is (equal list new-list))
      (is (equal plist new-plist)))))

(defun run-tests ()
  (run! :musicxml))
