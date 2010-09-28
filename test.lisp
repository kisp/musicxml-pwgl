;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:test
  (:use #:cl #:myam #:mxml)
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

(defun canonicalize (path new-path)
  (sb-ext:run-program
   "/bin/bash"
   (list "-c"
	 (format nil "Canonicalise <~A >~A"
		 (namestring path) (namestring new-path)))))

(deftest s-xml-read-write
  (dolist (xml (directory "any-xmls/*.xml"))
    (with-open-file (out "/tmp/foo.xml"
			 :direction :output
			 :if-exists :supersede)
      (s-xml:print-xml (parse-xml-file-via-cxml xml) :stream out))
    (canonicalize "/tmp/foo.xml" "/tmp/fooc.xml")
    (canonicalize xml "/tmp/origc.xml")
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
    (canonicalize "/tmp/foo.xml" "/tmp/fooc.xml")
    (canonicalize xml "/tmp/origc.xml")
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

(defun run-tests ()
  (run! :musicxml))
