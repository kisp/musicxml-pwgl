;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:test
  (:use #:cl #:myam)
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
		 (musicxml:to-lxml (musicxml:from-lxml lxml)))))))

(defun run-tests ()
  (run! :musicxml))
