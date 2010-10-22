;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:test-db
  (:nicknames #:tdb)
  (:use #:cl #:sqlite-orm #:alexandria)
  (:export
   #:make-entry
   #:show-foto
   #:nth-enp
   #:show-nth-foto
   #:replace-nth-foto
   #:list-test-cases
   #:name
   #:description
   #:enp
   #:musicxml
   #:status
   #:enp-screen-shot
   #:score
   #:apropos-test-case
   #:what-next
   #:skipped-tests-that-pass))

(in-package #:tdb)

(defpclass test-case ()
  ((name            :accessor name            :initarg :name)
   (description     :accessor description     :initarg :description)
   (enp             :accessor enp             :initarg :enp)
   (musicxml        :accessor musicxml        :initarg :musicxml)
   (status          :accessor status          :initarg :status)
   (enp-screen-shot :accessor enp-screen-shot :initarg :enp-screen-shot)
   (score :accessor score :initarg :score)))

(defmethod print-object ((obj test-case) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A ~A"
	    (store-object-id obj) (name obj) (status obj))))

(unless (store-open-p)
  (let ((path (merge-pathnames
	       "tests.db"
	       (asdf:component-pathname (asdf:find-system :musicxml)))))
    (assert (probe-file path))
    (open-store path)))

(defun list-test-cases ()
  (list-by-class 'test-case))

(defun find-all (fn list)
  (remove-if-not fn list))

(defun apropos-test-case (name)
  (find-all (lambda (test-case)
	      (search name (name test-case) :test #'char-equal))
	    (list-test-cases)))

(defun make-entry (list)
  (destructuring-bind (name description enp musicxml status enp-screen-shot score)
      list
    (make-instance 'test-case
		   :score score
		   :enp-screen-shot enp-screen-shot
		   :status status
		   :musicxml musicxml
		   :enp enp
		   :description description
		   :name name)
    t))

(defun show-foto (test-case)
  (write-byte-vector-into-file
   (enp-screen-shot test-case)
   "/tmp/s.png" :if-exists :supersede)
  (sb-ext:run-program "/usr/bin/gnome-open" (list "/tmp/s.png"))
  nil)

(defun set-to-run (test-case)
  (setf (status test-case) :run))

(defun set-to-skip (test-case)
  (setf (status test-case) :skip))

(defun nth-enp (n)
  (enp (nth n (list-by-class 'test-case))))

(defun show-nth-foto (n)
  (show-foto (nth n (list-by-class 'test-case))))

(defun replace-nth-foto (n s)
  (setf (enp-screen-shot (nth n (list-by-class 'test-case)))
	s))

(defun what-next ()
  (labels ((count-lines (string)
	     (with-input-from-string (in string)
	       (loop for i upfrom 0
		  for line = (read-line in nil)
		  while line
		  finally (return i)))))
    (let (res)
      (dolist (tc (list-test-cases))
	(handler-case
	    (when (not (test::check-test-db-test-case tc))
	      (push
	       (list tc
		     (count-lines
		      (test::diff "/tmp/resc.xml" "/tmp/expc.xml")))
	       res))
	  (error nil)))
      (dolist (tc (sort res #'< :key #'second))
	(format t "~s~60t~s~%" (first tc) (second tc))))))

(defun skipped-tests-that-pass ()
  (dolist (tc (list-test-cases))
    (when (and (eql :skip (status tc))
	       (ignore-errors (test::check-test-db-test-case tc)))
      (format t "~s~%" tc))))
