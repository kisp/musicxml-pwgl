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

;;; http
(defun read-line* (stream)
  (prog1
      (with-output-to-string (out)
        (loop for ch = (read-char stream)
           until (char= ch #.(code-char 13))
           do (write-char ch out)))
    (read-char stream)))

(defun get-request (host port uri)
  (with-open-stream (http (comm:open-tcp-stream
                           host port
                           :errorp t :timeout 3))
    (format http "GET ~A HTTP/1.1~C~CHost: ~A~C~C~C~C"
            uri
            (code-char 13) (code-char 10)
            host
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))
    (force-output http)
    (destructuring-bind (start-line . headers)
        (loop for line = (read-line* http)
           until (zerop (length line))
           collect line)
      (unless (string= start-line "HTTP/1.1 200 OK")
        (error "start-line is ~a" start-line))
      (let ((content-length (parse-integer
                             (find "Content-Length" headers
                                   :test (lambda (a b) (string= a b :end2 (length a))))
                             :start 15)))
        (with-output-to-string (out)
          (loop repeat content-length
             do (write-char (read-char http) out)))))))

(defun parse-version (string)
  (let ((pos (position #\. string)))
    (if pos
        (cons (parse-integer (subseq string 0 pos))
              (parse-version (subseq string (1+ pos))))
        (list (parse-integer string)))))

(defun list< (a b)
  (if (and (null a) (null b))
      nil
      (or (< (car a) (car b))
          (and (= (car a) (car b))
               (list< (cdr a) (cdr b))))))

(defun get-version ()
  (get-request "46.4.11.6" 80 "/musicxml-pwgl/version"))

(defun version-check ()
  (let ((installed-version
         (parse-version (asdf:component-version (asdf:find-system :musicxml-pwgl))))
        (available-version
         (parse-version (get-version)))
        (thanks "Thanks for trying out the MusicXML export. ~
                 A simple demo patch is provided under ~
                 PWGL help... > Library Tutorials."))
    (cond ((equal installed-version available-version)
           (capi:display-message "~?~2%You have the latest version installed."
                                 thanks nil))
          ((list< installed-version available-version)
           (capi:display-message "~?~2%Your version is outdated. ~
                                 The current version is \"~{~A~^.~}\"."
                                 thanks nil available-version))
          (t
           ;; oops...
           ))))


(define-box upgrade ()
  (with-open-file (out "/tmp/f.tgz" :direction :output
                       :if-exists :supersede)
    (write-sequence
     (get-request
      "46.4.11.6" 80
      (format nil "/musicxml-pwgl/tarballs/musicxml-pwgl-~A.tgz"
              (get-version)))
     out))
  (when (probe-file "/tmp/musicxml-pwgl/")
    (cl-fad:delete-directory-and-files "/tmp/musicxml-pwgl/"))
  (asdf:run-shell-command "cd /tmp && tar xfz f.tgz")
  (let ((location (asdf:component-pathname (asdf:find-system :musicxml-pwgl))))
    (cl-fad:delete-directory-and-files location)
    (asdf:run-shell-command "mv '~A' '~A'"
                            "/tmp/musicxml-pwgl"
                            location))
  (asdf:oos 'asdf:load-op :musicxml-pwgl))

(defvar *version-check-done* nil)

(unless *version-check-done*
  (ignore-errors (version-check))
  (setq *version-check-done* t))

(install-menu musicxml-pwgl)
