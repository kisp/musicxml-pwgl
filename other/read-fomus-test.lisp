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

(let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
  (set-pprint-dispatch '(cons (member :|pitch|) t)
                       (lambda (stream obj)
                         (format stream "~A~A" (second (assoc :|step| (cdr obj)))
                                 (second (assoc :|octave| (cdr obj))))))
  (set-pprint-dispatch '(cons (member :|note|) t)
                       (lambda (stream obj &aux chordp)
                         (pop obj)
                         (when (eql (first obj) :|chord|)
                           (setq chordp t)
                           (pop obj))
                         (toad-case1 obj
                                     ((list
                                       (and pitch (cons :|pitch| t))
                                       (list :|duration| duration)
                                       (list :|type| type)
                                       (list :|accidental| accidental)
                                       (list :|staff| staff))
                                      (format stream "<~W ~A ~A s~A>"
                                              pitch
                                              duration
                                              (subseq type 0 1)
                                              staff))
                                     ((list
                                       (and pitch (or :|rest| (cons :|pitch| t)))
                                       (list :|duration| duration)
                                       (list :|type| type)
                                       (list :|staff| staff))
                                      (format stream "<~W ~A ~A s~A>"
                                              pitch
                                              duration
                                              (subseq type 0 1)
                                              staff))
                                     (t (with-standard-io-syntax (error "~A" (prin1-to-string obj)))))
                         ))
  (set-pprint-dispatch '(member :|rest|)
                       (lambda (stream obj)
                         (format stream "R")))
  (pprint (s-xml:parse-xml-file "/tmp/foo.xml")))
