;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:musicxml
  (:nicknames #:mxml)
  (:use #:cl #:pprint-xml)
  (:export
   #:mxml-equal
   #:128th
   #:16th
   #:256th
   #:32nd
   #:64th
   #:a
   #:b
   #:breve
   #:c
   #:d
   #:double-sharp
   #:e
   #:eighth
   #:f
   #:flat
   #:flat-flat
   #:from-lxml
   #:g
   #:half
   #:long
   #:make-constructor-form
   #:natural
   #:natural-flat
   #:natural-sharp
   #:no
   #:note
   #:pitch
   #:print-musicxml
   #:quarter
   #:quarter-flat
   #:quarter-sharp
   #:rest*
   #:sharp
   #:sharp-sharp
   #:start
   #:stop
   #:three-quarters-flat
   #:three-quarters-sharp
   #:time-modification
   #:to-lxml
   #:tuplet
   #:whole
   #:yes
   #:attributes
   #:accidental))

(in-package #:musicxml)

(defun print-musicxml (dom &key (stream t) no-header)
  (unless no-header
    (write-line
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" stream)
    (format stream "<!DOCTYPE score-partwise PUBLIC ~
                   \"-//Recordare//DTD MusicXML 2.0 Partwise//EN\" ~
                   \"http://www.musicxml.org/dtds/partwise.dtd\">~%"))
  (ppxml:pprint-xml dom :stream stream))

(defun from-lxml (dom)
  (cond ((and (consp dom)
              (consp (car dom)))
         (translate-from-lxml dom (caar dom)))
        ((consp dom)
         (translate-from-lxml dom (car dom)))
        ((stringp dom) dom)
        ((keywordp dom)
         (translate-from-lxml dom dom))
        (t
         (error "dunno with ~S?" dom))))

(defmethod translate-from-lxml (dom type)
  (if (consp dom)
      (cons (car dom)
            (mapcar 'from-lxml (cdr dom)))
      dom))

(defun to-lxml (obj)
  (translate-to-lxml obj))

(defmethod translate-to-lxml (obj)
  (if (consp obj)
      (cons (car obj)
            (mapcar 'translate-to-lxml (cdr obj)))
      obj))

(defgeneric make-constructor-form (obj))

(defun generic-pretty-printer (stream obj)
  (pprint-redispatch (translate-to-lxml obj) stream))

(defmacro assoc-bind (bindings exp &body body)
  (let ((=exp= (gensym "=EXP=")))
    `(let ((,=exp= ,exp))
       (let ,(mapcar
              (lambda (binding)
                `(,binding
                  (second (assoc
                           ,(intern (string-downcase (string binding))
                                    "KEYWORD")
                           ,=exp=))))
              bindings)
         ,@body))))

(defun assoc* (item list)
  (if (null list)
      nil
      (let ((candidate (first list)))
        (if (cond ((and (consp candidate)
                        (consp (first candidate)))
                   (eql item (caar candidate)))
                  ((consp candidate)
                   (eql item (first candidate)))
                  (t
                   (eql item candidate)))
            candidate
            (assoc* item (rest list))))))

(defmacro assoc-bind* (bindings exp &body body)
  (let ((=exp= (gensym "=EXP=")))
    `(let ((,=exp= ,exp))
       (let ,(mapcar
              (lambda (binding)
                `(,binding
                  (assoc*
                   ,(intern (string-downcase (string binding))
                            "KEYWORD")
                   ,=exp=)))
              bindings)
         ,@body))))

(defun intern* (name)
  (intern (string-upcase name) (find-package "MXML")))

(defun mxml-equal (a b)
  (equal (translate-to-lxml a)
         (translate-to-lxml b)))

;;; eval-when currently needed by PWGL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct musicxml-object))

(defmethod print-object ((musicxml-object musicxml-object) stream)
  (write-string "#." stream)
  (prin1 (make-constructor-form musicxml-object) stream))

;;; pitch
(deftype pitch-step ()
  '(member c d e f g a b))

(defstruct (pitch (:include musicxml-object))
  (step nil :type pitch-step) alter octave)

(defmethod translate-from-lxml (dom (type (eql ':|pitch|)))
  (assoc-bind (step alter octave) (cdr dom)
              (make-pitch :step (intern* step)
                          :alter (if (null alter) 0 (read-from-string alter))
                          :octave (parse-integer octave))))

(defmethod translate-to-lxml ((pitch pitch))
  `(:|pitch|
     (:|step| ,(string (pitch-step pitch)))
     ,@(unless
        (eql 0 (pitch-alter pitch))
        `((:|alter| ,(princ-to-string (pitch-alter pitch)))))
     (:|octave| ,(princ-to-string (pitch-octave pitch)))))

(defmethod make-constructor-form ((pitch pitch))
  `(pitch ',(pitch-step pitch)
          ,(pitch-alter pitch)
          ,(pitch-octave pitch)))

(defun pitch (step alter octave)
  (make-pitch :step step :alter alter :octave octave))

(set-pprint-dispatch 'pitch 'generic-pretty-printer 0 *pprint-xml-table*)

;;; rest*
(defstruct (rest* (:include musicxml-object)))

(defmethod translate-from-lxml (dom (type (eql ':|rest|)))
  (make-rest*))

(defmethod translate-to-lxml ((rest* rest*))
  :|rest|)

(defmethod make-constructor-form ((rest* rest*))
  '(rest*))

(defun rest* () (make-rest*))

(set-pprint-dispatch 'rest* 'generic-pretty-printer 0 *pprint-xml-table*)

;;; note
(deftype accidental ()
  '(member nil sharp natural flat double-sharp sharp-sharp flat-flat
    natural-sharp natural-flat quarter-flat quarter-sharp three-quarters-flat
    three-quarters-sharp))

(deftype note-type ()
  '(member nil 256th 128th 64th 32nd 16th
    eighth quarter half whole breve long))

(defstruct (note (:include musicxml-object))
  pitch-or-rest duration chordp staff
  (accidental nil :type accidental)
  (type nil :type note-type)
  (dots nil :type (integer 0 3))
  notations
  (tie-start nil :type boolean)
  (tie-stop nil :type boolean)
  (time-modification nil :type (or null time-modification))
  beam-begin beam-continue beam-end)

(defun beam-element-p (dom)
  (and (consp dom)
       (consp (first dom))
       (eql :|beam| (caar dom))))

(defun tied-element-p (dom)
  (and (consp dom)
       (consp (first dom))
       (eql :|tied| (caar dom))))

(defun decode-beams (list-of-beams)
  "Returns list of beams for start, continue and end."
  (flet ((beam-number (beam)
           (parse-integer (third (car beam))))
         (beam-type (beam)
           (intern* (second beam))))
    (let (start continue end)
      (dolist (beam list-of-beams)
        (ecase (beam-type beam)
          (begin (push (beam-number beam) start))
          (continue (push (beam-number beam) continue))
          (end (push (beam-number beam) end))))
      (values (nreverse start) (nreverse continue) (nreverse end)))))

(defmethod translate-from-lxml (dom (type (eql ':|note|)))
  (assoc-bind* (duration chord rest pitch staff
                         accidental type notations
                         time-modification)
               dom
               (multiple-value-bind (beam-begin beam-continue beam-end)
                   (decode-beams (remove-if-not 'beam-element-p dom))
                 (make-note :pitch-or-rest (if rest (rest*) (from-lxml pitch))
                            :duration (parse-integer (second duration))
                            :chordp chord
                            :staff (and staff (parse-integer (second staff)))
                            :accidental (and accidental (intern* (second accidental)))
                            :type (and type (intern* (second type)))
                            :dots (count '(:|dot|) (cdr dom) :test #'equal)
                            :notations (mapcar #'from-lxml
                                               (remove-if #'tied-element-p (rest notations)))
                            :tie-start (when (find '((:|tie| :|type| "start"))
                                                   (cdr dom) :test #'equal)
                                         t)
                            :tie-stop (when (find '((:|tie| :|type| "stop"))
                                                  (cdr dom) :test #'equal)
                                        t)
                            :time-modification (and time-modification
                                                    (from-lxml time-modification))
                            :beam-begin beam-begin
                            :beam-continue beam-continue
                            :beam-end beam-end))))

(defmethod translate-to-lxml ((note note))
  `(:|note|
     ,@(when (note-chordp note) '(:|chord|))
     ,(translate-to-lxml (note-pitch-or-rest note))
     (:|duration| ,(princ-to-string (note-duration note)))
     ,@(when (note-tie-stop note) '(((:|tie| :|type| "stop"))))
     ,@(when (note-tie-start note) '(((:|tie| :|type| "start"))))
     ,@(when (note-type note)
             `((:|type| ,(string-downcase (symbol-name (note-type note))))))
     ,@(loop repeat (note-dots note) collect '(:|dot|))
     ,@(when (note-accidental note)
             `((:|accidental|
                 ,(string-downcase (symbol-name (note-accidental note))))))
     ,@(when (note-time-modification note)
             (list (translate-to-lxml (note-time-modification note))))
     ,@(when (note-staff note)
             `((:|staff| ,(princ-to-string (note-staff note)))))
     ,@(sort
        (append
         (mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
                               "begin"))
                 (note-beam-begin note))
         (mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
                               "continue"))
                 (note-beam-continue note))
         (mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
                               "end"))
                 (note-beam-end note)))
        #'< :key (lambda (x) (parse-integer (third (car x)))))
     ,@(when (note-notations* note)
             `((:|notations| ,@(mapcar #'to-lxml (note-notations* note)))))))

(defmethod make-constructor-form ((note note))
  `(note ,(note-pitch-or-rest note)
         ,(note-duration note)
         ',(note-type note)
         ,(note-dots note)
         ',(note-accidental note)
         :chordp ,(note-chordp note)
         :tie-start ,(note-tie-start note)
         :tie-stop ,(note-tie-stop note)
         :staff ,(note-staff note)
         :notations ',(remove-if #'tied-element-p (note-notations note))
         :time-modification ,(note-time-modification note)
         :beam-begin ,(note-beam-begin note)
         :beam-continue ,(note-beam-continue note)
         :beam-end ,(note-beam-end note)))

(defun note (pitch-or-rest duration type dots accidental
             &key chordp staff notations tie-start tie-stop
             time-modification
             beam-begin beam-continue beam-end)
  (make-note :pitch-or-rest pitch-or-rest :duration duration :chordp chordp
             :staff staff :accidental accidental :type type
             :time-modification time-modification
             :beam-begin beam-begin
             :beam-continue beam-continue
             :beam-end beam-end
             :dots dots
             :tie-start tie-start
             :tie-stop tie-stop
             :notations notations))

(defun note-notations* (note)
  (append (when (note-tie-stop note) '(((:|tied| :|type| "stop"))))
          (when (note-tie-start note) '(((:|tied| :|type| "start"))))
          (note-notations note)))

(set-pprint-dispatch 'note 'generic-pretty-printer 0 *pprint-xml-table*)

;;; time-modification
(defstruct (time-modification (:include musicxml-object))
  actual-notes normal-notes
  (normal-type nil :type note-type))

(defmethod translate-from-lxml (dom (type (eql ':|time-modification|)))
  (assoc-bind (actual-notes normal-notes normal-type) (cdr dom)
              (make-time-modification
               :actual-notes (parse-integer actual-notes)
               :normal-notes (parse-integer normal-notes)
               :normal-type (intern* normal-type))))

(defmethod translate-to-lxml ((time-modification time-modification))
  `(:|time-modification|
     (:|actual-notes|
       ,(princ-to-string (time-modification-actual-notes time-modification)))
     (:|normal-notes|
       ,(princ-to-string (time-modification-normal-notes time-modification)))
     ,@(when (time-modification-normal-type time-modification)
             `((:|normal-type|
                 ,(string-downcase (symbol-name (time-modification-normal-type
                                                 time-modification))))))))

(defmethod make-constructor-form ((time-modification time-modification))
  `(time-modification
    ,(time-modification-actual-notes time-modification)
    ,(time-modification-normal-notes time-modification)
    ',(time-modification-normal-type time-modification)))

(defun time-modification (actual-notes normal-notes normal-type)
  (make-time-modification :actual-notes actual-notes
                          :normal-notes normal-notes
                          :normal-type normal-type))

(set-pprint-dispatch 'time-modification 'generic-pretty-printer
                     0 *pprint-xml-table*)

;;; tuplet
(deftype start-stop ()
  '(member start stop))

(deftype yes-no? ()
  '(member nil yes no))

(defstruct (tuplet (:include musicxml-object))
  (type nil :type start-stop)
  id actual-number actual-type normal-number normal-type
  (bracket nil :type yes-no?))

(defmethod translate-from-lxml (dom (type (eql ':|tuplet|)))
  (assoc-bind* (tuplet-actual tuplet-normal) (cdr dom)
               (make-tuplet :type (intern* (third (car dom)))
                            :id (parse-integer (fifth (car dom)))
                            :actual-number (let ((x (second (assoc :|tuplet-number|
                                                                   (cdr tuplet-actual)))))
                                             (and x (parse-integer x)))
                            :actual-type (intern*
                                          (second (assoc :|tuplet-type|
                                                         (cdr tuplet-actual))))
                            :normal-number (let ((x (second (assoc :|tuplet-number|
                                                                   (cdr tuplet-normal)))))
                                             (and x (parse-integer x)))
                            :normal-type (intern*
                                          (second (assoc :|tuplet-type|
                                                         (cdr tuplet-normal))))
                            :bracket nil)))

(defmethod translate-to-lxml ((tuplet tuplet))
  `((:|tuplet|
      :|type|
      ,(string-downcase (symbol-name (tuplet-type tuplet)))
      :|number| ,(princ-to-string (tuplet-id tuplet))
      ,@(when (tuplet-bracket tuplet)
              `(:|bracket|
                 ,(string-downcase (symbol-name (tuplet-bracket tuplet))))))
    ,@(when (tuplet-actual-number tuplet)
            `((:|tuplet-actual|
                (:|tuplet-number|
                  ,(princ-to-string (tuplet-actual-number tuplet)))
                ,@(when (tuplet-actual-type tuplet)
                        `((:|tuplet-type|
                            ,(string-downcase (symbol-name (tuplet-actual-type tuplet)))))))))
    ,@(when (tuplet-normal-number tuplet)
            `((:|tuplet-normal|
                ,@(when (tuplet-normal-number tuplet)
                        `((:|tuplet-number| ,(princ-to-string (tuplet-normal-number tuplet)))))
                ,@(when (tuplet-normal-type tuplet)
                        `((:|tuplet-type|
                            ,(string-downcase (symbol-name (tuplet-normal-type tuplet)))))))))))

(defmethod make-constructor-form ((tuplet tuplet))
  `(tuplet ',(tuplet-type tuplet)
           ,(tuplet-id tuplet)
           ,(tuplet-actual-number tuplet)
           ',(tuplet-actual-type tuplet)
           ,(tuplet-normal-number tuplet)
           ',(tuplet-normal-type tuplet)
           ',(tuplet-bracket tuplet)))

(defun tuplet (type id &optional actual-number actual-type normal-number normal-type bracket)
  (make-tuplet :type type
               :id id
               :actual-number actual-number
               :actual-type actual-type
               :normal-number normal-number
               :normal-type normal-type
               :bracket bracket))

(set-pprint-dispatch 'tuplet 'generic-pretty-printer 0 *pprint-xml-table*)

;;; attributes
(deftype clef-sign ()
  '(member g))

(defstruct (attributes (:include musicxml-object))
  divisions time clef staves key)

(defmethod translate-from-lxml (dom (type (eql ':|attributes|)))
  (assoc-bind* (divisions time clef staves key) (cdr dom)
               (make-attributes :divisions (and divisions
                                                (parse-integer (second divisions)))
                                :staves (and staves (parse-integer (second staves)))
                                :key (and key
                                          (assoc-bind (fifths) (cdr key)
                                                      (parse-integer fifths)))
                                :time (and time
                                           (assoc-bind (beats beat-type) (cdr time)
                                                       (list (parse-integer beats)
                                                             (parse-integer beat-type))))
                                :clef (and clef
                                           (assoc-bind (sign line) (cdr clef)
                                                       (list (intern* sign)
                                                             (and line (parse-integer line))))))))

(defmethod translate-to-lxml ((attributes attributes))
  (let ((dom `(:|attributes|
                ,@(when (attributes-divisions attributes)
                        `((:|divisions|
                            ,(princ-to-string
                              (attributes-divisions attributes)))))
                ,@(when (attributes-key attributes)
                        `((:|key| (:|fifths|
                                    ,(princ-to-string
                                      (attributes-key attributes))))))
                ,@(when (attributes-time attributes)
                        `((:|time|
                            (:|beats|
                              ,(princ-to-string
                                (first (attributes-time attributes))))
                            (:|beat-type|
                              ,(princ-to-string
                                (second (attributes-time attributes)))))))
                ,@(when (attributes-staves attributes)
                        `((:|staves|
                            ,(princ-to-string
                              (attributes-staves attributes)))))
                ,@(cond
                   ;; KLUDGE
                   ((and (attributes-staves attributes)
                         (= 2 (attributes-staves attributes)))
                    '(((:|clef| :|number| "1") (:|sign| "G"))
                      ((:|clef| :|number| "2") (:|sign| "F"))))
                   ((attributes-clef attributes)
                    `((:|clef|
                        (:|sign|
                          ,(symbol-name (first (attributes-clef attributes))))
                        ,@(when
                           (second (attributes-clef attributes))
                           `((:|line|
                               ,(princ-to-string
                                 (second (attributes-clef attributes)))))))))
                   (t nil)))))
    (if (cdr dom)
        dom
        nil)))

(defmethod make-constructor-form ((attributes attributes))
  `(attributes :divisions ,(attributes-divisions attributes)
               :key ,(attributes-key attributes)
               :staves ,(attributes-staves attributes)
               :time ',(attributes-time attributes)
               :clef ',(attributes-clef attributes)))

(defun attributes (&rest args &key divisions time clef staves key)
  (declare (ignore divisions time clef staves key))
  (apply #'make-attributes args))

(set-pprint-dispatch 'attributes 'generic-pretty-printer 0 *pprint-xml-table*)
