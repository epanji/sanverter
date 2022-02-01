(cl:in-package #:sanverter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Subrip
;;;
(defvar *closer-stack* '())
(defvar *newline-stack* '())

(defgeneric print-subrip (object &optional index stream)
  (:documentation "Return NIL after printing OBJECT argument with default INDEX argument to STREAM argument."))

(defmethod print-subrip (object &optional index stream)
  (declare (ignore index stream))
  nil)

(defmethod print-subrip ((object null) &optional (index 1) stream)
  (declare (ignore index stream))
  nil)

(defmethod print-subrip ((object string) &optional (index 1) stream)
  (multiple-value-bind (subtitle dialogues) (parse-subrip object)
    (unless (null dialogues)
      (print-subrip subtitle index stream))))

(defmethod print-subrip ((object pathname) &optional (index 1) stream)
  (let ((type (pathname-type object)))
    (multiple-value-bind (subtitle dialogues)
        (cond ((or (string-equal "srt" type)
                   (string-equal "vtt" type))
               (parse-subrip object))
              ((string-equal "ass" type)
               (parse-subass object)))
      (unless (null dialogues)
        (print-subrip subtitle index stream)))))

(defmethod print-subrip ((object claraoke-subtitle:subtitle) &optional (index 1) stream)
  (let* ((cons1 (claraoke:lines (claraoke:events object)))
         (cons2 (loop for event in cons1
                      when (typep event 'claraoke-subtitle:dialogue)
                        collect event)))
    (print-subrip (reverse cons2) index stream)))

(defmethod print-subrip ((object cons) &optional (index 1) stream)
  (loop for i from index
        for dialogue in object
        do (print-subrip dialogue i stream)))

(defmethod print-subrip ((object claraoke-subtitle:dialogue) &optional (index 1) stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream))
        (start (claraoke:start object))
        (end (claraoke:end object))
        (text (claraoke:.text object)))
    (princ index stream)
    (terpri stream)
    (princ (timestring start) stream)
    (princ " --> " stream)
    (princ (timestring end) stream)
    (terpri stream)
    (princ (textstring text) stream)
    (terpri stream)
    (terpri stream)))

(defun timestring (object)
  (check-type object claraoke-duration:duration)
  (format nil "~2,'0D:~2,'0D:~2,'0D,~3,'0D"
          (claraoke:hours object)
          (claraoke:minutes object)
          (claraoke:seconds object)
          (* 10 (claraoke:centiseconds object))))

(defun textstring (object)
  (check-type object claraoke-text:text)
  (let ((overrides (claraoke:overrides object))
        (string (claraoke:.text object)))
    (with-output-to-string (stream)
      (loop with *closer-stack* = *closer-stack*
            and *newline-stack* = *newline-stack*
            for index from 0
            for char across string
            and override = (claraoke:find-override overrides index)
            do (print-text override stream)
               (princ char stream)
            finally (princ (apply 'concatenate 'string
                                  *closer-stack*)
                           stream)))))

(defgeneric print-text (object stream)
  (:method (object stream)
    nil)
  (:method ((object null) stream)
    nil)
  (:method ((object claraoke-text:modifier) stream)
    nil)
  (:method ((object claraoke-text:override) stream)
    (call-next-method object stream))
  (:method ((object cons) stream)
    (loop for override in object
          do (print-text override stream)))
  (:method ((object claraoke-text:batch) stream)
    (let ((modifiers (claraoke:modifiers object))
          (newline (claraoke:find-modifier object 'newline)))
      (when (typep newline 'claraoke-text:newline)
        (push (string #\Newline) *newline-stack*)
        (setf modifiers (remove newline modifiers)))
      (print-text (reverse modifiers) stream)))
  (:method ((object claraoke-text:newline) stream)
    (terpri stream))
  (:method ((object claraoke-text:bold) stream)
    (let ((arg (claraoke:arg1 object)))
      (cond ((zerop arg)
             (princ "</b>" stream)
             (princ (coerce (pop *newline-stack*) 'string) stream)
             (pop *closer-stack*))
            (t (princ "<b>" stream)
               (push "</b>" *closer-stack*)))))
  (:method ((object claraoke-text:italic) stream)
    (let ((arg (claraoke:arg1 object)))
      (cond ((zerop arg)
             (princ "</i>" stream)
             (princ (coerce (pop *newline-stack*) 'string) stream)
             (pop *closer-stack*))
            (t (princ "<i>" stream)
               (push "</i>" *closer-stack*)))))
  (:method ((object claraoke-text:underline) stream)
    (let ((arg (claraoke:arg1 object)))
      (cond ((zerop arg)
             (princ "</u>" stream)
             (princ (coerce (pop *newline-stack*) 'string) stream)
             (pop *closer-stack*))
            (t (princ "<u>" stream)
               (push "</u>" *closer-stack*)))))
  (:method ((object claraoke-text:strikeout) stream)
    (let ((arg (claraoke:arg1 object)))
      (cond ((zerop arg)
             (princ "</s>" stream)
             (princ (coerce (pop *newline-stack*) 'string) stream)
             (pop *closer-stack*))
            (t (princ "<s>" stream)
               (push "</s>" *closer-stack*)))))
  (:method ((object claraoke-text:color1) stream)
    (let* ((arg (claraoke:arg1 object))
           (color (claraoke:color arg))
           (r (claraoke:red color))
           (g (claraoke:green color))
           (b (claraoke:blue color))
           (a (claraoke:alpha color)))
      (format stream "<font color=\"#~2,'0X~2,'0X~2,'0X~@[~2,'0X~]\">" r g b a)
      (push "</font>" *closer-stack*)))
  (:method ((object claraoke-text:reset) stream)
    (princ "</font>" stream)
    (princ (coerce (pop *newline-stack*) 'string) stream)
    (pop *closer-stack*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Subass
;;;
(defgeneric print-subass (object &optional stream)
  (:documentation "Return NIL after printing OBJECT argument to STREAM argument."))

(defmethod print-subass (object &optional stream)
  (declare (ignore stream))
  nil)

(defmethod print-subass ((object null) &optional stream)
  (declare (ignore stream))
  nil)

(defmethod print-subass ((object string) &optional stream)
  (multiple-value-bind (subtitle dialogues)
      (parse-subass object)
    (unless (null dialogues)
      (print-subass subtitle stream))))

(defmethod print-subass ((object pathname) &optional stream)
  (let ((type (pathname-type object)))
    (multiple-value-bind (subtitle dialogues)
        (cond ((or (string-equal "srt" type)
                   (string-equal "vtt" type))
               (parse-subrip object))
              ((string-equal "ass" type)
               (parse-subass object)))
      (unless (null dialogues)
        (print-subass subtitle stream)))))

(defmethod print-subass ((object claraoke-subtitle:subtitle) &optional stream)
  (claraoke:print-script object stream)
  nil)

