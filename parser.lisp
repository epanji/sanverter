(cl:in-package #:sanverter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic rules
;;;
(esrap:defrule eof (esrap:! character))

(esrap:defrule newline (or #\Linefeed (and #\Return #\Linefeed)))

(esrap:defrule gchar (graphic-char-p character))
(esrap:defrule gchar* (* gchar))

(esrap:defrule space (or #\Space #\Tab))
(esrap:defrule space* (* space))

(esrap:defrule digit1 (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(esrap:defrule digit2 (and digit1 digit1))
(esrap:defrule digit3 (and digit1 digit1 digit1))
(esrap:defrule digit+ (+ digit1))

(esrap:defrule digitx1 (or digit1 #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F))
(esrap:defrule digitx3 (and digitx1 digitx1 digitx1))
(esrap:defrule digitx4 (and digitx3 digitx1))
(esrap:defrule digitx6 (and digitx3 digitx3))
(esrap:defrule digitx8 (and digitx6 digitx1 digitx1))

(esrap:defrule ignored-line (and gchar* newline)
  (:constant nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Timer rules
;;;
(esrap:defrule subrip-time1 (and digit+ ":" digit2 ":" digit2 (or "," ".") digit3)
  (:text t)
  (:lambda (txt)
    (substitute #\. #\, (subseq txt 0 (1- (length txt))))))

(esrap:defrule subrip-time2 (and subrip-time1 space* "-->" space* subrip-time1)
  (:destructure (tm1 ig1 ig2 ig3 tm2)
    (declare (ignore ig1 ig2 ig3))
    ;; VTT will assuming tm2 as duration length if it is not greater than tm1
    (unless (claraoke:duration-greaterp tm2 tm1)
      (setf tm2 (claraoke:durationstring
                 (claraoke:increase-duration
                  (claraoke:duration tm2) tm1))))
    (list :start tm1 :end tm2)))

(esrap:defrule subrip-timeline (and subrip-time2 gchar* newline)
  (:function first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CUE style rules
;;;
(defvar *cue-style* (make-hash-table :test 'equalp))

(esrap:defrule cue-open (and space* "{" space* (esrap:? newline))
  (:text t))
(esrap:defrule cue-close (and space* "}" newline)
  (:text t))
(esrap:defrule cue-element (+ (or (alphanumericp character) #\# #\.))
  (:text t))
(esrap:defrule cue-newline newline
  (:constant nil))
(esrap:defrule cue-line (and space* (+ (not (or space ":"))) ":"
                             space* (+ (not (or space ";"))) ";"
                             (* (or newline space)))
  (:constant nil))
(esrap:defrule cue-color-line (and space* "color:"
                                   space* (+ (not (or space ";"))) ";"
                                   (* (or newline space)))
  (:function fourth)
  (:text t))
(esrap:defrule cue-color (and "::cue(" cue-element ")"
                              cue-open
                              (+ (or cue-color-line
                                     cue-line
                                     cue-newline))
                              cue-close)
  (:lambda (lst)
    (let ((el (second lst))
          (co (first (remove nil (fifth lst)))))
      (unless (null co)
        (setf (gethash el *cue-style*) co)))
    nil))

(esrap:defrule cue-style (and "STYLE" newline (+ cue-color))
  (:constant nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text rules with tags
;;;
(esrap:defrule tag-open (or "<" "&lt;"))
(esrap:defrule tag-close (or ">" "&gt;"))
(esrap:defrule tag-bold-open (and tag-open "b" tag-close) (:constant "{\\b1}"))
(esrap:defrule tag-bold-close (and tag-open "/b" tag-close) (:constant "{\\b0}"))
(esrap:defrule tag-italic-open (and tag-open "i" tag-close) (:constant "{\\i1}"))
(esrap:defrule tag-italic-close (and tag-open "/i" tag-close) (:constant "{\\i0}"))
(esrap:defrule tag-underline-open (and tag-open "u" tag-close) (:constant "{\\u1}"))
(esrap:defrule tag-underline-close (and tag-open "/u" tag-close) (:constant "{\\u0}"))
(esrap:defrule tag-strikeout-open (and tag-open "s" tag-close) (:constant "{\\s1}"))
(esrap:defrule tag-strikeout-close (and tag-open "/s" tag-close) (:constant "{\\s0}"))

(esrap:defrule html-color (and #\# (or digitx8 digitx6 digitx4 digitx3))
  (:text t))

(esrap:defrule tag-cue-open (and tag-open "c" cue-element tag-close)
  (:lambda (lst)
    (let* ((el (third lst))
           (hc (concatenate 'string "#" (subseq el 1)))
           (de (if (claraoke-color:html-color-p hc)
                   (claraoke:color hc)
                   (claraoke:color el)))
           (co (gethash el *cue-style* de)))
      (format nil "{\\1c~A}" (claraoke:colorstring co)))))
(esrap:defrule tag-cue-close (and tag-open "/c" tag-close) (:constant "{\\1c}"))

(esrap:defrule tag-color-open (and tag-open (esrap:~ "font") space*
                                   (esrap:~ "color=") (esrap:? "\"")
                                   html-color
                                   (esrap:? "\"") space* tag-close)
  (:destructure (ig1 ig2 ig3 ig4 ig5 co ig6 ig7 ig8)
    (declare (ignore ig1 ig2 ig3 ig4 ig5 ig6 ig7 ig8))
    (format nil "{\\1c~A}" (claraoke:colorstring co))))
(esrap:defrule tag-color-close (and tag-open "/font" tag-close) (:constant "{\\1c}"))

(esrap:defrule html-tag (and (esrap:& tag-open)
                             (or tag-bold-open tag-bold-close
                                 tag-italic-open tag-italic-close
                                 tag-underline-open tag-underline-close
                                 tag-strikeout-open tag-strikeout-close
                                 tag-color-open tag-color-close
                                 tag-cue-open tag-cue-close))
  (:function second))

(esrap:defrule char-quot "&quot;" (:constant "\""))
(esrap:defrule char-amp "&amp;" (:constant "&"))
(esrap:defrule char-apos "&apos;" (:constant "'"))
(esrap:defrule char-nbsp "&nbsp;" (:constant " "))
(esrap:defrule char-lt "&lt;" (:constant "<"))
(esrap:defrule char-gt "&gt;" (:constant ">"))

(esrap:defrule html-char (and (esrap:& #\&)
                              (or char-quot char-amp char-apos
                                  char-nbsp char-lt char-gt))
  (:function second))

(esrap:defrule html-newline newline
  (:constant "\\N"))
(esrap:defrule html-text (+ (or html-tag html-char gchar))
  (:text t))
(esrap:defrule html-text-line (and html-text (or html-newline eof)))
(esrap:defrule subrip-textline (and (+ html-text-line) (or newline eof))
  (:destructure (lst ig)
    (declare (ignore ig))
    (format nil "~{~A~}" (butlast (apply 'append lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialogue rules
;;;
(esrap:defrule dialogue (and subrip-timeline
                             subrip-textline)
  (:destructure (times txt)
    (apply 'claraoke:dialogue txt times)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subrip rules
;;;
(esrap:defrule subrip (+ (or cue-style dialogue ignored-line))
  (:lambda (result)
    (remove nil result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subrip parser
;;;
(defgeneric parse-subrip (object &rest args &key &allow-other-keys)
  (:documentation "Return SUBTITLE object and list of DIALOGUES from parsing OBJECT argument.")
  (:method (object &key)
    (values nil nil))
  (:method ((object pathname) &rest args)
    (let ((string (alexandria:read-file-into-string object)))
      (apply 'parse-subrip string args)))
  (:method ((object string) &rest args)
    (clrhash *cue-style*)
    (let* ((concat (concatenate 'string object (string #\Newline)))
           (dialogues (esrap:parse 'subrip concat)))
      (apply 'parse-subrip dialogues args)))
  (:method ((object cons) &rest args)
    (setf (getf args :text) nil)
    (let ((subtitle (apply 'claraoke:subtitle "" args)))
      (setf (claraoke:lines (claraoke:events subtitle)) object)
      (claraoke:sort-events subtitle)
      ;; Does not return OBJECT because sort-events function is destructive
      (values subtitle (claraoke:lines (claraoke:events subtitle))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subass parser
;;;
(defgeneric parse-subass (object)
  (:documentation "Return SUBTITLE object and list of DIALOGUES from parsing OBJECT argument.")
  (:method (object)
    (values nil nil))
  (:method ((object pathname))
    (let ((string (alexandria:read-file-into-string object)))
      (parse-subass string)))
  (:method ((object string))
    (multiple-value-bind (subtitle dialogues)
        (handler-case (let ((subtitle1 (claraoke:parse-script (make-string-input-stream object))))
                        (claraoke:sort-events subtitle1)
                        (values subtitle1 (claraoke:lines (claraoke:events subtitle1))))
          (error () (parse-subass (make-string-input-stream object))))
      (let ((info (claraoke:lines (claraoke:script-info subtitle)))
            (styles (claraoke:lines (claraoke:styles subtitle))))
        (when (< (length info) 2)
          (setf (claraoke:lines (claraoke:script-info subtitle))
                (claraoke:lines (claraoke:script-info "Untitled"))))
        (when (null styles)
          (claraoke:insert-style subtitle (claraoke:style "Default")))
        (if (null dialogues)
            (parse-subass (make-string-input-stream object))
            (values subtitle dialogues)))))
  (:method ((object stream))
    (let ((subtitle (claraoke:subtitle "" :text nil))
          (dialogues (loop for line = (read-line object nil)
                           until (null line)
                           when (and (stringp line)
                                     (<= 9 (length line))
                                     (string-equal "Dialogue:" (subseq line 0 9)))
                             collect (claraoke-subtitle:dialogue-from-string line))))
      (setf (claraoke:lines (claraoke:events subtitle)) dialogues)
      (claraoke:sort-events subtitle)
      ;; Does not return DIALOGUES because sort-events function is destructive
      (values subtitle (claraoke:lines (claraoke:events subtitle))))))

