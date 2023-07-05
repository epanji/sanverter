(cl:in-package #:sanverter)

(defun sanverter-version ()
  "Return the SANVERTER version."
  #.(let ((file (merge-pathnames "version.lisp-expr" (or *compile-file-pathname* *load-truename*))))
      (format nil "SANVERTER v~A" (with-open-file (stream file) (read stream)))))

(defun convert-srt-to-ass (file.srt &rest args
                           &key output (if-exists :rename) (if-does-not-exist :create)
                           &allow-other-keys)
  "Return FILE path after successfully convert to ASS format with adjustment from ARGS."
  (check-type file.srt pathname)
  (check-type output (or null pathname))
  (remf args :output)
  (remf args :if-exists)
  (remf args :if-does-not-exist)
  (if (or (string-equal "srt" (pathname-type file.srt))
          (string-equal "vtt" (pathname-type file.srt)))
      (let* ((file.out (or output file.srt))
             (file.ass (merge-pathnames (format nil "~A.ass" (pathname-name file.out)) file.out))
             (subtitle (apply 'parse-subrip file.srt args)))
        (with-open-file (stream file.ass :direction :output
                                         :if-exists if-exists
                                         :if-does-not-exist if-does-not-exist)
          (print-subass subtitle stream))
        file.ass)
      (error "Only allowing input file.srt or file.vtt!")))

(defun convert-ass-to-srt (file.ass &key output language (if-exists :rename) (if-does-not-exist :create))
  "Return FILE path after successfully convert to SRT or VTT compatible format by supplying LANGUAGE argument."
  (check-type file.ass pathname)
  (check-type output (or null pathname))
  (if (string-equal "ass" (pathname-type file.ass))
      (let* ((file.out (or output file.ass))
             (control (if (null language) "~A.srt" "~A.vtt"))
             (index (cond ((null language) 1)
                          ((integerp language) language)
                          ((stringp language) language)
                          (t "en")))
             (file.srt (merge-pathnames (format nil control (pathname-name file.out)) file.out)))
        (with-open-file (stream file.srt :direction :output
                                         :if-exists if-exists
                                         :if-does-not-exist if-does-not-exist)
          (print-subrip file.ass index stream))
        file.srt)
      (error "Only allowing input file.ass!")))

