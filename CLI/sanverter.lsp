;;;;
;;;; Author: Panji Kusuma <epanji@gmail.com>
;;;; Repository: https://github.com/epanji/sanverter
;;;;

(defvar *debug* nil)

(defvar *corrections* nil)

(defvar *sanverter-options*
  `(:format "ass"
    :output nil
    :language nil
    :if-exists :error
    :play-res-x 1280
    :play-res-y 720
    :layout-res-x nil
    :layout-res-y nil
    :collisions "Normal"
    :wrap-style 1
    :scaled-border-and-shadow nil
    :kerning nil
    :timer "100.0000"
    :title "Unknown"
    :fontname "DejaVu Sans"
    :fontsize 36
    :primary-colour "#EEDC82"
    :secondary-colour "#FFFFFF"
    :outline-colour "#00000000"
    :back-colour "#000000D4"
    :bold -1
    :italic 0
    :underline 0
    :strike-out 0
    :scale-x 100
    :scale-y 100
    :spacing 0
    :angle 0
    :border-style 1
    :outline 1
    :shadow 1
    :alignment 2
    :style-margin-l ,(* 6 36)
    :style-margin-r ,(* 6 36)
    :style-margin-v ,(* 2 36)
    :encoding 1))

(setq ext:*help-message* (format nil "~2&~
SANVERTER is cross converter for some subtitle formats (vtt, srt, ass).

Usage:
  sanverter [option ...] <file>

Global options:
  -h,   --help                                Display this help message.
  -v,   --version                             Display the version.
  -d,   --debug                               Enable debug.
  -c,   --corrections                         Enable corrections for same format.
  -f,   --format <value>                      Set output format with value options: 'ass', 'srt' or 'vtt'. (default ~A)
  -o,   --output <value>                      Set output name or full pathname without extension. (default ~A)
  -l,   --language <value>                    Set language for ASS and WEBVTT compatible format. (default ~A)
  -e,   --file-exists <value>                 Set action if file exists with value options: 'error', 'new-version', 'rename',
                                              'rename-and-delete', 'overwrite', 'append', 'supersede', or 'nil'. (default ~A)

Output ASS script options:
  -prx, --play-resolution-x <value>           Set play resolution x. (default ~A)
  -pry, --play-resolution-y <value>           Set play resolution y. (default ~A)
  -lrx, --layout-resolution-x <value>         Set layout resolution x. (default ~A)
  -lry, --layout-resolution-y <value>         Set layout resolution y. (default ~A)
  -col, --collisions <value>                  Set collisions with value options: 'Normal' or 'Reverse'. (default ~A)
  -wps, --wrap-style <value>                  Set wrap-style. (default ~A)
  -sbs, --scaled-border-and-shadow <value>    Set scaled border and shadow with value options: 'Yes' or 'No'. (default ~A)
  -krn, --kerning <value>                     Set kerning with value options: 'Yes' or 'No'. (default ~A)
  -tmr, --timer <value>                       Set timer. (default ~A)
  -ttl, --title <value>                       Set title. (default ~A)

Output ASS style options:
  -fn,  --font-name <value>                   Set font name. (default ~A)
  -fs,  --font-size <value>                   Set font size. (default ~A)
  -pc,  --primary-color <value>               Set primary color. (default ~A)
  -sc,  --secondary-color <value>             Set secondary color. (default ~A)
  -oc,  --outline-color <value>               Set outline color. (default ~A)
  -bc,  --back-color <value>                  Set back/shadow color. (default ~A)
  -b,   --bold <value>                        Enable font bold with -1 and disable it with 0. (default ~A)
  -i,   --italic <value>                      Enable font italic with -1 and disable it with 0. (default ~A)
  -u,   --underline <value>                   Enable font underline with -1 and disable it with 0. (default ~A)
  -s,   --strike-out <value>                  Enable font strike-out with -1 and disable it with 0. (default ~A)
  -sx,  --scale-x <value>                     Set scale x. (default ~A)
  -sy,  --scale-y <value>                     Set scale y. (default ~A)
  -sp,  --spacing <value>                     Set letter spacing. (default ~A)
  -a,   --angle <value>                       Set line angle. (default ~A)
  -bs,  --border-style <value>                Set border style. (default ~A)
  -oe,  --outline <value>                     Set outline thickness. (default ~A)
  -sw,  --shadow <value>                      Set shadow distance. (default ~A)
  -an,  --alignment <value>                   Set alignment according to numpad layout. (default ~A)
  -ml,  --margin-left <value>                 Set margin left. (default ~A)
  -mr,  --margin-right <value>                Set margin right. (default ~A)
  -mv,  --margin-vertical <value>             Set margin vertical 'top and bottom'. (default ~A)
  -enc, --encoding <value>                    Set encoding. (default ~A)

Author:
  Panji Kusuma <epanji@gmail.com>

License:
  BSD-2-Clause license~2%"
                                 (getf *sanverter-options* :format "Undefined")
                                 (getf *sanverter-options* :output "Undefined")
                                 (getf *sanverter-options* :language "Undefined")
                                 (getf *sanverter-options* :if-exists "Undefined")
                                 (getf *sanverter-options* :play-res-x "Undefined")
                                 (getf *sanverter-options* :play-res-y "Undefined")
                                 (getf *sanverter-options* :layout-res-x "Undefined")
                                 (getf *sanverter-options* :layout-res-y "Undefined")
                                 (getf *sanverter-options* :collisions "Undefined")
                                 (getf *sanverter-options* :wrap-style "Undefined")
                                 (getf *sanverter-options* :scaled-border-and-shadow "Undefined")
                                 (getf *sanverter-options* :kerning "Undefined")
                                 (getf *sanverter-options* :timer "Undefined")
                                 (getf *sanverter-options* :title "Undefined")
                                 (getf *sanverter-options* :fontname "Undefined")
                                 (getf *sanverter-options* :fontsize "Undefined")
                                 (getf *sanverter-options* :primary-colour "Undefined")
                                 (getf *sanverter-options* :secondary-colour "Undefined")
                                 (getf *sanverter-options* :outline-colour "Undefined")
                                 (getf *sanverter-options* :back-colour "Undefined")
                                 (getf *sanverter-options* :bold "Undefined")
                                 (getf *sanverter-options* :italic "Undefined")
                                 (getf *sanverter-options* :underline "Undefined")
                                 (getf *sanverter-options* :strike-out "Undefined")
                                 (getf *sanverter-options* :scale-x "Undefined")
                                 (getf *sanverter-options* :scale-y "Undefined")
                                 (getf *sanverter-options* :spacing "Undefined")
                                 (getf *sanverter-options* :angle "Undefined")
                                 (getf *sanverter-options* :border-style "Undefined")
                                 (getf *sanverter-options* :outline "Undefined")
                                 (getf *sanverter-options* :shadow "Undefined")
                                 (getf *sanverter-options* :alignment "Undefined")
                                 (getf *sanverter-options* :style-margin-l "Undefined")
                                 (getf *sanverter-options* :style-margin-r "Undefined")
                                 (getf *sanverter-options* :style-margin-v "Undefined")
                                 (getf *sanverter-options* :encoding "Undefined")))

(defun convert (file.any &rest args
                &key output (format "ass") (if-exists :rename) (if-does-not-exist :create)
                &allow-other-keys)
  (check-type file.any pathname)
  (check-type output (or null pathname))
  (remf args :output)
  (remf args :format)
  (remf args :if-exists)
  (remf args :if-does-not-exist)
  (when (string-equal format (pathname-type file.any))
    (when (null *corrections*)
      (format t "~2&Convert nothing for ~A with option '-f ~A' or '--format ~:*~A'.~@
                If you are sure about this, add option '-c' or '--corrections'.~2%"
              file.any format)
      (return-from convert file.any))
    (when (null output)
      (setf output (merge-pathnames (format nil "~A-rev.~A"
                                            (pathname-name file.any)
                                            (pathname-type file.any))
                                    file.any))))
  (let* ((index (cond ((string-equal "ass" format) nil)
                      ((string-equal "srt" format) 1)
                      ((string-equal "vtt" format) (getf args :language "en"))))
         (file.out (or output file.any))
         (file.type (merge-pathnames (format nil "~A.~A" (pathname-name file.out) format) file.out)))
    (handler-case (with-open-file (stream file.type :direction :output
                                                    :if-exists if-exists
                                                    :if-does-not-exist if-does-not-exist)
                    (if (string-equal "ass" format)
                        (sanverter:print-subass
                         (if (string-equal "ass" (pathname-type file.any))
                             (let ((subass (sanverter:parse-subass file.any)))
                               (setf (aref (claraoke:lines subass) 0)
                                     (apply 'claraoke:script-info "" args))
                               (claraoke:delete-style subass "Default")
                               (claraoke:insert-style subass (apply 'claraoke:style "Default" args))
                               subass)
                             (apply 'sanverter:parse-subrip file.any args))
                         stream)
                        (sanverter:print-subrip file.any index stream)))
      (file-error ()
        (format t "~2&File ~A already exists.~2%" file.type)
        (return-from convert file.type)))
    (unless (null if-exists)
      (format t "~2&Success converting ~A to ~A.~2%" file.any file.type))
    file.type))

(defconstant +sanverter-rules+
  '((("-h" "--help") 0
     (progn (princ ext:*help-message* *standard-output*)
            (ext:quit 0))
     :noloadrc)
    (("-v" "--version") 0
     (progn (format t "~2&~A~%~A~2%"
                    (sanverter:sanverter-version)
                    (claraoke-internal:version))
            (ext:quit 0))
     :noloadrc)
    (("-d" "--debug") 0
     (setf *debug* t)
     :noloadrc)
    (("-c" "--corrections") 0
     (setf *corrections* t)
     :noloadrc)
    (("-f" "--format") 1
     (let ((format 1))
       (setf (getf *sanverter-options* :format) format)
       (when (string-equal "vtt" format)
         (setf (getf *sanverter-options* :language)
               (or (getf *sanverter-options* :language nil)
                   "en"))))
     :noloadrc)
    (("-o" "--output") 1
     (let ((out 1))
       (setf (getf *sanverter-options* :output) (pathname out)))
     :noloadrc)
    (("-e" "--file-exists") 1
     (let ((if-exists 1))
       (setf (getf *sanverter-options* :if-exists)
             (cond ((string-equal "nil" if-exists) nil)
                   ((string-equal "error" if-exists) :error)
                   ((string-equal "new-version" if-exists) :new-version)
                   ((string-equal "rename" if-exists) :rename)
                   ((string-equal "rename-and-delete" if-exists) :rename-and-delete)
                   ((string-equal "overwrite" if-exists) :overwrite)
                   ((string-equal "append" if-exists) :append)
                   ((string-equal "supersede" if-exists) :supersede))))
     :noloadrc)
    (("-l" "--language") 1
     (let ((language 1))
       (setf (getf *sanverter-options* :language)
             (unless (string-equal "nil" language)
               language)))
     :noloadrc)
    ;; Script
    (("-prx" "--play-resolution-x") 1
     (setf (getf *sanverter-options* :play-res-x) 1)
     :noloadrc)
    (("-pry" "--play-resolution-y") 1
     (setf (getf *sanverter-options* :play-res-y) 1)
     :noloadrc)
    (("-lrx" "--layout-resolution-x") 1
     (setf (getf *sanverter-options* :layout-res-x) 1)
     :noloadrc)
    (("-lry" "--layout-resolution-y") 1
     (setf (getf *sanverter-options* :layout-res-y) 1)
     :noloadrc)
    (("-col" "--collisions") 1
     (setf (getf *sanverter-options* :collisions) 1)
     :noloadrc)
    (("-wps" "--wrap-style") 1
     (setf (getf *sanverter-options* :wrap-style) 1)
     :noloadrc)
    (("-sbs" "--scaled-border-and-shadow") 1
     (setf (getf *sanverter-options* :scaled-border-and-shadow) 1)
     :noloadrc)
    (("-krn" "--kerning") 1
     (setf (getf *sanverter-options* :kerning) 1)
     :noloadrc)
    (("-tmr" "--timer") 1
     (setf (getf *sanverter-options* :timer) 1)
     :noloadrc)
    (("-ttl" "--title") 1
     (setf (getf *sanverter-options* :title) 1)
     :noloadrc)
    ;; Styles
    (("-fn" "--font-name") 1
     (setf (getf *sanverter-options* :fontname) 1)
     :noloadrc)
    (("-fs" "--font-size") 1
     (setf (getf *sanverter-options* :fontsize) 1)
     :noloadrc)
    (("-pc" "--primary-color") 1
     (setf (getf *sanverter-options* :primary-colour) (claraoke:color 1))
     :noloadrc)
    (("-sc" "--secondary-color") 1
     (setf (getf *sanverter-options* :secondary-colour) (claraoke:color 1))
     :noloadrc)
    (("-oc" "--outline-color") 1
     (setf (getf *sanverter-options* :outline-colour) (claraoke:color 1))
     :noloadrc)
    (("-bc" "--back-color") 1
     (setf (getf *sanverter-options* :back-colour) (claraoke:color 1))
     :noloadrc)
    (("-b" "--bold") 1
     (setf (getf *sanverter-options* :bold) 1)
     :noloadrc)
    (("-i" "--italic") 1
     (setf (getf *sanverter-options* :italic) 1)
     :noloadrc)
    (("-u" "--underline") 1
     (setf (getf *sanverter-options* :underline) 1)
     :noloadrc)
    (("-s" "--strike-out") 1
     (setf (getf *sanverter-options* :strike-out) 1)
     :noloadrc)
    (("-sx" "--scale-x") 1
     (setf (getf *sanverter-options* :scale-x) 1)
     :noloadrc)
    (("-sy" "--scale-y") 1
     (setf (getf *sanverter-options* :scale-y) 1)
     :noloadrc)
    (("-sp" "--spacing") 1
     (setf (getf *sanverter-options* :spacing) 1)
     :noloadrc)
    (("-a" "--angle") 1
     (setf (getf *sanverter-options* :angle) 1)
     :noloadrc)
    (("-bs" "--border-style") 1
     (setf (getf *sanverter-options* :border-style) 1)
     :noloadrc)
    (("-oe" "--outline") 1
     (setf (getf *sanverter-options* :outline) 1)
     :noloadrc)
    (("-sw" "--shadow") 1
     (setf (getf *sanverter-options* :shadow) 1)
     :noloadrc)
    (("-an" "--alignment") 1
     (setf (getf *sanverter-options* :alignment) 1)
     :noloadrc)
    (("-ml" "--margin-left") 1
     (setf (getf *sanverter-options* :style-margin-l) 1)
     :noloadrc)
    (("-mr" "--margin-right") 1
     (setf (getf *sanverter-options* :style-margin-r) 1)
     :noloadrc)
    (("-mv" "--margin-vertical") 1
     (setf (getf *sanverter-options* :style-margin-v) 1)
     :noloadrc)
    (("-enc" "--encoding") 1
     (setf (getf *sanverter-options* :encoding) 1)
     :noloadrc)
    ("*DEFAULT*" 1
     (let ((file (pathname (first 1)))
           (debug (or *debug* (ext:getenv "DEBUG"))))
       (cond ((or (null file)
                  (null (probe-file file)))
              (princ ext:*help-message* *error-output*)
              (ext:quit 1))
             ((not (null debug))
              (format t "~2&Options:~%~{~2T~(~A~): ~A~%~}~%" *sanverter-options*))
             (t (apply 'convert file *sanverter-options*))))
     :stop)))

(let ((ext:*lisp-init-file-list* nil)) ; No initialization files
  (handler-case (ext:process-command-args :rules +sanverter-rules+)
    (error ()
      (princ ext:*help-message* *error-output*)
      (ext:quit 1))))

(ext:quit 0)

