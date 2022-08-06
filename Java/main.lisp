(cl:in-package #:sanverter-java)

;;; Video sizes
(defclass video-size ()
  ((%name
    :initarg :name
    :initform (error "Name is required.")
    :accessor name)
   (%x
    :initarg :x
    :initform (error "X is required.")
    :accessor x)
   (%y
    :initarg :y
    :initform (error "Y is required.")
    :accessor y)))

(defvar *video-sizes*
  (stable-sort
   (list
    (make-instance 'video-size :name "ntsc" :x 720 :y 480)
    (make-instance 'video-size :name "pal" :x 720 :y 576)
    (make-instance 'video-size :name "qntsc" :x 352 :y 240)
    (make-instance 'video-size :name "qpal" :x 352 :y 288)
    (make-instance 'video-size :name "sntsc" :x 640 :y 480)
    (make-instance 'video-size :name "spal" :x 768 :y 576)
    (make-instance 'video-size :name "film" :x 352 :y 240)
    (make-instance 'video-size :name "ntsc-film" :x 352 :y 240)
    (make-instance 'video-size :name "sqcif" :x 128 :y 96)
    (make-instance 'video-size :name "qcif" :x 176 :y 144)
    (make-instance 'video-size :name "cif" :x 352 :y 288)
    (make-instance 'video-size :name "4cif" :x 704 :y 576)
    (make-instance 'video-size :name "16cif" :x 1408 :y 1152)
    (make-instance 'video-size :name "qqvga" :x 160 :y 120)
    (make-instance 'video-size :name "qvga" :x 320 :y 240)
    (make-instance 'video-size :name "vga" :x 640 :y 480)
    (make-instance 'video-size :name "svga" :x 800 :y 600)
    (make-instance 'video-size :name "xga" :x 1024 :y 768)
    (make-instance 'video-size :name "uxga" :x 1600 :y 1200)
    (make-instance 'video-size :name "qxga" :x 2048 :y 1536)
    (make-instance 'video-size :name "sxga" :x 1280 :y 1024)
    (make-instance 'video-size :name "qsxga" :x 2560 :y 2048)
    (make-instance 'video-size :name "hsxga" :x 5120 :y 4096)
    (make-instance 'video-size :name "wvga" :x 852 :y 480)
    (make-instance 'video-size :name "wxga" :x 1366 :y 768)
    (make-instance 'video-size :name "wsxga" :x 1600 :y 1024)
    (make-instance 'video-size :name "wuxga" :x 1920 :y 1200)
    (make-instance 'video-size :name "woxga" :x 2560 :y 1600)
    (make-instance 'video-size :name "wqsxga" :x 3200 :y 2048)
    (make-instance 'video-size :name "wquxga" :x 3840 :y 2400)
    (make-instance 'video-size :name "whsxga" :x 6400 :y 4096)
    (make-instance 'video-size :name "whuxga" :x 7680 :y 4800)
    (make-instance 'video-size :name "cga" :x 320 :y 200)
    (make-instance 'video-size :name "ega" :x 640 :y 350)
    (make-instance 'video-size :name "hd480" :x 852 :y 480)
    (make-instance 'video-size :name "hd720" :x 1280 :y 720)
    (make-instance 'video-size :name "hd1080" :x 1920 :y 1080)
    (make-instance 'video-size :name "2k" :x 2048 :y 1080)
    (make-instance 'video-size :name "2kflat" :x 1998 :y 1080)
    (make-instance 'video-size :name "2kscope" :x 2048 :y 858)
    (make-instance 'video-size :name "4k" :x 4096 :y 2160)
    (make-instance 'video-size :name "4kflat" :x 3996 :y 2160)
    (make-instance 'video-size :name "4kscope" :x 4096 :y 1716)
    (make-instance 'video-size :name "nhd" :x 640 :y 360)
    (make-instance 'video-size :name "hqvga" :x 240 :y 160)
    (make-instance 'video-size :name "wqvga" :x 400 :y 240)
    (make-instance 'video-size :name "fwqvga" :x 432 :y 240)
    (make-instance 'video-size :name "hvga" :x 480 :y 320)
    (make-instance 'video-size :name "qhd" :x 960 :y 540)
    (make-instance 'video-size :name "2kdci" :x 2048 :y 1080)
    (make-instance 'video-size :name "4kdci" :x 4096 :y 2160)
    (make-instance 'video-size :name "uhd2160" :x 3840 :y 2160)
    (make-instance 'video-size :name "uhd4320" :x 7680 :y 4320))
   (lambda (item1 item2)
     (if (= (x item1) (x item2))
         (< (y item1) (y item2))
         (< (x item1) (x item2))))))

(defun pretty-name (video-size)
  (declare (type video-size video-size))
  (format nil "~@:(~10A~) (~Dx~D)"
          (name video-size)
          (x video-size)
          (y video-size)))

(defun find-video-size (string &optional (fun 'pretty-name))
  (find string *video-sizes* :key fun :test 'string-equal))

(defun video-sizes (&optional type-or-fun)
  (let* ((fun (if (functionp type-or-fun) type-or-fun 'pretty-name))
         (names (mapcar fun *video-sizes*)))
    (case type-or-fun
      (:raw (java:jarray-from-list names))
      (:list names)
      (otherwise *video-sizes*))))

;;; Border styles
(defclass border-style ()
  ((%name
    :initarg :name
    :initform (error "Name is required.")
    :accessor name)
   (%value
    :initarg :value
    :initform (error "Value is required.")
    :accessor value)))

(defvar *border-styles*
  (list
   (make-instance 'border-style :name "Outline and Drop shadow" :value 1)
   (make-instance 'border-style :name "Opaque box each line" :value 3)
   (make-instance 'border-style :name "Opaque box whole line" :value 4)))

(defun find-border-style (string)
  (find string *border-styles* :key 'name :test 'string-equal))

(defun border-styles (&optional type)
  (let ((names (mapcar 'name *border-styles*)))
    (case type
      (:raw (java:jarray-from-list names))
      (:list names)
      (otherwise *border-styles*))))

;;; Main function
(defun main ()
  (let* ((frame (make-frame "SANVERTER"))
         (pane (content-pane frame))
         (file-label (make-label "-" :left))
         (open-file-button (make-button "Open"))
         (convert-file-button (make-button "Convert"))
         (quit-button (make-button "Quit"))
         (open-file-chooser (make-fchooser))
         (open-file-filter (make-fchooser-filter
                            "File srt / vtt / ass"
                            "srt" "vtt" "ass"))
         (title-field (make-text-field "Untitled"))
         (fontname-dropdown (make-combo (font-names :raw)))
         (fontsize-spinner (make-spinner-number 32))
         (bold-checkbox (make-check ""))
         (italic-checkbox (make-check ""))
         (video-size-dropdown (make-combo (video-sizes :raw)))
         (border-style-dropdown (make-combo (border-styles :raw)))
         (vertical-margin-spinner (make-spinner-number 72))
         (fontshadow-spinner (make-spinner-number 1))
         (icon-label (make-image-label *icon* "" :left))
         (open-color-button (make-button "Click me!")))
    (flet ((change-font ()
             (let ((fontname (selected-item fontname-dropdown))
                   (fontsize (value fontsize-spinner))
                   (fontstyle (logxor (checked-value bold-checkbox 1 0)
                                      (checked-value italic-checkbox 2 0))))
               (java:jcall "setFont" open-color-button
                           (make-font fontname fontsize
                                      (ecase fontstyle
                                        (0 :plain)
                                        (1 :bold)
                                        (2 :italic)
                                        (3 :bold-italic))))
               (pack frame))))
      ;; Frame
      (layout frame (make-gb-layout))
      (titled-border pane (make-string 2 :initial-element #\Space))
      (close-operation frame :dispose)
      (visible frame :true)
      ;; Components
      (enable-components
       :false
       title-field fontname-dropdown fontsize-spinner
       open-color-button bold-checkbox italic-checkbox
       video-size-dropdown border-style-dropdown
       vertical-margin-spinner fontshadow-spinner)
      (selected-item fontname-dropdown (ensure-font-name "Arial"))
      (selected-item video-size-dropdown (pretty-name (find-video-size "hd720" 'name)))
      (selected-item border-style-dropdown (find-border-style "Outline and Drop shadow"))
      (selected bold-checkbox :true)
      (foreground-color open-color-button (make-color 255 165 0))
      (background-color open-color-button (make-color 70 70 70))
      (change-font)
      ;; Open file
      (enable convert-file-button :false)
      (add-fchooser-filter open-file-chooser open-file-filter :false)
      (add-action-listener
       open-file-button
       (lambda (e)
         (declare (ignore e))
         (let ((file (open-fchooser open-file-chooser frame)))
           (unless (null file)
             (java:jcall "setText" file-label (namestring file))
             (if (or (string-equal (pathname-type file) "srt")
                     (string-equal (pathname-type file) "vtt"))
                 (enable-components
                  :true
                  title-field fontname-dropdown fontsize-spinner
                  open-color-button bold-checkbox italic-checkbox
                  video-size-dropdown border-style-dropdown
                  vertical-margin-spinner fontshadow-spinner)
                 (enable-components
                  :false
                  title-field fontname-dropdown fontsize-spinner
                  open-color-button bold-checkbox italic-checkbox
                  video-size-dropdown border-style-dropdown
                  vertical-margin-spinner fontshadow-spinner))
             (enable convert-file-button :true)
             (pack frame)))))
      ;; Open color
      (add-action-listener
       open-color-button
       (lambda (e)
         (declare (ignore e))
         (let ((c (open-cchooser open-color-button)))
           (unless (null c)
             (foreground-color open-color-button c)))))
      ;; Convert file
      (add-action-listener
       convert-file-button
       (lambda (e)
         (declare (ignore e))
         (let ((input (probe-file (java:jcall "getText" file-label))))
           (unless (null input)
             (let* ((in-name (pathname-name input))
                    (in-type (pathname-type input))
                    (output (merge-pathnames
                             (format nil "~A.~A" in-name
                                     (if (string-equal "ass" in-type)
                                         "srt"
                                         "ass"))
                             input))
                    (write-option (nth-value 1 (dialog-return nil :yes))))
               (when (probe-file output)
                 (setf write-option
                       (show-option-dialog
                        frame
                        (format nil "File ~A already exists. Supersede the file?"
                                output)
                        "File exists" :yes-no :warning)))
               (when (dialog-return write-option :yes)
                 (if (string-equal "ass" in-type)
                     (sanverter:convert-ass-to-srt input :if-exists :supersede)
                     (let ((title-text (java:jcall "getText" title-field))
                           (fontname (selected-item fontname-dropdown))
                           (fontsize (value fontsize-spinner))
                           (vertical-margin-value (value vertical-margin-spinner))
                           (fontshadow-value (value fontshadow-spinner))
                           (bold-value (checked-value bold-checkbox))
                           (italic-value (checked-value italic-checkbox))
                           (border-style-object (find-border-style (selected-item border-style-dropdown)))
                           (video-size-object (find-video-size (selected-item video-size-dropdown)))
                           (color-value (destructuring-bind (r g b a)
                                            (rgba (foreground-color open-color-button))
                                          (claraoke:rgb r g b (- 255 a))))
                           (res-x 1280)
                           (res-y 720))
                       (when (typep video-size-object 'video-size)
                         (setf res-x (x video-size-object) res-y (y video-size-object)))
                       (with-open-file (stream output :direction :output
                                                      :if-exists :supersede
                                                      :if-does-not-exist :create)
                         (sanverter:print-subass
                          (sanverter:parse-subrip
                           input
                           :title title-text
                           :play-res-x res-x
                           :play-res-y res-y
                           :border-style (value border-style-object)
                           :back-colour (claraoke:rgb 0 0 0 153) ; default
                           :style-margin-v vertical-margin-value
                           :shadow fontshadow-value
                           :fontname fontname
                           :fontsize fontsize
                           :bold bold-value
                           :italic italic-value
                           :primary-colour color-value)
                          stream))))
                 (show-message frame
                               (format nil "Success converting file ~A to ~A"
                                       input output)
                               "Success"
                               :information)
                 (java:jcall "setText" file-label "-")
                 (enable-components
                  :false
                  title-field fontname-dropdown fontsize-spinner
                  open-color-button bold-checkbox italic-checkbox
                  video-size-dropdown border-style-dropdown
                  vertical-margin-spinner fontshadow-spinner)
                 (enable convert-file-button :false)
                 (pack frame)))))))
      ;; Quit application
      (add-action-listener
       quit-button
       (lambda (e)
         (declare (ignore e))
         (java:jcall "dispose" frame)))
      ;; Change font
      (add-item-listener
       fontname-dropdown
       (lambda (e)
         (when (selected-event e :selected)
           (change-font))))
      ;; Change font size
      (add-change-listener
       fontsize-spinner
       (lambda (e)
         (declare (ignore e))
         (change-font)))
      ;; Change bold
      (add-action-listener
       bold-checkbox
       (lambda (e)
         (declare (ignore e))
         (change-font)))
      ;; Change italic
      (add-action-listener
       italic-checkbox
       (lambda (e)
         (declare (ignore e))
         (change-font)))
      ;; Grid Bag Layout
      (add-component-constraints pane icon-label 0 1 0 3)
      (add-component-constraints pane (make-label "Open file to be converted:" :right) 0 1 2 3)
      (add-component-constraints pane open-file-button 0 1 5 1)
      (add-component-constraints pane (make-label "Input") 1 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 1 1 1 1)
      (add-component-constraints pane file-label 1 1 2 3)
      (add-component-constraints pane (make-label "Title") 2 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 2 1 1 1)
      (add-component-constraints pane title-field 2 1 2 4)
      (add-component-constraints pane (make-label "Video size & Vertical margin") 3 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 3 1 1 1)
      (add-component-constraints pane video-size-dropdown 3 1 2 3)
      (add-component-constraints pane vertical-margin-spinner 3 1 5 1)
      (add-component-constraints pane (make-label "Border style & Shadow") 4 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 4 1 1 1)
      (add-component-constraints pane border-style-dropdown 4 1 2 3)
      (add-component-constraints pane fontshadow-spinner 4 1 5 1)
      (add-component-constraints pane (make-label "Font name & size") 5 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 5 1 1 1)
      (add-component-constraints pane fontname-dropdown 5 1 2 3)
      (add-component-constraints pane fontsize-spinner 5 1 5 1)
      (add-component-constraints pane (make-label "Bold") 6 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 6 1 1 1)
      (add-component-constraints pane bold-checkbox 6 1 2 4)
      (add-component-constraints pane (make-label "Italic") 7 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 7 1 1 1)
      (add-component-constraints pane italic-checkbox 7 1 2 4)
      (add-component-constraints pane (make-label "Global color") 8 1 0 1)
      (add-component-constraints pane (make-label ":" :right) 8 1 1 1)
      (add-component-constraints pane open-color-button 8 1 2 4)
      (add-component-constraints pane convert-file-button 9 1 5 1)
      (add-component-constraints pane quit-button 10 1 5 1)
      ;; Do not forget to pack the size
      (pack frame))))

