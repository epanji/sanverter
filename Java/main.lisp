(cl:in-package #:sanverter-java)

;;; TODO fontname and fontsize chooser
(defun main ()
  (let* ((frame (make-frame "SANVERTER"))
         (pane (content-pane frame))
         (file-label (make-label "-" :left))
         (open-file-button (make-button "Open File"))
         (convert-file-button (make-button "Convert"))
         (quit-button (make-button "Quit"))
         (open-file-chooser (make-fchooser))
         (open-file-filter (make-fchooser-filter
                            "File srt / vtt / ass"
                            "srt" "vtt" "ass"))
         (title (make-text-field "Untitled"))
         (bold (make-check ""))
         (italic (make-check ""))
         (underline (make-check ""))
         (strikeout (make-check ""))
         (open-color-button (make-button "Click me!")))
    ;; Frame
    (layout frame (make-gb-layout))
    (titled-border pane "SANVERTER")
    (close-operation frame :dispose)
    (visible frame :true)
    ;; Components
    (enable-components :false
                       title open-color-button
                       bold italic underline strikeout)
    (selected bold :true)
    (foreground-color open-color-button (make-color 255 165 0))
    (background-color open-color-button (make-color 70 70 70))
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
           (when (or (string-equal (pathname-type file) "srt")
                     (string-equal (pathname-type file) "vtt"))
             (enable-components :true
                                title open-color-button
                                bold italic underline strikeout))
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
                   (let ((title-text (java:jcall "getText" title))
                         (bold-value (checked-value bold))
                         (italic-value (checked-value italic))
                         (underline-value (checked-value underline))
                         (strikeout-value (checked-value strikeout))
                         (color-value (destructuring-bind (r g b a)
                                          (rgba (foreground-color open-color-button))
                                        (claraoke:rgb r g b (- 255 a)))))
                     (with-open-file (stream output :direction :output
                                                    :if-exists :supersede
                                                    :if-does-not-exist :create)
                       (sanverter:print-subass
                        (sanverter:parse-subrip
                         input
                         :title title-text
                         :bold bold-value
                         :italic italic-value
                         :underline underline-value
                         :strikeout strikeout-value
                         :primary-colour color-value)
                        stream))))
               (show-message frame
                             (format nil "Success converting file ~A to ~A"
                                     input output)
                             "Success"
                             :information)
               (java:jcall "setText" file-label "-")
               (enable-components :false
                                  title open-color-button
                                  bold italic underline strikeout)
               (enable convert-file-button :false)
               (pack frame)))))))
    ;; Quit application
    (add-action-listener
     quit-button
     (lambda (e)
       (declare (ignore e))
       (java:jcall "dispose" frame)))
    ;; Grid Bag Layout
    (add-component-constrains pane (make-label "Open file to be converted:" :right) 0 1 2 1)
    (add-component-constrains pane open-file-button 0 1 3 1)
    (add-component-constrains pane (make-label "Input") 1 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 1 1 1 1)
    (add-component-constrains pane file-label 1 1 2 1)
    (add-component-constrains pane (make-label "Title") 2 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 2 1 1 1)
    (add-component-constrains pane title 2 1 2 1)
    (add-component-constrains pane (make-label "Bold") 3 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 3 1 1 1)
    (add-component-constrains pane bold 3 1 2 1)
    (add-component-constrains pane (make-label "Italic") 4 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 4 1 1 1)
    (add-component-constrains pane italic 4 1 2 1)
    (add-component-constrains pane (make-label "Underline") 5 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 5 1 1 1)
    (add-component-constrains pane underline 5 1 2 1)
    (add-component-constrains pane (make-label "Strikeout") 6 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 6 1 1 1)
    (add-component-constrains pane strikeout 6 1 2 1)
    (add-component-constrains pane (make-label "Global color") 7 1 0 1)
    (add-component-constrains pane (make-label ":" :right) 7 1 1 1)
    (add-component-constrains pane open-color-button 7 1 2 1)
    (add-component-constrains pane convert-file-button 8 1 3 1)
    (add-component-constrains pane quit-button 9 1 3 1)
    ;; Do not forget to pack the size
    (pack frame)))

