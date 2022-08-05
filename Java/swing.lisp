(cl:in-package #:sanverter-java.swing)

;;; java.awt.*
;;; javax.swing.*
;;;
;;; GridBagLayout
;;;
;;; Closure
;;;
(let* (;; classes
       (jframe (java:jclass "javax.swing.JFrame"))
       (jtext (java:jclass "javax.swing.JTextField"))
       (jlabel (java:jclass "javax.swing.JLabel"))
       (jimage (java:jclass "javax.swing.ImageIcon"))
       (jcheck (java:jclass "javax.swing.JCheckBox"))
       (jcombo (java:jclass "javax.swing.JComboBox"))
       (jbutton (java:jclass "javax.swing.JButton"))
       (joption (java:jclass "javax.swing.JOptionPane"))
       (jfchooser (java:jclass "javax.swing.JFileChooser"))
       (jcchooser (java:jclass "javax.swing.JColorChooser"))
       (jspinner (java:jclass "javax.swing.JSpinner"))
       (spinner-number-model (java:jclass "javax.swing.SpinnerNumberModel"))
       (border-factory (java:jclass "javax.swing.BorderFactory"))
       (gb-layout (java:jclass "java.awt.GridBagLayout"))
       (gb-constraints (java:jclass "java.awt.GridBagConstraints"))
       (insets (java:jclass "java.awt.Insets"))
       (color (java:jclass "java.awt.Color"))
       (font (java:jclass "java.awt.Font"))
       (ievent (java:jclass "java.awt.event.ItemEvent"))
       (fchooser-filter (java:jclass "javax.swing.filechooser.FileNameExtensionFilter"))
       (graphics-environment (java:jclass "java.awt.GraphicsEnvironment"))
       ;; fields
       (jframe->do-nothing (java:jfield jframe "DO_NOTHING_ON_CLOSE"))
       (jframe->dispose (java:jfield jframe "DISPOSE_ON_CLOSE"))
       (jframe->hide (java:jfield jframe "HIDE_ON_CLOSE"))
       (jframe->exit (java:jfield jframe "EXIT_ON_CLOSE"))
       (jfchooser->approve (java:jfield jfchooser "APPROVE_OPTION"))
       (gb-constraints->both (java:jfield gb-constraints "BOTH"))
       (gb-constraints->center (java:jfield gb-constraints "CENTER"))
       (jlabel->left (java:jfield jlabel "LEFT"))
       (jlabel->center (java:jfield jlabel "CENTER"))
       (jlabel->right (java:jfield jlabel "RIGHT"))
       (message->error (java:jfield joption "ERROR_MESSAGE"))
       (message->information (java:jfield joption "INFORMATION_MESSAGE"))
       (message->warning (java:jfield joption "WARNING_MESSAGE"))
       (message->question (java:jfield joption "QUESTION_MESSAGE"))
       (message->plain (java:jfield joption "PLAIN_MESSAGE"))
       (message->yes-no (java:jfield joption "YES_NO_OPTION"))
       (message->yes-no-cancel (java:jfield joption "YES_NO_CANCEL_OPTION"))
       (message->yes (java:jfield joption "YES_OPTION"))
       (message->no (java:jfield joption "NO_OPTION"))
       (ievent->selected (java:jfield ievent "SELECTED"))
       (ievent->deselected (java:jfield ievent "DESELECTED"))
       (font->plain (java:jfield font "PLAIN"))
       (font->bold (java:jfield font "BOLD"))
       (font->italic (java:jfield font "ITALIC"))
       (font->bold-italic (logxor font->bold font->italic)))

  (defun make-frame (&optional (title "Untitled"))
    (java:jnew jframe title))

  (defun make-button (&optional (text "Button"))
    (java:jnew jbutton text))

  (defun make-label (&optional (text "Label") (alignment :left))
    (java:jnew jlabel text (ecase alignment
                             (:left jlabel->left)
                             (:center jlabel->center)
                             (:right jlabel->right))))

  (defun make-image-label (image &optional (text "Label") (alignment :center))
    (let ((label (make-label text alignment))
          (icon (java:jnew jimage image)))
      (java:jcall "setIcon" label icon) label))

  (defun make-text-field (&optional (text ""))
    (java:jnew jtext text))

  (defun make-check (&optional (text ""))
    (java:jnew jcheck text))

  (defun make-combo (names)
    (java:jnew jcombo names))

  (defun make-gb-layout ()
    (java:jnew gb-layout))

  (defun make-gb-constraints (gx gy gw gh)
    (java:jnew gb-constraints
               gx gy
               gw gh
               1.0 1.0
               gb-constraints->center
               gb-constraints->both
               (java:jnew insets 10 10 10 10)
               0 0))

  (defun make-titled-border (title)
    (java:jstatic "createTitledBorder" border-factory title))

  (defun make-fchooser ()
    (java:jnew jfchooser))

  (defun make-fchooser-filter (description &rest extensions)
    (java:jnew fchooser-filter description
               (java:jarray-from-list extensions)))

  (defun close-operation (frame operation)
    (java:jcall "setDefaultCloseOperation" frame
                (ecase operation
                  (:do-nothing jframe->do-nothing)
                  (:dispose jframe->dispose)
                  (:hide jframe->hide)
                  (:exit jframe->exit))))

  (defun visible (frame bool)
    (java:jcall "setVisible" frame
                (ecase bool
                  (:true java:+true+)
                  (:false java:+false+))))

  (defun pack (frame)
    (java:jcall "pack" frame))

  (defun layout (frame layout)
    (java:jcall "setLayout" frame layout))

  (defun titled-border (panel title)
    (let ((border (make-titled-border title)))
      (java:jcall "setBorder" panel border)))

  (defun content-pane (frame)
    (java:jcall "getContentPane" frame))

  (defun make-action-listener (performed-function)
    (java:jinterface-implementation
     "java.awt.event.ActionListener"
     "actionPerformed"
     (lambda (e)
       (handler-case (funcall performed-function e)
         (java:java-exception (c)
           (format t "~40A~%~40A~%" "handle-java-error:" c)
           (force-output))))))

  (defun add-action-listener (button function)
    (java:jcall "addActionListener" button
                (make-action-listener function)))

  (defun make-item-listener (item-function)
    (java:jinterface-implementation
     "java.awt.event.ItemListener"
     "itemStateChanged"
     (lambda (e)
       (handler-case (funcall item-function e)
         (java:java-exception (c)
           (format t "~40A~%~40A~%" "handle-java-error:" c)
           (force-output))))))

  (defun add-item-listener (component function)
    (java:jcall "addItemListener" component
                (make-item-listener function)))

  (defun make-change-listener (change-function)
    (java:jinterface-implementation
     "javax.swing.event.ChangeListener"
     "stateChanged"
     (lambda (e)
       (handler-case (funcall change-function e)
         (java:java-exception (c)
           (format t "~40A~%~40A~%" "handle-java-error:" c)
           (force-output))))))

  (defun add-change-listener (component function)
    (java:jcall "addChangeListener" component
                (make-change-listener function)))

  (defun enable (component bool)
    (java:jcall "setEnabled" component
                (ecase bool
                  (:true java:+true+)
                  (:false java:+false+))))

  (defun enable-components (bool &rest components)
    (mapc (lambda (c)
            (funcall 'enable c bool))
          components))

  (defun selected (component bool)
    (java:jcall "setSelected" component
                (ecase bool
                  (:true java:+true+)
                  (:false java:+false+))))

  (defun add-component-constraints (container component rows nrows cols ncols)
    (java:jcall "add" container component
                (make-gb-constraints cols rows ncols nrows)))

  (defun add-fchooser-filter (chooser fchooser-filter &optional all)
    (java:jcall "addChoosableFileFilter" chooser fchooser-filter)
    (unless (null all)
      (java:jcall "setAcceptAllFileFilterUsed" chooser
                  (ecase all
                    (:true java:+true+)
                    (:false java:+false+)))))

  (defun open-fchooser (chooser parent)
    (let ((result (java:jcall "showOpenDialog" chooser parent)))
      (when (equal result jfchooser->approve)
        (let ((file (java:chain chooser "getSelectedFile" "getAbsoluteFile" "toString")))
          (pathname file)))))

  (defun make-color (r g b &optional alpha)
    (java:jnew color r g b (or alpha 255)))

  (defun open-cchooser (component)
    (let* ((title (java:jcall "getText" component))
           (color (foreground-color component))
           (result (java:jstatic "showDialog" jcchooser
                                 component title color)))
      (unless (null result) result)))

  (defun foreground-color (component &optional c)
    (if (null c)
        (let* ((jrgb (java:jcall "getForeground" component))
               (r (java:jcall "getRed" jrgb))
               (g (java:jcall "getGreen" jrgb))
               (b (java:jcall "getBlue" jrgb))
               (a (java:jcall "getAlpha" jrgb)))
          (java:jnew color r g b a))
        (java:jcall "setForeground" component c)))

  (defun background-color (component &optional c)
    (if (null c)
        (let* ((jrgb (java:jcall "getBackground" component))
               (r (java:jcall "getRed" jrgb))
               (g (java:jcall "getGreen" jrgb))
               (b (java:jcall "getBlue" jrgb))
               (a (java:jcall "getAlpha" jrgb)))
          (java:jnew color r g b a))
        (java:jcall "setBackground" component c)))

  (defun rgba (color)
    (let ((r (java:jcall "getRed" color))
          (g (java:jcall "getGreen" color))
          (b (java:jcall "getBlue" color))
          (a (java:jcall "getAlpha" color)))
      (list r g b a)))

  (defun show-message (parent message title type)
    (java:jstatic "showMessageDialog" joption
                  parent message title
                  (ecase type
                    (:information message->information)
                    (:error message->error)
                    (:warning message->warning)
                    (:question message->question)
                    (:plain message->plain))))

  (defun show-option-dialog (parent message title option-type message-type)
    (java:jstatic "showOptionDialog" joption
                  parent message title
                  (ecase option-type
                    (:yes-no message->yes-no)
                    (:yes-no-cancel message->yes-no-cancel))
                  (ecase message-type
                    (:information message->information)
                    (:error message->error)
                    (:warning message->warning)
                    (:question message->question)
                    (:plain message->plain))
                  java:+null+
                  java:+null+
                  java:+null+))

  (defun checked-value (checkbox &optional checked unchecked)
    (if (java:jcall "isSelected" checkbox)
        (or checked -1)
        (or unchecked 0)))

  (defun dialog-return (value &optional (type :yes))
    (let ((expected (ecase type
                      (:yes message->yes)
                      (:no message->no))))
      (values (eql expected value) expected)))

  (defun local-graphics-environment ()
    (java:jstatic "getLocalGraphicsEnvironment"
                  graphics-environment))

  (defun selected-event (component &optional (state :selected))
    (= (java:jcall "getStateChange" component)
       (ecase state
         (:selected ievent->selected)
         (:deselected ievent->deselected))))

  (defun selected-item (component &optional item)
    (if (null item)
        (java:jcall "getSelectedItem" component)
        (java:jcall "setSelectedItem" component item)))

  (defun ensure-font-name (&optional name)
    (let ((names (font-names :vector)))
      (flet ((find-name (string)
               (find string names :test 'string-equal)))
        (or (find-name name)
            (find-name "Arial")
            (aref names 0)))))

  (defun font-names (&optional (type :vector))
    (ecase type
      (:vector
       (java:jcall
        "getAvailableFontFamilyNames"
        (local-graphics-environment)))
      (:raw
       (java:jcall-raw
        "getAvailableFontFamilyNames"
        (local-graphics-environment)))
      (:list
       (coerce
        (java:jcall
         "getAvailableFontFamilyNames"
         (local-graphics-environment))
        'list))))

  (defun make-font (name &optional size style)
    (let ((fontstyle (case style
                       (:bold font->bold)
                       (:italic font->italic)
                       ((:bold-italic :italic-bold) font->bold-italic)
                       (otherwise font->plain)))
          (fontsize (or size 12))
          (fontname (ensure-font-name name)))
      (java:jnew font fontname fontstyle fontsize)))

  (defun make-spinner-number (value &key (min 0) (max 100) (step 1))
    (let ((model (java:jnew spinner-number-model value min max step)))
      (java:jnew jspinner model)))

  (defgeneric value (component)
    (:method (component)
      (java:jcall "getValue" component)))
  ;;
  ;; End closure
  )

