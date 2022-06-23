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
       (jcheck (java:jclass "javax.swing.JCheckBox"))
       (jbutton (java:jclass "javax.swing.JButton"))
       (joption (java:jclass "javax.swing.JOptionPane"))
       (jfchooser (java:jclass "javax.swing.JFileChooser"))
       (jcchooser (java:jclass "javax.swing.JColorChooser"))
       (border-factory (java:jclass "javax.swing.BorderFactory"))
       (gb-layout (java:jclass "java.awt.GridBagLayout"))
       (gb-constraints (java:jclass "java.awt.GridBagConstraints"))
       (insets (java:jclass "java.awt.Insets"))
       (color (java:jclass "java.awt.Color"))
       (fchooser-filter (java:jclass "javax.swing.filechooser.FileNameExtensionFilter"))
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
       (message->no (java:jfield joption "NO_OPTION")))

  (defun make-frame (&optional (title "Untitled"))
    (java:jnew jframe title))

  (defun make-button (&optional (text "Button"))
    (java:jnew jbutton text))

  (defun make-label (&optional (text "Label") (alignment :left))
    (java:jnew jlabel text (ecase alignment
                             (:left jlabel->left)
                             (:center jlabel->center)
                             (:right jlabel->right))))

  (defun make-text-field (&optional (text ""))
    (java:jnew jtext text))

  (defun make-check (&optional (text ""))
    (java:jnew jcheck text))

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

  (defun add-component-constrains (container component rows nrows cols ncols)
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
      (unless (java:jequal result java:+null+)
        result)))

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
      (values (eql expected value) expected))))

