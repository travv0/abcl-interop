(in-package :swing-test)

(defun create-and-show-gui ()
  (let ((frame (make-instance 'frame
                              :title "Celsius Converter"
                              :layout :vertical
                              :panels (list
                                       (make-instance 'panel
                                                      :components (list
                                                                   *text-field*
                                                                   (make-instance 'label
                                                                                  :text "Celsius")))
                                       (make-instance 'panel
                                                      :components (list
                                                                   (make-instance 'button
                                                                                  :text "Convert"
                                                                                  :click-handler 'convert-celsius)
                                                                   *label*))))))
    (display frame)))

(defun convert-celsius ()
  (let ((f (+ (* (parse-integer (text-field-text *text-field*) :junk-allowed t)
                 1.8)
              32)))
    (setf (label-text *label*) (format nil "~a Fahrenheit" (round f)))))

(defclass component ()
  ((jcomponent :reader jcomponent)
   (parent :reader component-parent)))

(defclass text-field (component)
  ((text :initarg :text)))

(defmethod initialize-instance :after ((text-field text-field) &key)
  (with-slots (jcomponent text) text-field
    (setf jcomponent (new 'JTextField text))))

(defmethod text-field-text ((text-field text-field))
  (with-slots (jcomponent) text-field
    (#"getText" jcomponent)))

(defparameter *text-field* (make-instance 'text-field
                                          :text "0"))

(defclass button (component)
  ((text :reader button-text :initarg :text)
   (click-handler :reader button-click-handler :initarg :click-handler)))

(defmethod initialize-instance :after ((button button) &key)
  (with-slots (jcomponent text click-handler) button
    (setf jcomponent (new 'JButton text))
    (when click-handler
      (#"addActionListener" jcomponent (action-listener (lambda (event)
                                                          (declare (ignore event))
                                                          (funcall click-handler)))))))

(defclass label (component)
  ((text :reader label-text :initarg :text)))

(defmethod initialize-instance :after ((label label) &key)
  (with-slots (jcomponent text) label
    (setf jcomponent (new 'JLabel text))))

(defmethod (setf label-text) (new-text (label label))
  (with-slots (jcomponent text) label
    (#"setText" jcomponent new-text)
    (setf text new-text)))

(defparameter *label* (make-instance 'label :text "Fahrenheit"))

(defclass frame ()
  ((jframe :reader jframe)
   (title :reader frame-title :initarg :title)
   (layout :reader frame-layout :initarg :layout :initform :horizontal)
   (panels :reader frame-panels :initarg :panels :initform '())))

(defmethod initialize-instance :after ((frame frame) &key)
  (with-slots (jframe title panels layout) frame
    (setf jframe (new 'JFrame title))
    (ecase layout
      (:vertical (#"setLayout" jframe (new 'GridLayout (length panels) 1)))
      (:horizontal (#"setLayout" jframe (new 'GridLayout 1 (length panels)))))
    (dolist (panel panels)
      (setf (slot-value panel 'parent) frame)
      (-> jframe (#"getContentPane") (#"add" (jpanel panel))))))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type t :identity t)
    (format stream "~a" (frame-title frame))))

(defmethod show-message ((frame null) format-string &rest format-args)
  (declare (ignore frame))
  (#"showMessageDialog" 'JOptionPane +null+ (apply #'format nil format-string format-args)))

(defmethod show-message ((frame frame) format-string &rest format-args)
  (#"showMessageDialog" 'JOptionPane (jframe frame) (apply #'format nil format-string format-args))
  frame)

(defmethod display ((frame frame) &optional (visible t))
  (#"pack" (jframe frame))
  (#"setVisible" (jframe frame) visible)
  frame)

(defclass panel ()
  ((jpanel :reader jpanel)
   (parent :reader panel-parent)
   (layout :reader panel-layout :initarg :layout :initform :horizontal)
   (panels :initarg :panels :reader panel-panels :initform '())
   (components :initarg :components :reader panel-components :initform '())))

(defmethod initialize-instance :after ((panel panel) &key)
  (with-slots (jpanel components panels layout) panel
    (setf jpanel (new 'JPanel
                      (case layout
                        (:vertical (new 'GridLayout (length components) 1))
                        (:horizontal (new 'GridLayout 1 (length components))))))
    (dolist (sub-panel panels)
      (setf (slot-value sub-panel 'parent) panel)
      (add panel sub-panel))
    (dolist (component components)
      (setf (slot-value component 'parent) panel)
      (add panel component))))

(defmethod add ((panel panel) (component component))
  (-> (jpanel panel) (#"add" (jcomponent component)))
  panel)

(defmethod add ((panel panel) (component java-object))
  (-> (jpanel panel) (#"add" component))
  panel)

(defmethod add ((panel panel) (sub-panel panel))
  (-> (jpanel panel) (#"add" (jpanel sub-panel)))
  panel)

(defun runnable (f)
  (jmake-proxy (find-java-class 'Runnable)
               (lambda (lt p)
                 (declare (ignore lt p))
                 (funcall f))))

(defun action-listener (f)
  (jmake-proxy (find-java-class 'ActionListener)
               (lambda (m lt p)
                 (declare (ignore m lt))
                 (funcall f p))))

(defun invoke-later (f)
  (#"invokeLater" 'SwingUtilities
                  (runnable f)))

(defun main ()
  (invoke-later 'create-and-show-gui))
