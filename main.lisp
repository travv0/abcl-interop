(in-package :swing-test)

(defun create-and-show-gui ()
  (let ((frame (make-instance 'frame
                              :title "HelloWorldSwing"
                              :panels (list
                                       (make-instance 'panel
                                                      :layout :horizontal
                                                      :components (list
                                                                   (new 'JLabel "Hello World")
                                                                   (make-instance 'label
                                                                                  :text "Another Label")
                                                                   (make-instance 'label
                                                                                  :text "aaaaaaaaaaaaaaaaa")))
                                       (make-instance 'panel
                                                      :layout :vertical
                                                      :components (list
                                                                   (new 'JLabel "FFFFFFFF")
                                                                   (new 'JLabel "BBBBBBBBBB")))))))
    (display frame)))

(defclass component ()
  ((jcomponent :reader jcomponent)))

(defmethod initialize-instance :after ((component component) &key))

(defclass label (component)
  ((text :reader label-text :initarg :text)))

(defmethod initialize-instance :after ((label label) &key)
  (with-slots (jcomponent text) label
    (setf jcomponent (new 'JLabel text))))

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
      (~> jframe (#"getContentPane") (#"add" (jpanel panel))))))

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
      (add panel sub-panel))
    (dolist (component components)
      (add panel component))))

(defmethod add ((panel panel) (component component))
  (~> (jpanel panel) (#"add" (jcomponent component)))
  panel)

(defmethod add ((panel panel) (component java-object))
  (~> (jpanel panel) (#"add" component))
  panel)

(defmethod add ((panel panel) (sub-panel panel))
  (~> (jpanel panel) (#"add" (jpanel sub-panel)))
  panel)

(defun runnable (f)
  (jmake-proxy (find-java-class 'Runnable)
               (lambda (lt p)
                 (declare (ignore lt p))
                 (funcall f))))

(defun invoke-later (f)
  (#"invokeLater" 'SwingUtilities
                  (runnable f)))

(defun main ()
  (invoke-later 'create-and-show-gui))
