(in-package :swing-test)

(defvar *frame*)

(defun create-and-show-gui ()
  (setf *frame* (new 'JFrame "HelloWorldSwing"))
  (let* ((label (new 'JLabel "Hello World")))
    (~> *frame*
        (#"getContentPane")
        (#"add" label))
    (#"pack" *frame*)
    (show-gui)))

(defun show-gui ()
  (#"setVisible" *frame* t))

(defun hide-gui ()
  (#"setVisible" *frame* nil))

(defun runnable (f)
  (jmake-proxy (find-java-class 'Runnable)
               (lambda (lt p)
                 (declare (ignore lt p))
                 (funcall f))))

(defun main ()
  (#"invokeLater" 'SwingUtilities
                  (runnable 'create-and-show-gui)))
