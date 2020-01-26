(in-package :swing-test)

(defun create-and-show-gui ()
  (let* ((frame (new 'JFrame "HelloWorldSwing"))
         (label (new 'JLabel "Hello World")))
    (~> frame
        (#"getContentPane")
        (#"add" label))
    (#"pack" frame)
    (show-gui frame)
    frame))

(defun show-gui (frame)
  (#"setVisible" frame t)
  frame)

(defun hide-gui (frame)
  (#"setVisible" frame nil)
  frame)

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
