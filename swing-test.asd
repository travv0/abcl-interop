(require :abcl-contrib)
(asdf:defsystem #:swing-test
  :serial t
  :depends-on (:jss
               :alexandria
               :cl-arrows)
  :components ((:file "package")
               (:file "main")))
