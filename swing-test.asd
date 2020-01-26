(require :abcl-contrib)
(asdf:defsystem #:swing-test
  :serial t
  :depends-on (:jss
               :serapeum)
  :components ((:file "package")
               (:file "main")))
