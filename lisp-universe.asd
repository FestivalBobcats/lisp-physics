;;;; lisp-universe.asd

(asdf:defsystem #:lisp-universe
  :serial t
  :description "Describe lisp-universe here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:rt)
  :components ((:file "package")
               (:file "lisp-universe")))

