;;;; tetris.asd

(asdf:defsystem #:tetris
  :description "Describe tetris here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:alexandria)
  :components ((:file "package")
               (:file "colours")
               (:file "blocks")
               (:file "tetris")))
