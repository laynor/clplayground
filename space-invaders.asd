;;;; space-invaders.asd

(asdf:defsystem #:space-invaders
  :description "Describe space-invaders here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:cl-opengl #:alexandria #:cl-arrows)
  :components ((:file "package")
               (:file "space-invaders")))
