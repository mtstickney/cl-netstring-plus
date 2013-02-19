;;;; netstring.asd

(asdf:defsystem #:cl-netstring+
  :serial t
  :description "A simple library for sending and receiving messages with a netstring-like encoding"
  :author "Matthew Stickney <mtstickney@gmail.com>"
  :license "Specify license here"
  :depends-on (#:trivial-utf-8
               #:flexi-streams)
  :components ((:file "package")
               (:file "netstring-plus")))
