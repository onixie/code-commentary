;;;; quicklisp.asd

(asdf:defsystem #:quicklisp
  :description "The Quicklisp client application."
  :author "Zach Beane <zach@quicklisp.org>"
  :license "BSD-style"
  :serial t
  :version "2011051901"
  :components ((:file "package")
               (:file "utils")
               (:file "config")
               (:file "impl")
               (:file "impl-util")
               (:file "network")
               (:file "progress")
               (:file "http")
               (:file "deflate")
               (:file "minitar")
               (:file "dist")
               (:file "setup")
               (:file "client")
               (:file "client-update")
               (:file "dist-update")
               (:file "misc")))
