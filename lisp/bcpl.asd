;;;; bcpl.asd

(asdf:defsystem #:bcpl
  :description "BCPL compiler"
  :author "Lars Brinkhoff <lars@nocrew.org>"
  :license  "GPL3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "lex")
               (:file "parse")
               (:file "bcpl")))
