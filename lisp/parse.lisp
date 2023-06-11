;;;; bcpl.lisp

(in-package #:bcpl)

(defun parse (f)
  (loop for token = (lex f) until (eq token :eof) collect token))
