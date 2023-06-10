;;;; bcpl.lisp

(in-package #:bcpl)

(defun compile-file (path)
  (with-open-file (f path)
    (parse f)))
