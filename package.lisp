;;;; package.lisp

(defpackage #:cl-netstring+
  (:use #:cl)
  (:export #:write-netstring-bytes
           #:read-netstring-data
           #:netstring-data
           #:netstring-bytes
           #:data
           #:str))
