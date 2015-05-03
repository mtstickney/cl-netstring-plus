;;;; package.lisp

(defpackage #:cl-netstring+
  (:use #:cl)
  (:nicknames #:nsp)
  (:export #:write-netstring-bytes
           #:write-netstring-bytes*
           #:make-decoder-state
           #:next-read-size
           #:pump-data
           #:read-netstring-data
           #:netstring-data
           #:netstring-bytes
           #:netstring-bytes*
           #:data
           #:str))
