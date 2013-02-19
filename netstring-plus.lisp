;;;; netstring.lisp

(in-package #:cl-netstring+)

(define-condition netstring-parse-error (error)
  ()
  (:documentation "Generic condition representing all netstring parse errors."))

(define-condition empty-header (netstring-parse-error)
  ()
  (:documentation "Condition representing a netstring header with no size digits.")
  (:report (lambda (condition stream)
             (format stream "Netstring header is empty, expected hexadecimal digits."))))

(define-condition invalid-header-character (netstring-parse-error)
  ((invalid-char :initarg :char :reader invalid-char))
  (:documentation "Condition representing an invalid character in a netstring header.")
  (:report (lambda (condition stream)
             (format stream "Invalid character ~S in netstring header." (invalid-char condition)))))

(define-condition too-much-data (netstring-parse-error)
  ((expected-size :initarg :expected-size :reader expected-size)
   (found-char :initarg :found :reader :found-char))
  (:documentation "Condition representing a missing terminator after the data bytes.")
  (:report (lambda (condition stream)
             (format stream "Expected linefeed terminator after ~A bytes, got ~S."
                     (expected-size condition)
                     (found-char condition)))))

(defun write-netstring-bytes (stream data)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
    (format stream "~X:" (length data))
    (loop for e across data
       do (write-byte e stream))
    (write-char #\Linefeed stream)))

(defun is-hex-digit (c)
  (parse-integer (string c) :radix 16 :junk-allowed t))

;; TODO: this relies on =stream= having :external-format :utf-8
;; currently done in read-netstring-data, but eh....
(defun read-hex-header (stream)
  (let* (c
         (header-str
          (with-output-to-string
            (header-stream)
            (loop while (is-hex-digit (setf c (read-char stream)))
               do (write-char c header-stream)
               finally (unread-char c stream))))
         (len (parse-integer header-str :radix 16))
         (c nil))
    (setf c (read-char stream))
    (when (and (eql c #\:) (null len))
      (error 'empty-header))
    (when (not (eql c #\:))
      (error 'invalid-header-char :char c))
    len))

;;; TODO: Add support for a resynchronizing restart to dump data until
;;; newline on error
(defun read-netstring-data (stream)
  (let* ((fstream (flexi-streams:make-flexi-stream stream :external-format :utf-8))
         (len (read-hex-header fstream))
         (seq (make-array len :element-type '(unsigned-byte 8)
                          :fill-pointer 0))
         c)
    (loop for i from 1 to len
       do (vector-push (read-byte fstream) seq))
    (when (not (eql (setf c (read-char fstream))
                   #\Linefeed))
      (error 'too-much-data :expected-size len :found c))
    seq))

(defun netstring-bytes (data)
  (flexi-streams:with-output-to-sequence (byte-stream)
    (write-netstring-bytes byte-stream data)))

(defun netstring-data (bytes)
  (flexi-streams:with-input-from-sequence (byte-stream bytes)
    (read-netstring-data byte-stream)))

(defmacro data (sequence)
  (let* ((len (length sequence))
         (hex-header (trivial-utf-8:string-to-utf-8-bytes
                      (format nil "~X:" len))))
    (concatenate '(vector (unsigned-byte 8)) hex-header sequence)))

(defmacro str (string)
  `(data ,(trivial-utf-8:string-to-utf-8-bytes string)))
