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
   (found-char :initarg :found :reader found-char))
  (:documentation "Condition representing a missing terminator after the data bytes.")
  (:report (lambda (condition stream)
             (format stream "Expected linefeed terminator after ~A bytes, got ~S."
                     (expected-size condition)
                     (found-char condition)))))

(defun write-netstring-bytes (stream data)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format '(:utf-8 :eol-style :lf))))
    (format stream "~X:" (length data))
    (loop for e across data
       do (write-byte e stream))
    (write-char #\Linefeed stream)))

;;; Decoding

(defun hex-digit-p (c)
  (parse-integer (string c) :radix 16 :junk-allowed t))

(defclass decoder-state ()
  ((state :initarg :state :accessor state)
   (data-size :initarg :size :accessor size)
   (message-data :initarg :data :accessor msg-data))
  (:default-initargs
   :state :initial
    :size 0
    :data nil))

(defmethod print-object ((object decoder-state) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "State: ~S Data-size: ~A Message-data: ~A"
            (state object)
            (if (slot-boundp object 'data-size)
                (size object)
                "<unbound>")
            (if (slot-boundp object 'message-data)
                (format nil "~S" (msg-data object))
                "<unbound>"))))

(defun add-header-digit! (state n)
  (check-type state decoder-state)
  (check-type n (integer 0 15))
  (setf (size state)
        (+ (* (size state) 16)
           n)))

(defun add-data-byte! (state byte)
  (check-type state decoder-state)
  (check-type byte (unsigned-byte 8))
  (unless (msg-data state)
    (setf (msg-data state)
          (make-array (size state)
                      :element-type '(unsigned-byte 8)
                      :fill-pointer 0)))
  (vector-push byte (msg-data state)))

(defun transition-state! (state to-state)
  (check-type state decoder-state)
  (check-type to-state keyword)
  (flet ((assert-coming-from (state curstates)
           (unless (member (state state) curstates)
             (error "Transition from decoder state ~S to state ~S is not permissible."
                    (state state)
                    to-state))))
    (ecase to-state
      (:initial (error "Trying to transition to initial decoder state from state ~S"
                      (state state)))
      (:header (assert-coming-from state '(:initial)))
      (:data (assert-coming-from state '(:header)))
      (:end-of-data (assert-coming-from state '(:data :header)))
      (:complete (assert-coming-from state '(:end-of-data))))
    ;; If we haven't errored out by now, update the state
    (setf (state state) to-state)))

(defun make-decoder-state (&rest args &key state size data)
  (declare (ignore state size data))
  (apply #'make-instance 'decoder-state args))

(defun next-read-size (state)
  "Return the size of the next chunk of data that STATE expects as an efficiency hint."
  (ecase (state state)
    ((:header :initial) 1)
    (:data (- (size state) (length (msg-data state))))
    (:end-of-data 1)
    (:complete 0)))

(defun pump-element (stream state)
  (check-type state decoder-state)
  (ecase (state state)
    ((:header :initial)
     (let* ((c (read-char stream))
            (n (hex-digit-p c)))
       (cond
         ((eql c #\:) (cond
                        ;; If we've read an acceptable character,
                        ;; we'll have transitioned to :header.
                        ((eql (state state) :initial)
                         (error 'empty-header))
                        ((= (size state) 0)
                         (transition-state! state :end-of-data))
                        (t (transition-state! state :data))))
         ((null n) (error 'invalid-header-character :char c))
         (t (add-header-digit! state n)
            (when (eql (state state) :initial)
              (transition-state! state :header))))))
    (:data
     (let ((byte (read-byte stream)))
       (add-data-byte! state byte)
       (when (= (length (msg-data state)) (size state))
         (transition-state! state :end-of-data))))
    (:end-of-data
     (let ((c (read-char stream)))
       (unless (eql c #\Linefeed)
         (unread-char c stream)
         (error 'too-much-data :expected-size (size state) :found c))
       (transition-state! state :complete)))
    (:complete (error "Decoder ~S is complete, can't pump any more data." state))))

(defun pump-data (stream state)
  (let ((msgs '()))
    (handler-case
        (loop
           (loop until (eql (state state) :complete)
              do (pump-element stream state)
              finally (push (msg-data state) msgs))
           (setf state (make-decoder-state)))
      (end-of-file ()
        ;; Just trap it
        ))
    (values state (nreverse msgs))))

;; TODO: this relies on =stream= having :external-format :utf-8
;; currently done in read-netstring-data, but eh....
(defun read-hex-header (stream)
  (let* (c
         (header-str
          (with-output-to-string
            (header-stream)
            (loop while (hex-digit-p (setf c (read-char stream)))
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
  (let* ((fstream (flexi-streams:make-flexi-stream stream :external-format '(:utf-8 :eol-style :lf)))
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
    (concatenate '(vector (unsigned-byte 8))
                 hex-header
                 sequence
                 (trivial-utf-8:string-to-utf-8-bytes (format nil "~c" #\Linefeed)))))

(defmacro str (string)
  `(data ,(trivial-utf-8:string-to-utf-8-bytes string)))
