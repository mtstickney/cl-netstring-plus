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

(defun write-netstring-bytes (stream &rest datae)
  (write-netstring-bytes* stream datae))

(defun write-netstring-bytes* (stream datae)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format '(:utf-8 :eol-style :lf)))
        (len (reduce #'+ datae :key #'length)))
    (format stream "~X:" len)
    (map nil (lambda (data)
               (map nil (lambda (b)
                          (write-byte b stream))
                    data))
         datae)
    (write-char #\Linefeed stream)))

;;; Decoding

(defun ascii-hex-digit-p (code)
  "Return T if CODE is the ASCII value for a hexadecimal digit, NIL otherwise."
  (check-type code (unsigned-byte 8))
  (cond
    ((<= 48 code 57)
     (- code 48))
    ((<= 65 code 70)
     (+ (- code 65) 10))
    ((<= 97 code 102)
     (+ (- code 97) 10))
    (t nil)))

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

(defun pump-byte! (state byte)
  (check-type state decoder-state)
  (ecase (state state)
    ((:header :initial)
     ;; We're abusing the ASCII-UTF8 equivalence a bit here.
     (let* ((c (code-char byte))
            (n (ascii-hex-digit-p byte)))
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
     (add-data-byte! state byte)
     (when (= (length (msg-data state)) (size state))
       (transition-state! state :end-of-data)))
    (:end-of-data
     (let ((c (code-char byte)))
       (unless (eql c #\Linefeed)
         (error 'too-much-data :expected-size (size state) :found c))
       (transition-state! state :complete)))
    (:complete (error "Decoder ~S is complete, can't pump any more data." state))))

(defun pump-stream! (stream state &key count bytes)
  (let ((msgs '())
        (i 0)
        (byte-count 0))
    (handler-case
        (loop
           while (and (or (not bytes)
                          (<= byte-count bytes))
                      (or (not count)
                          (< i count)))
           do
           (loop until (eql (state state) :complete)
              do (let ((byte (read-byte stream)))
                   (pump-byte! state byte)
                   (incf byte-count))
              finally (progn
                        (push (msg-data state) msgs)
                        (incf i)))
           (setf state (make-decoder-state)))
      (end-of-file ()
        ;; Just trap it
        ))
    (values state (nreverse msgs))))

(defun pump-vector! (vector state &key (start 0) end count)
  (let ((msgs '())
        (i 0)
        (end (if end end (1- (length vector)))))
    ;; TODO: It would be faster to check the element-type before the
    ;; loop, but that might cause issues for vectors with valid elements
    ;; but no specific element-type.
    (loop while (and (<= start end)
                     (or (not count)
                         (< i count)))
       do (let ((byte (elt vector start)))
            (check-type byte (unsigned-byte 8))
            (pump-byte! state byte)
            (incf start)
            (when (eql (state state) :complete)
              (push (msg-data state) msgs)
              (incf i)
              (setf state (make-decoder-state)))))
    (values state (nreverse msgs) start)))

;;; TODO: Add support for a resynchronizing restart to dump data until
;;; newline on error
(defun read-netstring-data (stream)
  (let ((state (make-decoder-state)))
    (first (nth-value 0 (pump-stream! stream state :count 1)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun netstring-bytes (&rest data)
    (flexi-streams:with-output-to-sequence (byte-stream)
      (write-netstring-bytes* byte-stream data))))

(defun netstring-bytes* (datae)
  "Return a netstring encoding for the concatenation of DATAE."
  (flexi-streams:with-output-to-sequence (byte-stream)
    (write-netstring-bytes* byte-stream datae)))

(defun netstring-data (bytes)
  (let ((state (make-decoder-state)))
    (first (nth-value 1 (pump-vector! bytes state :count 1)))))

(defmacro data (sequence)
  (netstring-bytes sequence))

(defmacro str (string)
  `(data ,(trivial-utf-8:string-to-utf-8-bytes string)))
