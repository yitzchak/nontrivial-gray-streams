(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-a
    (ngray:fundamental-character-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod ngray:stream-read-char ((stream character-input-stream-a))
  (record-invocation :stream-read-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod ngray:stream-unread-char ((stream character-input-stream-a) character)
  (record-invocation :stream-unread-char stream character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char value (decf index)))
      (error "Cannot unread a character that does not match."))
    nil))

#+gray-streams-streamp
(defmethod ngray:streamp ((stream character-input-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream character-input-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream character-input-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream character-input-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test character-input-a)

#+gray-streams-file-length
(define-test character-input-a.file-length.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (fail (file-length stream) 'type-error))))

(define-test character-input-a.read-char.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (is equal #\a (read-char stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-a.read-char-no-hang.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (is equal #\a (read-char-no-hang stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-a.peek-char.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (is equal #\a (peek-char nil stream))
      (true (invoked-p :stream-read-char stream))
      (true (invoked-p :stream-unread-char stream #\a)))))

(define-test character-input-a.listen.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (true (listen stream))
      (true (invoked-p :stream-read-char stream))
      (true (invoked-p :stream-unread-char stream #\a)))))

(define-test character-input-a.read-line.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (is-values (read-line stream nil)
                 (equal "a")
                 (eql t))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-a.clear-input.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a :value "a")))
      (false (clear-input stream)))))

(define-test character-input-a.streamp.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test character-input-a.input-stream-p.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test character-input-a.output-stream-p.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a)))
      (false (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test character-input-a.interactive-stream-p.01
    :parent character-input-a
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
