(in-package #:nontrivial-gray-streams/test)

(defclass binary-input-stream-a
    (ngray:fundamental-binary-input-stream binary-input-mixin)
  ())

(defmethod ngray:stream-read-byte ((stream binary-input-stream-a))
  (record-invocation :stream-read-byte stream)
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (elt input-value input-index)
          (incf input-index))
        :eof)))

(defmethod ngray:stream-listen ((stream binary-input-stream-a))
  (record-invocation :stream-listen stream)
  (< (input-index stream) (length (input-value stream))))

(defmethod ngray:stream-element-type ((stream binary-input-stream-a))
  (record-invocation :stream-element-type stream)
  '(unsigned-byte 8))

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-input-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-input-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-input-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream binary-input-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test binary-input-a)

#+gray-streams-file-length
(define-test binary-input-a.file-length.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
      (fail (file-length stream) 'type-error))))

(define-test binary-input-a.read-byte.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
      (is equal 10 (read-byte stream))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input-a.listen.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
      (true (listen stream))
      (true (invoked-p :stream-listen stream)))))

(define-test binary-input-a.clear-input.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
      (false (clear-input stream)))))

(define-test binary-input-a.streamp.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test binary-input-a.input-stream-p.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test binary-input-a.output-stream-p.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a)))
      (false (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test binary-input-a.interactive-stream-p.01
    :parent binary-input-a
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
