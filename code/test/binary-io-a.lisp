(in-package #:nontrivial-gray-streams/test)

(defclass binary-io-stream-a
    (ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream
     binary-input-mixin binary-output-mixin)
  ())

(defmethod ngray:stream-read-byte ((stream binary-io-stream-a))
  (record-invocation :stream-read-byte stream)
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (elt input-value input-index)
          (incf input-index))
        :eof)))

(defmethod ngray:stream-listen ((stream binary-io-stream-a))
  (record-invocation :stream-listen stream)
  (< (input-index stream) (length (input-value stream))))

(defmethod ngray:stream-element-type ((stream binary-io-stream-a))
  (record-invocation :stream-element-type stream)
  '(unsigned-byte 8))

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-io-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-io-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-io-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream binary-io-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(defmethod ngray:stream-write-byte ((stream binary-io-stream-a) byte)
  (record-invocation :stream-write-byte stream byte)
  (vector-push-extend byte (output-value stream))
  byte)

(defmethod ngray:stream-line-column ((stream binary-io-stream-a))
  (record-invocation :stream-line-column stream)
  nil)

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-io-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-io-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-io-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream binary-io-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test binary-io-a)

#+gray-streams-file-length
(define-test binary-io-a.file-length.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
      (fail (file-length stream) 'type-error))))

(define-test binary-io-a.read-byte.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
      (is equal 10 (read-byte stream))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-io-a.listen.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
      (true (listen stream))
      (true (invoked-p :stream-listen stream)))))

(define-test binary-io-a.clear-input.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
      (false (clear-input stream)))))

(define-test binary-io-a.write-byte.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (is equal 10 (write-byte 10 stream))
      (is equalp #(10) (output-value stream))
      (true (invoked-p :stream-write-byte stream 10)))))

(define-test binary-io-a.finish-output.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (false (finish-output stream)))))

(define-test binary-io-a.force-output.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (false (force-output stream)))))

(define-test binary-io-a.clear-output.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (false (clear-output stream)))))

(define-test binary-io-a.streamp.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test binary-io-a.input-stream-p.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test binary-io-a.output-stream-p.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (true (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test binary-io-a.interactive-stream-p.01
    :parent binary-io-a
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
