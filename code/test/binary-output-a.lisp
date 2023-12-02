(in-package #:nontrivial-gray-streams/test)

(defclass binary-output-stream-a
    (ngray:fundamental-binary-output-stream)
  ((value :accessor value
          :initform (make-array 16 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0))))

(defmethod ngray:stream-write-byte ((stream binary-output-stream-a) byte)
  (record-invocation :stream-write-byte stream byte)
  (vector-push-extend byte (value stream))
  byte)

(defmethod ngray:stream-line-column ((stream binary-output-stream-a))
  (record-invocation :stream-line-column stream)
  nil)

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-output-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-output-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-output-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream binary-output-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test binary-output-a)

(define-test binary-output-a.write-byte.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (is equal 10 (write-byte 10 stream))
      (is equalp #(10) (value stream))
      (true (invoked-p :stream-write-byte stream 10)))))

(define-test binary-output-a.finish-output.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (false (finish-output stream)))))

(define-test binary-output-a.force-output.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (false (force-output stream)))))

(define-test binary-output-a.clear-output.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (false (clear-output stream)))))

(define-test binary-output-a.streamp.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test binary-output-a.input-stream-p.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (false (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test binary-output-a.output-stream-p.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (true (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test binary-output-a.interactive-stream-p.01
    :parent binary-output-a
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
