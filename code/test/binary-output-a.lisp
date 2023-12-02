(in-package #:nontrivial-gray-streams/test)

(defclass binary-output-stream-a
    (binary-output-mixin-a
     ngray:fundamental-binary-output-stream)
  ())

(define-test binary-output-a)

(define-test binary-output-a.write-byte.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (is equal 10 (write-byte 10 stream))
    (is equalp #(10) (output-value stream))
    (true (invoked-p stream :stream-write-byte stream 10))))

(define-test binary-output-a.finish-output.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (false (finish-output stream))))

(define-test binary-output-a.force-output.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (false (force-output stream))))

(define-test binary-output-a.clear-output.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (false (clear-output stream))))

(define-test binary-output-a.streamp.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

(define-test binary-output-a.input-stream-p.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (false (input-stream-p stream))
    #+gray-streams-input-stream-p
    (true (invoked-p stream :input-stream-p stream))))

(define-test binary-output-a.output-stream-p.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (true (output-stream-p stream))
    #+gray-streams-output-stream-p
    (true (invoked-p stream :output-stream-p stream))))

#+gray-streams-interactive-stream-p
(define-test binary-output-a.interactive-stream-p.01
  :parent binary-output-a
  (let ((stream (make-instance 'binary-output-stream-a)))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))
