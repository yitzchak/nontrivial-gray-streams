(in-package #:nontrivial-gray-streams/test)

(defclass binary-io-stream-a
    (binary-input-mixin-a
     binary-output-mixin-a
     ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream)
  ())

(define-test binary-io-a)

#+gray-streams-file-length
(define-test binary-io-a.file-length.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
    (fail (file-length stream) 'type-error)))

(define-test binary-io-a.read-byte.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
    (is equal 10 (read-byte stream))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-a.listen.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
    (true (listen stream))
    (true (invoked-p stream :stream-listen stream))))

(define-test binary-io-a.clear-input.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a :input-value #(10))))
    (false (clear-input stream))))

(define-test binary-io-a.write-byte.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (is equal 10 (write-byte 10 stream))
    (is equalp #(10) (output-value stream))
    (true (invoked-p stream :stream-write-byte stream 10))))

(define-test binary-io-a.finish-output.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (false (finish-output stream))))

(define-test binary-io-a.force-output.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (false (force-output stream))))

(define-test binary-io-a.clear-output.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (false (clear-output stream))))

(define-test binary-io-a.streamp.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

(define-test binary-io-a.input-stream-p.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (true (input-stream-p stream))
    #+gray-streams-input-stream-p
    (true (invoked-p stream :input-stream-p stream))))

(define-test binary-io-a.output-stream-p.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (true (output-stream-p stream))
    #+gray-streams-output-stream-p
    (true (invoked-p stream :output-stream-p stream))))

#+gray-streams-interactive-stream-p
(define-test binary-io-a.interactive-stream-p.01
  :parent binary-io-a
  (let ((stream (make-instance 'binary-io-stream-a)))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))
