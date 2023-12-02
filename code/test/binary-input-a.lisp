(in-package #:nontrivial-gray-streams/test)

(defclass binary-input-stream-a
    (binary-input-mixin-a
     ngray:fundamental-binary-input-stream)
  ())

(define-test binary-input-a)

#+gray-streams-file-length
(define-test binary-input-a.file-length.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
    (fail (file-length stream) 'type-error)))

(define-test binary-input-a.read-byte.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
    (is equal 10 (read-byte stream))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-input-a.listen.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
    (true (listen stream))
    (true (invoked-p stream :stream-listen stream))))

(define-test binary-input-a.clear-input.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a :input-value #(10))))
    (false (clear-input stream))))

(define-test binary-input-a.streamp.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

(define-test binary-input-a.input-stream-p.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a)))
    (true (input-stream-p stream))
    #+gray-streams-input-stream-p
    (true (invoked-p stream :input-stream-p stream))))

(define-test binary-input-a.output-stream-p.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a)))
    (false (output-stream-p stream))
    #+gray-streams-output-stream-p
    (true (invoked-p stream :output-stream-p stream))))

#+gray-streams-interactive-stream-p
(define-test binary-input-a.interactive-stream-p.01
  :parent binary-input-a
  (let ((stream (make-instance 'binary-input-stream-a)))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))
