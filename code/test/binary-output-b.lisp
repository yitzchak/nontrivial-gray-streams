(in-package #:nontrivial-gray-streams/test)

(defclass binary-output-stream-b
    (binary-output-mixin-b
     ngray:fundamental-binary-output-stream)
  ())

(define-test binary-output-b)

(define-test binary-output-b.write-byte.01
  :parent binary-output-b
  (let ((stream (make-instance 'binary-output-stream-b)))
    (is eql 10 (write-byte 10 stream))
    (is eql 11 (write-byte 11 stream))
    (is equalp #(10 11) (output-value stream))
    (true (invoked-p stream :stream-write-byte stream 10))
    (true (invoked-p stream :stream-write-byte stream 11))))

#+gray-streams-sequence
(define-test binary-output-b.write-sequence.01
  :parent binary-output-b
  (let ((stream (make-instance 'binary-output-stream-b)))
    (write-sequence #(10 11 12 13) stream)
    (write-sequence #(14 15 16 17) stream :start 1 :end 3)
    (is equalp #(10 11 12 13 15 16) (output-value stream))
    (true (or (invoked-p stream :stream-write-sequence stream #(10 11 12 13) nil nil)
              (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 nil)
              (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 4)))
    (true (invoked-p stream :stream-write-sequence stream #(14 15 16 17) 1 3))))

#+gray-streams-pathname
(define-test binary-output-b.pathname.01
  :parent binary-output-b
  (let ((stream (make-instance 'binary-output-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (pathname stream))
    (true (invoked-p stream :pathname stream))))

#+gray-streams-truename
(define-test binary-output-b.pathname.01
  :parent binary-output-b
  (let ((stream (make-instance 'binary-output-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (truename stream))
    (true (invoked-p stream :truename stream))))
