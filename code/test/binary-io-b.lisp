(in-package #:nontrivial-gray-streams/test)

(defclass binary-io-stream-b
    (binary-input-mixin-b
     binary-output-mixin-b
     ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream
     #+ccl file-stream)
  ())

(define-test binary-io-b)

(define-test binary-io-b.read-byte.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (is eql 10 (read-byte stream nil))
    (is eql 11 (read-byte stream nil))
    (false (read-byte stream nil))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-b.read-byte.02
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
    (false (read-byte stream nil))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-b.read-byte.03
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
    (eql :wibble (read-byte stream nil :wibble))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-b.read-byte.04
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
    (fail (read-byte stream))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-b.clear-input.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10))))
    (false (clear-input stream))
    (fail (read-byte stream))
    (true (invoked-p stream :stream-clear-input stream))
    (true (invoked-p stream :stream-read-byte stream))))

(define-test binary-io-b.listen.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (true (listen stream))))

(define-test binary-io-b.listen.02
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
    (false (listen stream))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
        (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
    (is eql 2 (read-sequence sequence stream))
    (is equalp sequence #(10 11 nil))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
              (invoked-p stream :stream-read-sequence stream sequence 0 3)
              (invoked-p stream :stream-read-sequence stream sequence nil nil)
              (invoked-p stream :stream-read-sequence stream sequence nil 3)))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.02
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
        (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
    (is eql 1 (read-sequence sequence stream :end 1))
    (is equalp sequence #(10 nil nil))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
              (invoked-p stream :stream-read-sequence stream sequence nil 1)))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.03
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
        (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
    (is eql 3 (read-sequence sequence stream :start 1))
    (is equalp sequence #(nil 10 11))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
              (invoked-p stream :stream-read-sequence stream sequence 1 3)))))

#+gray-streams-file-length
(define-test binary-io-b.file-length.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (is eql (file-length stream) 2)
    (true (invoked-p stream :stream-file-length stream nil))))

#+gray-streams-file-position
(define-test binary-io-b.file-position.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (is equal 0 (file-position stream))
    (is eql 10 (read-byte stream nil))
    (is equal 1 (file-position stream))
    (true (invoked-p stream :stream-file-position stream nil))))

#+gray-streams-file-position
(define-test binary-io-b.file-position.02
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (true (file-position stream 1))
    (is equal 1 (file-position stream))
    (is eql 11 (read-byte stream nil))
    (true (invoked-p stream :stream-file-position stream nil))))

#+gray-streams-interactive
(define-test binary-io-b.interactive-stream-p.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))

(define-test binary-io-b.write-byte.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b)))
    (is eql 10 (write-byte 10 stream))
    (is eql 11 (write-byte 11 stream))
    (is equalp #(10 11) (output-value stream))
    (true (invoked-p stream :stream-write-byte stream 10))
    (true (invoked-p stream :stream-write-byte stream 11))))

#+gray-streams-sequence
(define-test binary-io-b.write-sequence.01
  :parent binary-io-b
  (let ((stream (make-instance 'binary-io-stream-b)))
    (write-sequence #(10 11 12 13) stream)
    (write-sequence #(14 15 16 17) stream :start 1 :end 3)
    (is equalp #(10 11 12 13 15 16) (output-value stream))
    (true (or (invoked-p stream :stream-write-sequence stream #(10 11 12 13) nil nil)
              (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 nil)
              (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 4)))
    (true (invoked-p stream :stream-write-sequence stream #(14 15 16 17) 1 3))))
