(in-package #:nontrivial-gray-streams/test)

(defclass character-output-stream-b
    (character-output-mixin-b
     ngray:fundamental-character-output-stream)
  ())

(define-test character-output-b)

(define-test character-output-b.write-char.01
  :parent character-output-b
  (let ((stream (make-instance 'character-output-stream-b)))
    (is eql #\a (write-char #\a stream))
    (is eql #\b (write-char #\b stream))
    (is equal "ab" (output-value stream))
    (true (invoked-p stream :stream-write-char stream #\a))
    (true (invoked-p stream :stream-write-char stream #\b))))

#+gray-streams-sequence
(define-test character-output-b.write-sequence.01
  :parent character-output-b
  (let ((stream (make-instance 'character-output-stream-b)))
    (write-sequence "abcd" stream)
    (write-sequence "efgh" stream :start 1 :end 3)
    (is equal "abcdfg" (output-value stream))
    (true (or (invoked-p stream :stream-write-sequence stream "abcd" nil nil)
              (invoked-p stream :stream-write-sequence stream "abcd" 0 nil)
              (invoked-p stream :stream-write-sequence stream "abcd" 0 4)))
    (true (invoked-p stream :stream-write-sequence stream "efgh" 1 3))))

#+gray-streams-line-length
(define-test character-output-b.line-length.01
  :parent character-output-b
  (let ((stream (make-instance 'character-output-stream-b))
        (*print-right-margin* nil)
        (*print-pretty* nil))
    (format stream "~<aaaa~:;bbbb~>")
    (true (invoked-p stream :stream-line-length stream))))

#+gray-streams-pathname
(define-test character-output-b.pathname.01
  :parent character-output-b
  (let ((stream (make-instance 'character-output-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (pathname stream))
    (true (invoked-p stream :pathname stream))))

#+gray-streams-truename
(define-test character-output-b.pathname.01
  :parent character-output-b
  (let ((stream (make-instance 'character-output-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (truename stream))
    (true (invoked-p stream :truename stream))))
