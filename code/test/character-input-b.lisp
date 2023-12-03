(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-b
    (character-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream)
  ())

(define-test character-input-b)

(define-test character-input-b.read-char.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (is eql #\a (read-char stream nil))
    (is eql #\b (read-char stream nil))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "")))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "")))
    (eql :wibble (read-char stream nil :wibble))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.04
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "")))
    (fail (read-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.peek-char.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (is eql #\a (peek-char nil stream nil))
    (is eql #\a (read-char stream nil))
    (is eql #\b (read-char stream nil))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-peek-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.peek-char.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "")))
    (false (peek-char nil stream nil))))

(define-test character-input-b.listen.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (true (listen stream))))

(define-test character-input-b.listen.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "")))
    (false (listen stream))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
    (is eql 2 (read-sequence sequence stream))
    (is equalp sequence #(#\a #\b nil))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
              (invoked-p stream :stream-read-sequence stream sequence 0 3)
              (invoked-p stream :stream-read-sequence stream sequence nil nil)
              (invoked-p stream :stream-read-sequence stream sequence nil 3)))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
    (is eql 1 (read-sequence sequence stream :end 1))
    (is equalp sequence #(#\a nil nil))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
              (invoked-p stream :stream-read-sequence stream sequence nil 1)))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
    (is eql 3 (read-sequence sequence stream :start 1))
    (is equalp sequence #(nil #\a #\b))
    (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
              (invoked-p stream :stream-read-sequence stream sequence 1 3)))))

#+gray-streams-file-length
(define-test character-input-b.file-length.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (is eql (file-length stream) 2)
    (true (invoked-p stream :stream-file-length stream nil))))

#+gray-streams-file-position
(define-test character-input-b.file-position.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (is equal 0 (file-position stream))
    (is eql #\a (peek-char nil stream nil))
    (is equal 0 (file-position stream))
    (is eql #\a (read-char stream nil))
    (is equal 1 (file-position stream))
    (true (invoked-p stream :stream-file-position stream nil))))

#+gray-streams-file-position
(define-test character-input-b.file-position.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (true (file-position stream 1))
    (is equal 1 (file-position stream))
    (is eql #\b (read-char stream nil))
    (true (invoked-p stream :stream-file-position stream nil))))

#+gray-streams-interactive
(define-test character-input-b.interactive-stream-p.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :input-value "ab")))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))

#+gray-streams-pathname
(define-test character-input-b.pathname.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (pathname stream))
    (true (invoked-p stream :pathname stream))))

#+gray-streams-truename
(define-test character-input-b.pathname.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (truename stream))
    (true (invoked-p stream :truename stream))))
