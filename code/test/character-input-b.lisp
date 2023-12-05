(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-b
    (character-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream)
  ())

(define-test character-input-b)

(define-test character-input-b.clear-input.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a")))
    (false (clear-input stream))
    (true (invoked-p stream :stream-clear-input stream))))

(define-test character-input-b.interactive-stream-p.01
  :parent character-input-b
  (skip-on
   ((not :gray-streams-interactive))
   "Interactive extension not present"
   (let ((stream (make-instance 'character-input-stream-b
                                :input-value "ab")))
     (false (interactive-stream-p stream))
     (true (invoked-p stream :interactive-stream-p stream)))))

(define-test character-input-b.interactive-stream-p.02
  :parent character-input-b
  (skip-on
   ((not :gray-streams-interactive))
   "Interactive extension not present"
   (let ((stream (make-instance 'character-input-stream-b
                                :input-value "ab"
                                :interactive t)))
     (true (interactive-stream-p stream))
     (true (invoked-p stream :interactive-stream-p stream)))))

(define-test character-input-b.input-stream-p.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (true (input-stream-p stream))
    (skip-on
     ((not :gray-streams-input-stream-p))
     "INPUT-STREAM-P extension not present"
     (true (invoked-p stream :input-stream-p stream)))))

#+gray-streams-file-length
(define-test character-input-b.file-length.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is eql (file-length stream) 2)
    (true (invoked-p stream :stream-file-length stream nil))))

#+gray-streams-file-position
(define-test character-input-b.file-position.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is equal 0 (file-position stream))
    (is eql #\a (peek-char nil stream nil))
    (is equal 0 (file-position stream))
    (is eql #\a (read-char stream nil))
    (is equal 1 (file-position stream))
    (true (invoked-p stream :stream-file-position stream nil))))

#+gray-streams-file-position
(define-test character-input-b.file-position.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (true (file-position stream 1))
    (is equal 1 (file-position stream))
    (is eql #\b (read-char stream nil))
    (true (invoked-p stream :stream-file-position stream nil))))

(define-test character-input-b.listen.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (true (listen stream))))

(define-test character-input-b.listen.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (false (listen stream))))

(define-test character-input-b.output-stream-p.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (false (output-stream-p stream))
    (skip-on
     ((not :gray-streams-output-stream-p))
     "OUTPUT-STREAM-P extension not present"
     (true (invoked-p stream :output-stream-p stream)))))

#+gray-streams-pathname
(define-test character-input-b.pathname.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (pathname stream))
    (true (invoked-p stream :pathname stream))))

#+gray-streams-truename
(define-test character-input-b.pathname.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :pathname #P"fu.bar")))
    (is equalp #P"fu.bar" (truename stream))
    (true (invoked-p stream :truename stream))))

(define-test character-input-b.peek-char.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is eql #\a (peek-char nil stream nil))
    (is eql #\a (read-char stream nil))
    (is eql #\b (read-char stream nil))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-peek-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.peek-char.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (fail (peek-char nil stream))
    (true (invoked-p stream :stream-peek-char stream))))

(define-test character-input-b.peek-char.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (is eql :wibble (peek-char nil stream nil :wibble))
    (true (invoked-p stream :stream-peek-char stream))))

(define-test character-input-b.peek-char.04
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is equal #\b (peek-char #\b stream))
    (print (invocations stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.peek-char.05
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a")))
    (fail (peek-char #\b stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.peek-char.06
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a")))
    (is equal :wibble (peek-char #\b stream nil :wibble))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is eql #\a (read-char stream nil))
    (is eql #\b (read-char stream nil))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (eql :wibble (read-char stream nil :wibble))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char.04
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (fail (read-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-b.read-char-no-hang.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a")))
    (is equal #\a (read-char-no-hang stream))
    (true (invoked-p stream :stream-read-char-no-hang stream))))

(define-test character-input-b.read-line.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a")))
    (is-values (read-line stream)
               (equal "a")
               (eql t))
    (true (invoked-p stream :stream-read-line stream))))

(define-test character-input-b.read-line.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "a
b")))
    (is-values (read-line stream)
               (equal "a")
               (eql nil))
    (true (invoked-p stream :stream-read-line stream))))

(define-test character-input-b.read-line.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (fail (read-line stream))
    (true (invoked-p stream :stream-read-line stream))))

(define-test character-input-b.read-line.04
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (is-values (read-line stream nil :wibble)
               (eql :wibble)
               (eql t))
    (true (invoked-p stream :stream-read-line stream))))

(define-test character-input-b.read-sequence.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab"))
        (sequence (make-array 3 :element-type 'character
                                :initial-element #\Space)))
    (is eql 2 (read-sequence sequence stream))
    (is equalp sequence "ab ")
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
             (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
                       (invoked-p stream :stream-read-sequence stream sequence 0 3)
                       (invoked-p stream :stream-read-sequence stream sequence nil nil)
                       (invoked-p stream :stream-read-sequence stream sequence nil 3))))))

(define-test character-input-b.read-sequence.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab"))
        (sequence (make-array 3 :element-type 'character
                                :initial-element #\Space)))
    (is eql 1 (read-sequence sequence stream :end 1))
    (is equalp sequence "a  ")
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
    (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
              (invoked-p stream :stream-read-sequence stream sequence nil 1))))))

(define-test character-input-b.read-sequence.03
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab"))
        (sequence (make-array 3 :element-type 'character
                                :initial-element #\Space)))
    (is eql 3 (read-sequence sequence stream :start 1))
    (is equalp sequence " ab")
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
             (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
                       (invoked-p stream :stream-read-sequence stream sequence 1 3))))))

(define-test character-input-b.stream-element-type.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (is equal 'character (stream-element-type stream))
    (true (invoked-p stream :stream-element-type stream))))

(define-test character-input-b.streamp.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

(define-test character-input-b.unread-char.01
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is equal #\a (read-char stream))
    (false (unread-char #\a stream))
    (is equal #\a (read-char stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\a))))

(define-test character-input-b.unread-char.02
  :parent character-input-b
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "ab")))
    (is equal #\a (read-char stream))
    (fail (unread-char #\b stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\b))))
