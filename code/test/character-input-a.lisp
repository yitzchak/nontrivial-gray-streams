(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-a
    (character-input-mixin-a
     ngray:fundamental-character-input-stream)
  ())

(define-test character-input-a)

(define-test character-input-a.clear-input.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (false (clear-input stream))
    (true (invoked-p stream :stream-clear-input stream))))

#+gray-streams-file-length
(define-test character-input-a.file-length.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (fail (file-length stream) 'type-error)))

(define-test character-input-a.input-stream-p.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (true (input-stream-p stream))
    (skip-on
     ((not :gray-streams-input-stream-p))
     "INPUT-STREAM-P extension not present"
     (true (invoked-p stream :input-stream-p stream)))))

(define-test character-input-a.interactive-stream-p.01
  :parent character-input-a
  (skip-on
   ((not :gray-streams-interactive))
   "Interactive extension not present"
   (let ((stream (make-instance 'character-input-stream-a
                                :input-value "ab")))
     (false (interactive-stream-p stream))
     (true (invoked-p stream :interactive-stream-p stream)))))

(define-test character-input-a.listen.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (true (listen stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\a))))

(define-test character-input-a.output-stream-p.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (false (output-stream-p stream))
    (skip-on
     ((not :gray-streams-output-stream-p))
     "OUTPUT-STREAM-P extension not present"
     (true (invoked-p stream :output-stream-p stream)))))

(define-test character-input-a.peek-char.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (is equal #\a (peek-char nil stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\a))))

(define-test character-input-a.read-char.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (is equal #\a (read-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-char.02
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "")))
    (false (read-char stream nil))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-char.03
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "")))
    (eql :wibble (read-char stream nil :wibble))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-char.04
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-b
                               :input-value "")))
    (fail (read-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-char-no-hang.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (is equal #\a (read-char-no-hang stream))
    (true (invoked-p stream :stream-read-char-no-hang stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-line.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "a")))
    (is-values (read-line stream nil)
               (equal "a")
               (eql t))
    (true (invoked-p stream :stream-read-line stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-sequence.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null)
                                :initial-element nil)))
    (is eql 2 (read-sequence sequence stream))
    (is equalp sequence #(#\a #\b nil))
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
             (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
                       (invoked-p stream :stream-read-sequence stream sequence 0 3)
                       (invoked-p stream :stream-read-sequence stream sequence nil nil)
                       (invoked-p stream :stream-read-sequence stream sequence nil 3))))))

(define-test character-input-a.read-sequence.02
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null)
                                :initial-element nil)))
    (is eql 1 (read-sequence sequence stream :end 1))
    (is equalp sequence #(#\a nil nil))
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
             (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
                       (invoked-p stream :stream-read-sequence stream sequence nil 1))))))

(define-test character-input-a.read-sequence.03
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a
                               :input-value "ab"))
        (sequence (make-array 3 :element-type '(or character null)
                                :initial-element nil)))
    (is eql 3 (read-sequence sequence stream :start 1))
    (is equalp sequence #(nil #\a #\b))
    (skip-on ((not :gray-streams-sequence))
             "Sequence extension not present"
             (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
                       (invoked-p stream :stream-read-sequence stream sequence 1 3))))))

(define-test character-input-a.streamp.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

