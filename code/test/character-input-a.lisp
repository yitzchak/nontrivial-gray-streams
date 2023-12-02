(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-a
    (character-input-mixin-a
     ngray:fundamental-character-input-stream)
  ())

(define-test character-input-a)

#+gray-streams-file-length
(define-test character-input-a.file-length.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (fail (file-length stream) 'type-error)))

(define-test character-input-a.read-char.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (is equal #\a (read-char stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.read-char-no-hang.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (is equal #\a (read-char-no-hang stream))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.peek-char.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (is equal #\a (peek-char nil stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\a))))

(define-test character-input-a.listen.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (true (listen stream))
    (true (invoked-p stream :stream-read-char stream))
    (true (invoked-p stream :stream-unread-char stream #\a))))

(define-test character-input-a.read-line.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (is-values (read-line stream nil)
               (equal "a")
               (eql t))
    (true (invoked-p stream :stream-read-char stream))))

(define-test character-input-a.clear-input.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a :input-value "a")))
    (false (clear-input stream))))

(define-test character-input-a.streamp.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (true (streamp stream))
    #+(and gray-streams-streamp (not ccl))
    (true (invoked-p stream :streamp stream))))

(define-test character-input-a.input-stream-p.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (true (input-stream-p stream))
    #+gray-streams-input-stream-p
    (true (invoked-p stream :input-stream-p stream))))

(define-test character-input-a.output-stream-p.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (false (output-stream-p stream))
    #+gray-streams-output-stream-p
    (true (invoked-p stream :output-stream-p stream))))

#+gray-streams-interactive-stream-p
(define-test character-input-a.interactive-stream-p.01
  :parent character-input-a
  (let ((stream (make-instance 'character-input-stream-a)))
    (false (interactive-stream-p stream))
    (true (invoked-p stream :interactive-stream-p stream))))
