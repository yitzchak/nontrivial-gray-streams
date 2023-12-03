(in-package #:nontrivial-gray-streams/test)

#+gray-streams-element-type
(progn

  (defclass bivalent-input-stream-a
      (bivalent-input-mixin-a
       ngray:fundamental-binary-input-stream
       ngray:fundamental-character-input-stream)
    ())

  (define-test bivalent-input-a)

  (define-test bivalent-input-a.element-type.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a")))
      (is equalp '(unsigned-byte 8) (stream-element-type stream))
      (is equalp 'character (setf (stream-element-type stream) 'character))
      (is equalp 'character (stream-element-type stream))
      (true (invoked-p stream :stream-element-type stream))
      (true (invoked-p stream :stream-element-type stream 'character))))

  #+gray-streams-file-length
  (define-test bivalent-input-a.file-length.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (fail (file-length stream) 'type-error)))

  (define-test bivalent-input-a.read-char.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (is equal #\a (read-char stream))
      (true (invoked-p stream :stream-read-char stream))))

  (define-test bivalent-input-a.read-char.02
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a")))
      (fail (read-char stream))
      (true (invoked-p stream :stream-read-char stream))))

  (define-test bivalent-input-a.read-char.03
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a")))
      (is eql 'character (setf (stream-element-type stream) 'character))
      (is equal #\a (read-char stream))
      (true (invoked-p stream :stream-read-char stream))
      (true (invoked-p stream :stream-element-type stream 'character))))

  (define-test bivalent-input-a.read-char-no-hang.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (is equal #\a (read-char-no-hang stream))
      (true (invoked-p stream :stream-read-char stream))))

  (define-test bivalent-input-a.peek-char.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (is equal #\a (peek-char nil stream))
      (true (invoked-p stream :stream-read-char stream))
      (true (invoked-p stream :stream-unread-char stream #\a))))

  (define-test bivalent-input-a.listen.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (true (listen stream))
      (true (invoked-p stream :stream-read-char stream))
      (true (invoked-p stream :stream-unread-char stream #\a))))

  (define-test bivalent-input-a.read-line.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (is-values (read-line stream nil)
                 (equal "a")
                 (eql t))
      (true (invoked-p stream :stream-read-char stream))))

  (define-test bivalent-input-a.clear-input.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a
                                 :input-value "a"
                                 :element-type 'character)))
      (false (clear-input stream))))

  (define-test bivalent-input-a.streamp.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p stream :streamp stream))))

  (define-test bivalent-input-a.input-stream-p.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p stream :input-stream-p stream))))

  (define-test bivalent-input-a.output-stream-p.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a)))
      (false (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p stream :output-stream-p stream))))

  #+gray-streams-interactive-stream-p
  (define-test bivalent-input-a.interactive-stream-p.01
    :parent bivalent-input-a
    (let ((stream (make-instance 'bivalent-input-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p stream :interactive-stream-p stream)))))
