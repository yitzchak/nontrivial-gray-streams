(in-package #:nontrivial-gray-streams/test)

(defclass character-output-stream-a
    (ngray:fundamental-character-output-stream)
  ((value :accessor value
          :initform (make-array 16 :element-type 'character
                                   :adjustable t :fill-pointer 0))))

(defmethod ngray:stream-write-char ((stream character-output-stream-a) char)
  (record-invocation :stream-write-char stream char)
  (vector-push-extend char (value stream))
  char)

(defmethod ngray:stream-line-column ((stream character-output-stream-a))
  (record-invocation :stream-line-column stream)
  nil)

#+gray-streams-streamp
(defmethod ngray:streamp ((stream character-output-stream-a))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream character-output-stream-a))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream character-output-stream-a))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream character-output-stream-a))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test character-output-a)

(define-test character-output-a.write-char.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (is equal #\a (write-char #\a stream))
      (is equal "a" (value stream))
      (true (invoked-p :stream-write-char stream #\a)))))

(define-test character-output-a.stream-start-line-p.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (ngray:stream-start-line-p stream))
      (true (invoked-p :stream-line-column stream)))))

(define-test character-output-a.write-string.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (is equal "ab" (write-string "ab" stream))
      (is equal "ab" (value stream))
      (true (invoked-p :stream-write-char stream #\a))
      (true (invoked-p :stream-write-char stream #\b)))))

(define-test character-output-a.terpri.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (terpri stream))
      (true (invoked-p :stream-write-char stream #\Newline)))))

(define-test character-output-a.fresh-line.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (true (fresh-line stream))
      (true (invoked-p :stream-write-char stream #\Newline)))))

(define-test character-output-a.finish-output.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (finish-output stream)))))

(define-test character-output-a.force-output.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (force-output stream)))))

(define-test character-output-a.clear-output.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (clear-output stream)))))

(define-test character-output-a.advance-to-column.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (ngray:stream-advance-to-column stream 10)))))

(define-test character-output-a.streamp.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test character-output-a.input-stream-p.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test character-output-a.output-stream-p.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (true (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test character-output-a.interactive-stream-p.01
    :parent character-output-a
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-a)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
