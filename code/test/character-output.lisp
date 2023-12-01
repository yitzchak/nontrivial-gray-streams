(in-package #:nontrivial-gray-streams/test)

(defclass character-output-stream
    (ngray:fundamental-character-output-stream)
  ((value :accessor value
          :initform (make-array 16 :element-type 'character
                                   :adjustable t :fill-pointer 0))))

(defmethod ngray:stream-write-char ((stream character-output-stream) char)
  (record-invocation :stream-write-char stream char)
  (vector-push-extend char (value stream))
  char)

(defmethod ngray:stream-line-column ((stream character-output-stream))
  (record-invocation :stream-line-column stream)
  nil)

#+gray-streams-streamp
(defmethod ngray:streamp ((stream character-output-stream))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream character-output-stream))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream character-output-stream))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod ngray:interactive-stream-p ((stream character-output-stream))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

(define-test character-output.write-char.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (is equal #\a (write-char #\a stream))
      (is equal "a" (value stream))
      (true (invoked-p :stream-write-char stream #\a)))))

(define-test character-output.stream-start-line-p.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (ngray:stream-start-line-p stream))
      (true (invoked-p :stream-line-column stream)))))

(define-test character-output.write-string.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (is equal "ab" (write-string "ab" stream))
      (is equal "ab" (value stream))
      (true (invoked-p :stream-write-char stream #\a))
      (true (invoked-p :stream-write-char stream #\b)))))

(define-test character-output.terpri.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (terpri stream))
      (true (invoked-p :stream-write-char stream #\Newline)))))

(define-test character-output.fresh-line.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (true (fresh-line stream))
      (true (invoked-p :stream-write-char stream #\Newline)))))

(define-test character-output.finish-output.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (finish-output stream)))))

(define-test character-output.force-output.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (force-output stream)))))

(define-test character-output.clear-output.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (clear-output stream)))))

(define-test character-output.advance-to-column.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (ngray:stream-advance-to-column stream 10)))))

(define-test character-output.streamp.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test character-output.input-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test character-output.output-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (true (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test character-output.interactive-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-output-stream)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))

(defclass test-string-output-stream
    (ngray:fundamental-character-output-stream)
  ((value :accessor value
          :initform (make-array 16 :element-type 'character
                                   :adjustable t :fill-pointer 0))
   (line-column :accessor line-column
                :initform 0)
   (line-length :reader line-length
                :initform nil
                :initarg :line-length)
   (openp :accessor openp
          :initform t)))

(defmethod ngray:stream-line-column ((stream test-string-output-stream))
  (record-invocation :stream-line-column stream)
  (line-column stream))

(defmethod ngray:stream-start-line-p ((stream test-string-output-stream))
  (record-invocation :stream-start-line-p stream)
  (zerop (line-column stream)))

(defmethod ngray:stream-write-char ((stream test-string-output-stream) char)
  (record-invocation :stream-write-char stream char)
  (vector-push-extend char (value stream))
  (if (char= char #\Newline)
      (setf (line-column stream) 0)
      (incf (line-column stream)))
  char)

(defmethod ngray:stream-write-string
  ((stream test-string-output-stream) sequence &optional start end)
  (record-invocation :stream-write-string stream sequence start end)
  (with-accessors ((value value))
      stream
    (unless end
      (setf end (length sequence)))
    (unless start
      (setf start 0))
    (let* ((len (- end start))
           (start1 (fill-pointer value))
           (end1 (+ start1 len)))
      (if (< (array-total-size value) end1)
          (setf value (adjust-array value end1 :fill-pointer end1))
          (setf (fill-pointer value) end1))
      (replace value sequence
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(defmethod ngray:stream-terpri ((stream test-string-output-stream))
  (record-invocation :stream-terpri stream)
  (vector-push-extend #\Newline (value stream))
  (setf (line-column stream) 0)
  nil)

(defmethod ngray:stream-fresh-line ((stream test-string-output-stream))
  (record-invocation :stream-fresh-line stream)
  (unless (zerop (line-column stream))
    (vector-push-extend #\Newline (value stream))
    (setf (line-column stream) 0))
  nil)

(defmethod ngray:stream-clear-output ((stream test-string-output-stream))
  (record-invocation :stream-clear-output stream)
  nil)

(defmethod ngray:stream-finish-output ((stream test-string-output-stream))
  (record-invocation :stream-finish-output stream)
  nil)

(defmethod ngray:stream-force-output ((stream test-string-output-stream))
  (record-invocation :stream-force-output stream)
  nil)

(defmethod ngray:close ((stream test-string-output-stream) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-output stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

(defmethod ngray:stream-advance-to-column ((stream test-string-output-stream) column)
  (record-invocation :stream-advance-to-column stream column)
  (prog ((current (ngray:stream-line-column stream)))
   repeat
     (when (and current (< column current))
       (ngray:stream-write-char stream #\Space)
       (setf current (ngray:stream-line-column stream))
       (go repeat))))

#+gray-streams-line-length
(defmethod ngray:stream-line-length ((stream test-string-output-stream))
  (record-invocation :stream-line-length stream)
  (or (line-length stream)
      (call-next-method)))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
  #+gray-streams-sequence/variant-1
  ((stream test-string-output-stream) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream test-string-output-stream) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream test-string-output-stream) &key start end)
  (record-invocation :stream-write-sequence stream sequence start end)
  (with-accessors ((value value))
      stream
    (unless end
      (setf end (length sequence)))
    (unless start
      (setf start 0))
    (let* ((len (- end start))
           (start1 (fill-pointer value))
           (end1 (+ start1 len)))
      (if (< (array-total-size value) end1)
          (setf value (adjust-array value end1 :fill-pointer end1))
          (setf (fill-pointer value) end1))
      (replace value sequence
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(define-test character-output.write-char.01
  (with-invocations
    (let ((stream (make-instance 'test-string-output-stream)))
      (is eql #\a (write-char #\a stream))
      (is eql #\b (write-char #\b stream))
      (is equal "ab" (value stream))
      (true (invoked-p :stream-write-char stream #\a))
      (true (invoked-p :stream-write-char stream #\b)))))

#+gray-streams-sequence
(define-test character-output.write-sequence.01
  (with-invocations
    (let ((stream (make-instance 'test-string-output-stream)))
      (write-sequence "abcd" stream)
      (write-sequence "efgh" stream :start 1 :end 3)
      (is equal "abcdfg" (value stream))
      (true (or (invoked-p :stream-write-sequence stream "abcd" nil nil)
                (invoked-p :stream-write-sequence stream "abcd" 0 nil)
                (invoked-p :stream-write-sequence stream "abcd" 0 4)))
      (true (invoked-p :stream-write-sequence stream "efgh" 1 3)))))

#+gray-streams-line-length
(define-test character-output.line-length.01
  (with-invocations
    (let ((stream (make-instance 'test-string-output-stream))
          (*print-right-margin* nil)
          (*print-pretty* nil))
      (format stream "~<aaaa~:;bbbb~>")
      (true (invoked-p :stream-line-length stream)))))
