(in-package #:nontrivial-gray-streams/test)

(defclass character-output-stream-b
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

(defmethod ngray:stream-line-column ((stream character-output-stream-b))
  (record-invocation :stream-line-column stream)
  (line-column stream))

(defmethod ngray:stream-start-line-p ((stream character-output-stream-b))
  (record-invocation :stream-start-line-p stream)
  (zerop (line-column stream)))

(defmethod ngray:stream-write-char ((stream character-output-stream-b) char)
  (record-invocation :stream-write-char stream char)
  (vector-push-extend char (value stream))
  (if (char= char #\Newline)
      (setf (line-column stream) 0)
      (incf (line-column stream)))
  char)

(defmethod ngray:stream-write-string
  ((stream character-output-stream-b) sequence &optional start end)
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

(defmethod ngray:stream-terpri ((stream character-output-stream-b))
  (record-invocation :stream-terpri stream)
  (vector-push-extend #\Newline (value stream))
  (setf (line-column stream) 0)
  nil)

(defmethod ngray:stream-fresh-line ((stream character-output-stream-b))
  (record-invocation :stream-fresh-line stream)
  (unless (zerop (line-column stream))
    (vector-push-extend #\Newline (value stream))
    (setf (line-column stream) 0))
  nil)

(defmethod ngray:stream-clear-output ((stream character-output-stream-b))
  (record-invocation :stream-clear-output stream)
  nil)

(defmethod ngray:stream-finish-output ((stream character-output-stream-b))
  (record-invocation :stream-finish-output stream)
  nil)

(defmethod ngray:stream-force-output ((stream character-output-stream-b))
  (record-invocation :stream-force-output stream)
  nil)

(defmethod ngray:close ((stream character-output-stream-b) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-output stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

(defmethod ngray:stream-advance-to-column ((stream character-output-stream-b) column)
  (record-invocation :stream-advance-to-column stream column)
  (prog ((current (ngray:stream-line-column stream)))
   repeat
     (when (and current (< column current))
       (ngray:stream-write-char stream #\Space)
       (setf current (ngray:stream-line-column stream))
       (go repeat))))

#+gray-streams-line-length
(defmethod ngray:stream-line-length ((stream character-output-stream-b))
  (record-invocation :stream-line-length stream)
  (or (line-length stream)
      (call-next-method)))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
  #+gray-streams-sequence/variant-1
  ((stream character-output-stream-b) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream character-output-stream-b) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream character-output-stream-b) &key start end)
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

(define-test character-output-b)

(define-test character-output-b.write-char.01
    :parent character-output-b
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-b)))
      (is eql #\a (write-char #\a stream))
      (is eql #\b (write-char #\b stream))
      (is equal "ab" (value stream))
      (true (invoked-p :stream-write-char stream #\a))
      (true (invoked-p :stream-write-char stream #\b)))))

#+gray-streams-sequence
(define-test character-output-b.write-sequence.01
    :parent character-output-b
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-b)))
      (write-sequence "abcd" stream)
      (write-sequence "efgh" stream :start 1 :end 3)
      (is equal "abcdfg" (value stream))
      (true (or (invoked-p :stream-write-sequence stream "abcd" nil nil)
                (invoked-p :stream-write-sequence stream "abcd" 0 nil)
                (invoked-p :stream-write-sequence stream "abcd" 0 4)))
      (true (invoked-p :stream-write-sequence stream "efgh" 1 3)))))

#+gray-streams-line-length
(define-test character-output-b.line-length.01
    :parent character-output-b
  (with-invocations
    (let ((stream (make-instance 'character-output-stream-b))
          (*print-right-margin* nil)
          (*print-pretty* nil))
      (format stream "~<aaaa~:;bbbb~>")
      (true (invoked-p :stream-line-length stream)))))
