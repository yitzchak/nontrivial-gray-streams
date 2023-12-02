(in-package #:nontrivial-gray-streams/test)

(defclass binary-output-stream-b
    (ngray:fundamental-binary-output-stream binary-output-mixin)
  ((openp :accessor openp
          :initform t)))

(defmethod ngray:stream-line-column ((stream binary-output-stream-b))
  (record-invocation :stream-line-column stream)
  (line-column stream))

(defmethod ngray:stream-start-line-p ((stream binary-output-stream-b))
  (record-invocation :stream-start-line-p stream)
  (zerop (line-column stream)))

(defmethod ngray:stream-write-byte ((stream binary-output-stream-b) byte)
  (record-invocation :stream-write-byte stream byte)
  (vector-push-extend byte (output-value stream))
  byte)

(defmethod ngray:stream-write-string
  ((stream binary-output-stream-b) sequence &optional start end)
  (record-invocation :stream-write-string stream sequence start end)
  (with-accessors ((output-value output-value))
      stream
    (unless end
      (setf end (length sequence)))
    (unless start
      (setf start 0))
    (let* ((len (- end start))
           (start1 (fill-pointer output-value))
           (end1 (+ start1 len)))
      (if (< (array-total-size output-value) end1)
          (setf output-value (adjust-array output-value end1 :fill-pointer end1))
          (setf (fill-pointer output-value) end1))
      (replace output-value sequence
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(defmethod ngray:stream-clear-output ((stream binary-output-stream-b))
  (record-invocation :stream-clear-output stream)
  nil)

(defmethod ngray:stream-finish-output ((stream binary-output-stream-b))
  (record-invocation :stream-finish-output stream)
  nil)

(defmethod ngray:stream-force-output ((stream binary-output-stream-b))
  (record-invocation :stream-force-output stream)
  nil)

(defmethod ngray:close ((stream binary-output-stream-b) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-output stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
  #+gray-streams-sequence/variant-1
  ((stream binary-output-stream-b) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream binary-output-stream-b) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream binary-output-stream-b) &key start end)
  (record-invocation :stream-write-sequence stream sequence start end)
  (with-accessors ((output-value output-value))
      stream
    (unless end
      (setf end (length sequence)))
    (unless start
      (setf start 0))
    (let* ((len (- end start))
           (start1 (fill-pointer output-value))
           (end1 (+ start1 len)))
      (if (< (array-total-size output-value) end1)
          (setf output-value (adjust-array output-value end1 :fill-pointer end1))
          (setf (fill-pointer output-value) end1))
      (replace output-value sequence
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(define-test binary-output-b)

(define-test binary-output-b.write-byte.01
    :parent binary-output-b
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-b)))
      (is eql 10 (write-byte 10 stream))
      (is eql 11 (write-byte 11 stream))
      (is equalp #(10 11) (output-value stream))
      (true (invoked-p :stream-write-byte stream 10))
      (true (invoked-p :stream-write-byte stream 11)))))

#+gray-streams-sequence
(define-test binary-output-b.write-sequence.01
    :parent binary-output-b
  (with-invocations
    (let ((stream (make-instance 'binary-output-stream-b)))
      (write-sequence #(10 11 12 13) stream)
      (write-sequence #(14 15 16 17) stream :start 1 :end 3)
      (is equalp #(10 11 12 13 15 16) (output-value stream))
      (true (or (invoked-p :stream-write-sequence stream #(10 11 12 13) nil nil)
                (invoked-p :stream-write-sequence stream #(10 11 12 13) 0 nil)
                (invoked-p :stream-write-sequence stream #(10 11 12 13) 0 4)))
      (true (invoked-p :stream-write-sequence stream #(14 15 16 17) 1 3)))))
