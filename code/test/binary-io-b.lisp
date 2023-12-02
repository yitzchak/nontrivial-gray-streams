(in-package #:nontrivial-gray-streams/test)

(defclass binary-io-stream-b
    (ngray:fundamental-binary-input-stream
     binary-input-mixin binary-output-mixin
     #+ccl file-stream)
  ((interactive :reader interactive-p
                :initform nil
                :initarg :interactive)
   (openp :accessor openp
          :initform t)))

(defmethod ngray:close ((stream binary-io-stream-b) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-input stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

(defmethod ngray:stream-listen ((stream binary-io-stream-b))
  (record-invocation :stream-listen stream)
  t)

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-io-stream-b))
  (record-invocation :streamp stream)
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-io-stream-b))
  (record-invocation :input-stream-p stream)
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-io-stream-b))
  (record-invocation :output-stream-p stream)
  nil)

(defmethod ngray:stream-element-type ((stream binary-io-stream-b))
  (record-invocation :stream-element-type stream)
  '(unsigned-byte 8))

#+gray-streams-interactive
(defmethod ngray:interactive-stream-p ((stream binary-io-stream-b))
  (record-invocation :interactive-stream-p stream)
  (interactive-p stream))

(defmethod ngray:stream-read-byte ((stream binary-io-stream-b))
  (record-invocation :stream-read-byte stream)
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (elt input-value input-index)
          (incf input-index))
        :eof)))

(defmethod ngray:stream-listen ((stream binary-io-stream-b))
  (record-invocation :stream-listen stream)
  (< (input-index stream) (length (input-value stream))))

(defmethod ngray:stream-clear-input ((stream binary-io-stream-b))
  (record-invocation :stream-clear-input stream)
  (setf (input-index stream) 0
        (input-value stream) #())
  nil)

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence
  #+gray-streams-sequence/variant-1
  ((stream binary-io-stream-b) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream binary-io-stream-b) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream binary-io-stream-b) &key start end)
  (record-invocation :stream-read-sequence stream sequence start end)
  (unless end
    (setf end (length sequence)))
  (prog ((input-index (or start 0)) ch)
   next
     (when (< input-index end)
       (setf ch (ngray:stream-read-byte stream))
       (unless (eq ch :eof)
         (setf (elt sequence input-index) ch)
         (incf input-index)
         (go next)))
     (return input-index)))

#+gray-streams-file-length/variant-3
(defmethod ngray:stream-file-length ((stream binary-io-stream-b))
  (record-invocation :stream-file-length stream nil)
  (length (input-value stream)))

#+gray-streams-file-length/variant-1
(defmethod ngray:stream-file-length ((stream binary-io-stream-b) &optional length)
  (record-invocation :stream-file-length stream length)
  (length (input-value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod ngray:stream-file-position
    ((stream binary-io-stream-b)
     #+gray-streams-file-position/variant-1 &optional position)
  (record-invocation :stream-file-position stream position)
  (if position
      (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
        (assert (typep position typespec) (position)
                'type-error :datum position :expected-type typespec)
        (setf (input-index stream) position)
        t)
      (input-index stream)))

#+gray-streams-file-position/variant-3
(defmethod ngray:stream-file-position ((stream binary-io-stream-b))
  (record-invocation :stream-file-position stream nil)
  (input-index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf ngray:stream-file-position) (position (stream binary-io-stream-b))
  (record-invocation :stream-file-position stream position)
  (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (input-index stream) position)))

(defmethod ngray:stream-line-column ((stream binary-io-stream-b))
  (record-invocation :stream-line-column stream)
  (line-column stream))

(defmethod ngray:stream-start-line-p ((stream binary-io-stream-b))
  (record-invocation :stream-start-line-p stream)
  (zerop (line-column stream)))

(defmethod ngray:stream-write-byte ((stream binary-io-stream-b) byte)
  (record-invocation :stream-write-byte stream byte)
  (vector-push-extend byte (output-value stream))
  byte)

(defmethod ngray:stream-write-string
  ((stream binary-io-stream-b) sequence &optional start end)
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

(defmethod ngray:stream-clear-output ((stream binary-io-stream-b))
  (record-invocation :stream-clear-output stream)
  nil)

(defmethod ngray:stream-finish-output ((stream binary-io-stream-b))
  (record-invocation :stream-finish-output stream)
  nil)

(defmethod ngray:stream-force-output ((stream binary-io-stream-b))
  (record-invocation :stream-force-output stream)
  nil)

(defmethod ngray:close ((stream binary-io-stream-b) &key abort)
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
  ((stream binary-io-stream-b) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream binary-io-stream-b) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream binary-io-stream-b) &key start end)
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

(define-test binary-io-b)

(define-test binary-io-b.read-byte.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (is eql 10 (read-byte stream nil))
      (is eql 11 (read-byte stream nil))
      (false (read-byte stream nil))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-io-b.read-byte.02
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
      (false (read-byte stream nil))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-io-b.read-byte.03
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
      (eql :wibble (read-byte stream nil :wibble))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-io-b.read-byte.04
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
      (fail (read-byte stream))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-io-b.listen.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (true (listen stream)))))

(define-test binary-io-b.listen.02
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #())))
      (false (listen stream)))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 2 (read-sequence sequence stream))
      (is equalp sequence #(10 11 nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 nil)
                (invoked-p :stream-read-sequence stream sequence 0 3)
                (invoked-p :stream-read-sequence stream sequence nil nil)
                (invoked-p :stream-read-sequence stream sequence nil 3))))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.02
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 1 (read-sequence sequence stream :end 1))
      (is equalp sequence #(10 nil nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 1)
                (invoked-p :stream-read-sequence stream sequence nil 1))))))

#+gray-streams-sequence
(define-test binary-io-b.read-sequence.03
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 3 (read-sequence sequence stream :start 1))
      (is equalp sequence #(nil 10 11))
      (true (or (invoked-p :stream-read-sequence stream sequence 1 nil)
                (invoked-p :stream-read-sequence stream sequence 1 3))))))

#+gray-streams-file-length
(define-test binary-io-b.file-length.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (is eql (file-length stream) 2)
      (true (invoked-p :stream-file-length stream nil)))))

#+gray-streams-file-position
(define-test binary-io-b.file-position.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (is equal 0 (file-position stream))
      (is eql 10 (read-byte stream nil))
      (is equal 1 (file-position stream))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-file-position
(define-test binary-io-b.file-position.02
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (true (file-position stream 1))
      (is equal 1 (file-position stream))
      (is eql 11 (read-byte stream nil))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-interactive
(define-test binary-io-b.interactive-stream-p.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b :input-value #(10 11))))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))

(define-test binary-io-b.write-byte.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b)))
      (is eql 10 (write-byte 10 stream))
      (is eql 11 (write-byte 11 stream))
      (is equalp #(10 11) (output-value stream))
      (true (invoked-p :stream-write-byte stream 10))
      (true (invoked-p :stream-write-byte stream 11)))))

#+gray-streams-sequence
(define-test binary-io-b.write-sequence.01
    :parent binary-io-b
  (with-invocations
    (let ((stream (make-instance 'binary-io-stream-b)))
      (write-sequence #(10 11 12 13) stream)
      (write-sequence #(14 15 16 17) stream :start 1 :end 3)
      (is equalp #(10 11 12 13 15 16) (output-value stream))
      (true (or (invoked-p :stream-write-sequence stream #(10 11 12 13) nil nil)
                (invoked-p :stream-write-sequence stream #(10 11 12 13) 0 nil)
                (invoked-p :stream-write-sequence stream #(10 11 12 13) 0 4)))
      (true (invoked-p :stream-write-sequence stream #(14 15 16 17) 1 3)))))
