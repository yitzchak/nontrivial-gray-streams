(in-package #:nontrivial-gray-streams/test)

(defclass binary-input-stream
    (nt-gray:fundamental-binary-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod nt-gray:stream-read-byte ((stream binary-input-stream))
  (record-invocation :stream-read-byte stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (elt value index)
          (incf index))
        :eof)))

(defmethod nt-gray:stream-listen ((stream binary-input-stream))
  (record-invocation :stream-listen stream)
  (< (index stream) (length (value stream))))

(defmethod nt-gray:stream-element-type ((stream binary-input-stream))
  (record-invocation :stream-element-type stream)
  '(unsigned-byte 8))

#+gray-streams-streamp
(defmethod nt-gray:streamp ((stream binary-input-stream))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod nt-gray:input-stream-p ((stream binary-input-stream))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod nt-gray:output-stream-p ((stream binary-input-stream))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod nt-gray:interactive-stream-p ((stream binary-input-stream))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

#+gray-streams-file-length
(define-test binary-input.file-length.default-method.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream :value #(10))))
      (fail (file-length stream) 'type-error))))

(define-test binary-input.read-byte.default-method.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream :value #(10))))
      (is equal 10 (read-byte stream))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input.listen.default-method.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream :value #(10))))
      (true (listen stream))
      (true (invoked-p :stream-listen stream)))))

(define-test binary-input.clear-input.default-method.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream :value #(10))))
      (false (clear-input stream)))))

(define-test binary-input.streamp.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream)))
      (true (streamp stream))
      #+(and gray-streams-streamp (not ccl))
      (true (invoked-p :streamp stream)))))

(define-test binary-input.input-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test binary-input.output-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream)))
      (false (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test binary-input.interactive-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'binary-input-stream)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))

(defclass test-binary-input-stream
    (nt-gray:fundamental-binary-input-stream #+ccl file-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)
   (interactive :reader interactive-p
                :initform nil
                :initarg :interactive)
   (openp :accessor openp
          :initform t)))

(defmethod nt-gray:close ((stream test-binary-input-stream) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-input stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

(defmethod nt-gray:stream-listen ((stream binary-input-stream))
  (record-invocation :stream-listen stream)
  t)

#+gray-streams-streamp
(defmethod nt-gray:streamp ((stream test-binary-input-stream))
  (record-invocation :streamp stream)
  t)

#+gray-streams-input-stream-p
(defmethod nt-gray:input-stream-p ((stream test-binary-input-stream))
  (record-invocation :input-stream-p stream)
  t)

#+gray-streams-output-stream-p
(defmethod nt-gray:output-stream-p ((stream test-binary-input-stream))
  (record-invocation :output-stream-p stream)
  nil)

(defmethod nt-gray:stream-element-type ((stream test-binary-input-stream))
  (record-invocation :stream-element-type stream)
  '(unsigned-byte 8))

#+gray-streams-interactive
(defmethod nt-gray:interactive-stream-p ((stream test-binary-input-stream))
  (record-invocation :interactive-stream-p stream)
  (interactive-p stream))

(defmethod nt-gray:stream-read-byte ((stream test-binary-input-stream))
  (record-invocation :stream-read-byte stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (elt value index)
          (incf index))
        :eof)))

(defmethod nt-gray:stream-listen ((stream test-binary-input-stream))
  (record-invocation :stream-listen stream)
  (< (index stream) (length (value stream))))

(defmethod nt-gray:stream-clear-input ((stream test-binary-input-stream))
  (record-invocation :stream-clear-input stream)
  (setf (index stream) 0
        (value stream) "")
  nil)

#+gray-streams-sequence
(defmethod nt-gray:stream-read-sequence
  #+gray-streams-sequence/variant-1
  ((stream test-binary-input-stream) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream test-binary-input-stream) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream test-binary-input-stream) &key start end)
  (record-invocation :stream-read-sequence stream sequence start end)
  (unless end
    (setf end (length sequence)))
  (prog ((index (or start 0)) ch)
   next
     (when (< index end)
       (setf ch (nt-gray:stream-read-byte stream))
       (unless (eq ch :eof)
         (setf (elt sequence index) ch)
         (incf index)
         (go next)))
     (return index)))

#+gray-streams-file-length/variant-3
(defmethod nt-gray:stream-file-length ((stream test-binary-input-stream))
  (record-invocation :stream-file-length stream nil)
  (length (value stream)))

#+gray-streams-file-length/variant-1
(defmethod nt-gray:stream-file-length ((stream test-binary-input-stream) &optional length)
  (record-invocation :stream-file-length stream length)
  (length (value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod nt-gray:stream-file-position
    ((stream test-binary-input-stream)
     #+gray-streams-file-position/variant-1 &optional position)
  (record-invocation :stream-file-position stream position)
  (if position
      (let ((typespec `(integer 0 ,(1- (length (value stream))))))
        (assert (typep position typespec) (position)
                'type-error :datum position :expected-type typespec)
        (setf (index stream) position)
        t)
      (index stream)))

#+gray-streams-file-position/variant-3
(defmethod nt-gray:stream-file-position ((stream test-binary-input-stream))
  (record-invocation :stream-file-position stream nil)
  (index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf nt-gray:stream-file-position) (position (stream test-binary-input-stream))
  (record-invocation :stream-file-position stream position)
  (let ((typespec `(integer 0 ,(1- (length (value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (index stream) position)))

(define-test binary-input.read-byte.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (is eql 10 (read-byte stream nil))
      (is eql 11 (read-byte stream nil))
      (false (read-byte stream nil))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input.read-byte.02
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value "")))
      (false (read-byte stream nil))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input.read-byte.03
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value "")))
      (eql :wibble (read-byte stream nil :wibble))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input.read-byte.04
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value "")))
      (fail (read-byte stream))
      (true (invoked-p :stream-read-byte stream)))))

(define-test binary-input.listen.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (true (listen stream)))))

(define-test binary-input.listen.02
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value "")))
      (false (listen stream)))))

#+gray-streams-sequence
(define-test binary-input.read-sequence.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 2 (read-sequence sequence stream))
      (is equalp sequence #(10 11 nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 nil)
                (invoked-p :stream-read-sequence stream sequence 0 3)
                (invoked-p :stream-read-sequence stream sequence nil nil)
                (invoked-p :stream-read-sequence stream sequence nil 3))))))

#+gray-streams-sequence
(define-test binary-input.read-sequence.02
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 1 (read-sequence sequence stream :end 1))
      (is equalp sequence #(10 nil nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 1)
                (invoked-p :stream-read-sequence stream sequence nil 1))))))

#+gray-streams-sequence
(define-test binary-input.read-sequence.03
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11)))
          (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
      (is eql 3 (read-sequence sequence stream :start 1))
      (is equalp sequence #(nil 10 11))
      (true (or (invoked-p :stream-read-sequence stream sequence 1 nil)
                (invoked-p :stream-read-sequence stream sequence 1 3))))))

#+gray-streams-file-length
(define-test binary-input.file-length.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (is eql (file-length stream) 2)
      (true (invoked-p :stream-file-length stream nil)))))

#+gray-streams-file-position
(define-test binary-input.file-position.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (is equal 0 (file-position stream))
      (is eql 10 (read-byte stream nil))
      (is equal 1 (file-position stream))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-file-position
(define-test binary-input.file-position.02
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (true (file-position stream 1))
      (is equal 1 (file-position stream))
      (is eql 11 (read-byte stream nil))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-interactive
(define-test binary-input.interactive-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'test-binary-input-stream :value #(10 11))))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))