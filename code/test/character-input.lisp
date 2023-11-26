(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream
    (nt-gray:fundamental-character-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod nt-gray:stream-read-char ((stream character-input-stream))
  (record-invocation :stream-read-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod nt-gray:stream-unread-char ((stream character-input-stream) character)
  (record-invocation :stream-unread-char stream character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char value (decf index)))
      (error "Cannot unread a character that does not match."))
    nil))

#+gray-streams-streamp
(defmethod nt-gray:streamp ((stream character-input-stream))
  (record-invocation :streamp stream)
  (call-next-method))

#+gray-streams-input-stream-p
(defmethod nt-gray:input-stream-p ((stream character-input-stream))
  (record-invocation :input-stream-p stream)
  (call-next-method))

#+gray-streams-output-stream-p
(defmethod nt-gray:output-stream-p ((stream character-input-stream))
  (record-invocation :output-stream-p stream)
  (call-next-method))

#+gray-streams-interactive-stream-p
(defmethod nt-gray:interactive-stream-p ((stream character-input-stream))
  (record-invocation :interactive-stream-p stream)
  (call-next-method))

#+gray-streams-file-length
(define-test character-input.file-length.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (fail (file-length stream) 'type-error))))

(define-test character-input.read-char.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (is equal #\a (read-char stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.read-char-no-hang.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (is equal #\a (read-char-no-hang stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.peek-char.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (is equal #\a (peek-char nil stream))
      (true (invoked-p :stream-read-char stream))
      (true (invoked-p :stream-unread-char stream #\a)))))

(define-test character-input.listen.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (true (listen stream))
      (true (invoked-p :stream-read-char stream))
      (true (invoked-p :stream-unread-char stream #\a)))))

(define-test character-input.read-line.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (is-values (read-line stream nil)
                 (equal "a")
                 (eql t))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.clear-input.default-method.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream :value "a")))
      (false (clear-input stream)))))

(define-test character-input.streamp.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream)))
      (true (streamp stream))
      #+gray-streams-streamp
      (true (invoked-p :streamp stream)))))

(define-test character-input.input-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream)))
      (true (input-stream-p stream))
      #+gray-streams-input-stream-p
      (true (invoked-p :input-stream-p stream)))))

(define-test character-input.output-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream)))
      (false (output-stream-p stream))
      #+gray-streams-output-stream-p
      (true (invoked-p :output-stream-p stream)))))

#+gray-streams-interactive-stream-p
(define-test character-input.interactive-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'character-input-stream)))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))

(defclass test-string-input-stream
    (nt-gray:fundamental-character-input-stream #+ccl file-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)
   (interactive :reader interactive-p
                :initform nil
                :initarg :interactive)
   (openp :accessor openp
          :initform t)))

(defmethod nt-gray:close ((stream test-string-input-stream) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-input stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

#+gray-streams-streamp
(defmethod nt-gray:streamp ((stream test-string-input-stream))
  (record-invocation :streamp stream)
  t)

#+gray-streams-input-stream-p
(defmethod nt-gray:input-stream-p ((stream test-string-input-stream))
  (record-invocation :input-stream-p stream)
  t)

#+gray-streams-output-stream-p
(defmethod nt-gray:output-stream-p ((stream test-string-input-stream))
  (record-invocation :output-stream-p stream)
  nil)

(defmethod nt-gray:stream-element-type ((stream test-string-input-stream))
  (record-invocation :stream-element-type stream)
  'character)

#+gray-streams-interactive
(defmethod nt-gray:interactive-stream-p ((stream test-string-input-stream))
  (record-invocation :interactive-stream-p stream)
  (interactive-p stream))

(defmethod nt-gray:stream-read-char ((stream test-string-input-stream))
  (record-invocation :stream-read-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod nt-gray:stream-read-char-no-hang ((stream test-string-input-stream))
  (record-invocation :stream-read-char-no-hang stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        (if (interactive-p stream) nil :eof))))

(defmethod nt-gray:stream-unread-char ((stream test-string-input-stream) character)
  (record-invocation :stream-unread-char stream character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char value (decf index)))
      (error "Cannot unread a character that does not match."))
    nil))

(defmethod nt-gray:stream-peek-char ((stream test-string-input-stream))
  (record-invocation :stream-peek-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (char value index)
        :eof)))

(defmethod nt-gray:stream-listen ((stream test-string-input-stream))
  (record-invocation :stream-listen stream)
  (< (index stream) (length (value stream))))

(defmethod nt-gray:stream-read-line ((stream test-string-input-stream))
  (record-invocation :stream-read-line stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< (index stream) (length (value stream)))
        (let ((pos (position #\Newline value :start index)))
          (if pos
              (multiple-value-prog1
                  (values (subseq value index pos) nil)
                (setf index (1+ pos)))
              (multiple-value-prog1
                  (values (subseq value index) t)
                (setf index (length value)))))
        (values :eof nil))))

(defmethod nt-gray:stream-clear-input ((stream test-string-input-stream))
  (record-invocation :stream-clear-input stream)
  (setf (index stream) 0
        (value stream) "")
  nil)

#+gray-streams-sequence
(defmethod nt-gray:stream-read-sequence
  #+gray-streams-sequence/variant-1
  ((stream test-string-input-stream) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream test-string-input-stream) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream test-string-input-stream) &key start end)
  (record-invocation :stream-read-sequence stream sequence start end)
  (unless end
    (setf end (length sequence)))
  (prog ((index (or start 0)) ch)
   next
     (when (< index end)
       (setf ch (nt-gray:stream-read-char stream))
       (unless (eq ch :eof)
         (setf (elt sequence index) ch)
         (incf index)
         (go next)))
     (return index)))

#+gray-streams-file-length/variant-3
(defmethod nt-gray:stream-file-length ((stream test-string-input-stream))
  (record-invocation :stream-file-length stream nil)
  (length (value stream)))

#+gray-streams-file-length/variant-1
(defmethod nt-gray:stream-file-length ((stream test-string-input-stream) &optional length)
  (record-invocation :stream-file-length stream length)
  (length (value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod nt-gray:stream-file-position
    ((stream test-string-input-stream)
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
(defmethod nt-gray:stream-file-position ((stream test-string-input-stream))
  (record-invocation :stream-file-position stream nil)
  (index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf nt-gray:stream-file-position) (position (stream test-string-input-stream))
  (record-invocation :stream-file-position stream position)
  (let ((typespec `(integer 0 ,(1- (length (value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (index stream) position)))

(define-test character-input.read-char.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (is eql #\a (read-char stream nil))
      (is eql #\b (read-char stream nil))
      (false (read-char stream nil))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.read-char.02
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "")))
      (false (read-char stream nil))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.peek-char.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (is eql #\a (peek-char nil stream nil))
      (is eql #\a (read-char stream nil))
      (is eql #\b (read-char stream nil))
      (false (read-char stream nil))
      (true (invoked-p :stream-peek-char stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input.peek-char.02
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "")))
      (false (peek-char nil stream nil)))))

(define-test character-input.listen.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (true (listen stream)))))

(define-test character-input.listen.02
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "")))
      (false (listen stream)))))

#+gray-streams-sequence
(define-test character-input.read-sequence.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 2 (read-sequence sequence stream))
      (is equalp sequence #(#\a #\b nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 nil)
                (invoked-p :stream-read-sequence stream sequence 0 3)
                (invoked-p :stream-read-sequence stream sequence nil nil)
                (invoked-p :stream-read-sequence stream sequence nil 3))))))

#+gray-streams-sequence
(define-test character-input.read-sequence.02
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 1 (read-sequence sequence stream :end 1))
      (is equalp sequence #(#\a nil nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 1)
                (invoked-p :stream-read-sequence stream sequence nil 1))))))

#+gray-streams-sequence
(define-test character-input.read-sequence.03
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 3 (read-sequence sequence stream :start 1))
      (is equalp sequence #(nil #\a #\b))
      (true (or (invoked-p :stream-read-sequence stream sequence 1 nil)
                (invoked-p :stream-read-sequence stream sequence 1 3))))))

#+gray-streams-file-length
(define-test character-input.file-length.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (is eql (file-length stream) 2)
      (true (invoked-p :stream-file-length stream nil)))))

#+gray-streams-file-position
(define-test character-input.file-position.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (is equal 0 (file-position stream))
      (is eql #\a (peek-char nil stream nil))
      (is equal 0 (file-position stream))
      (is eql #\a (read-char stream nil))
      (is equal 1 (file-position stream))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-file-position
(define-test character-input.file-position.02
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (true (file-position stream 1))
      (is equal 1 (file-position stream))
      (is eql #\b (read-char stream nil))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-interactive
(define-test character-input.interactive-stream-p.01
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
