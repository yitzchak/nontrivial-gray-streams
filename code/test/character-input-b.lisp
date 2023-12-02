(in-package #:nontrivial-gray-streams/test)

(defclass character-input-stream-b
    (ngray:fundamental-character-input-stream #+ccl file-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)
   (interactive :reader interactive-p
                :initform nil
                :initarg :interactive)
   (openp :accessor openp
          :initform t)))

(defmethod ngray:close ((stream character-input-stream-b) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (when abort
           (clear-input stream))
         (setf (openp stream) nil)
         t)
        (t
         nil)))

#+gray-streams-streamp
(defmethod ngray:streamp ((stream character-input-stream-b))
  (record-invocation :streamp stream)
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream character-input-stream-b))
  (record-invocation :input-stream-p stream)
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream character-input-stream-b))
  (record-invocation :output-stream-p stream)
  nil)

(defmethod ngray:stream-element-type ((stream character-input-stream-b))
  (record-invocation :stream-element-type stream)
  'character)

#+gray-streams-interactive
(defmethod ngray:interactive-stream-p ((stream character-input-stream-b))
  (record-invocation :interactive-stream-p stream)
  (interactive-p stream))

(defmethod ngray:stream-read-char ((stream character-input-stream-b))
  (record-invocation :stream-read-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod ngray:stream-read-char-no-hang ((stream character-input-stream-b))
  (record-invocation :stream-read-char-no-hang stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        (if (interactive-p stream) nil :eof))))

(defmethod ngray:stream-unread-char ((stream character-input-stream-b) character)
  (record-invocation :stream-unread-char stream character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char value (decf index)))
      (error "Cannot unread a character that does not match."))
    nil))

(defmethod ngray:stream-peek-char ((stream character-input-stream-b))
  (record-invocation :stream-peek-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (char value index)
        :eof)))

(defmethod ngray:stream-listen ((stream character-input-stream-b))
  (record-invocation :stream-listen stream)
  (< (index stream) (length (value stream))))

(defmethod ngray:stream-read-line ((stream character-input-stream-b))
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

(defmethod ngray:stream-clear-input ((stream character-input-stream-b))
  (record-invocation :stream-clear-input stream)
  (setf (index stream) 0
        (value stream) "")
  nil)

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence
  #+gray-streams-sequence/variant-1
  ((stream character-input-stream-b) sequence &optional start end)
  #+gray-streams-sequence/variant-2
  ((stream character-input-stream-b) sequence start end)
  #+gray-streams-sequence/variant-3
  (sequence (stream character-input-stream-b) &key start end)
  (record-invocation :stream-read-sequence stream sequence start end)
  (unless end
    (setf end (length sequence)))
  (prog ((index (or start 0)) ch)
   next
     (when (< index end)
       (setf ch (ngray:stream-read-char stream))
       (unless (eq ch :eof)
         (setf (elt sequence index) ch)
         (incf index)
         (go next)))
     (return index)))

#+gray-streams-file-length/variant-3
(defmethod ngray:stream-file-length ((stream character-input-stream-b))
  (record-invocation :stream-file-length stream nil)
  (length (value stream)))

#+gray-streams-file-length/variant-1
(defmethod ngray:stream-file-length ((stream character-input-stream-b) &optional length)
  (record-invocation :stream-file-length stream length)
  (length (value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod ngray:stream-file-position
    ((stream character-input-stream-b)
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
(defmethod ngray:stream-file-position ((stream character-input-stream-b))
  (record-invocation :stream-file-position stream nil)
  (index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf ngray:stream-file-position) (position (stream character-input-stream-b))
  (record-invocation :stream-file-position stream position)
  (let ((typespec `(integer 0 ,(1- (length (value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (index stream) position)))

(define-test character-input-b)

(define-test character-input-b.read-char.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (is eql #\a (read-char stream nil))
      (is eql #\b (read-char stream nil))
      (false (read-char stream nil))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-b.read-char.02
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "")))
      (false (read-char stream nil))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-b.read-char.03
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "")))
      (eql :wibble (read-char stream nil :wibble))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-b.read-char.04
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "")))
      (fail (read-char stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-b.peek-char.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (is eql #\a (peek-char nil stream nil))
      (is eql #\a (read-char stream nil))
      (is eql #\b (read-char stream nil))
      (false (read-char stream nil))
      (true (invoked-p :stream-peek-char stream))
      (true (invoked-p :stream-read-char stream)))))

(define-test character-input-b.peek-char.02
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "")))
      (false (peek-char nil stream nil)))))

(define-test character-input-b.listen.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (true (listen stream)))))

(define-test character-input-b.listen.02
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "")))
      (false (listen stream)))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 2 (read-sequence sequence stream))
      (is equalp sequence #(#\a #\b nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 nil)
                (invoked-p :stream-read-sequence stream sequence 0 3)
                (invoked-p :stream-read-sequence stream sequence nil nil)
                (invoked-p :stream-read-sequence stream sequence nil 3))))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.02
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 1 (read-sequence sequence stream :end 1))
      (is equalp sequence #(#\a nil nil))
      (true (or (invoked-p :stream-read-sequence stream sequence 0 1)
                (invoked-p :stream-read-sequence stream sequence nil 1))))))

#+gray-streams-sequence
(define-test character-input-b.read-sequence.03
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab"))
          (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
      (is eql 3 (read-sequence sequence stream :start 1))
      (is equalp sequence #(nil #\a #\b))
      (true (or (invoked-p :stream-read-sequence stream sequence 1 nil)
                (invoked-p :stream-read-sequence stream sequence 1 3))))))

#+gray-streams-file-length
(define-test character-input-b.file-length.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (is eql (file-length stream) 2)
      (true (invoked-p :stream-file-length stream nil)))))

#+gray-streams-file-position
(define-test character-input-b.file-position.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (is equal 0 (file-position stream))
      (is eql #\a (peek-char nil stream nil))
      (is equal 0 (file-position stream))
      (is eql #\a (read-char stream nil))
      (is equal 1 (file-position stream))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-file-position
(define-test character-input-b.file-position.02
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (true (file-position stream 1))
      (is equal 1 (file-position stream))
      (is eql #\b (read-char stream nil))
      (true (invoked-p :stream-file-position stream nil)))))

#+gray-streams-interactive
(define-test character-input-b.interactive-stream-p.01
    :parent character-input-b
  (with-invocations
    (let ((stream (make-instance 'character-input-stream-b :value "ab")))
      (false (interactive-stream-p stream))
      (true (invoked-p :interactive-stream-p stream)))))
