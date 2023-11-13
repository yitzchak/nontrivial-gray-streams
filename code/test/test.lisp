(in-package #:nontrivial-gray-streams/test)

(defparameter *invocations* nil)

(defmacro with-invocations (&body body)
  `(let (*invocations*) ,@body))

(defun record-invocation (&rest args)
  (pushnew args *invocations* :test #'equal))

(defun invoked-p (&rest args)
  (and (position args *invocations* :test #'equal) t))

(defclass test-string-input-stream
    (nt-gray:fundamental-character-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod nt-gray:stream-read-char ((stream test-string-input-stream))
  (record-invocation :stream-read-char stream)
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (char value index)
          (incf index))
        :eof)))

(defmethod nt-gray:stream-unread-char ((stream test-string-input-stream) character)
  (record-invocation :stream-unread-char stream character)
  (with-accessors ((value value)
                   (index index))
      stream
    (when (zerop value)
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

#+(or gray-streams-sequence-optional
      gray-streams-sequence-required
      gray-streams-sequence-key
      gray-streams-vector)
(defmethod #-gray-streams-vector nt-gray:stream-read-sequence
           #+gray-streams-vector nt-gray:stream-read-vector
    ((stream test-string-input-stream) sequence
     #+gray-streams-sequence-optional &optional
     #+gray-streams-sequence-key &key
     start end)
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

#+gray-streams-file-length
(defmethod nt-gray:stream-file-length ((stream test-string-input-stream))
  (record-invocation :stream-file-length stream)
  (length (value stream)))

#+(or gray-streams-file-position-optional
      gray-streams-file-position-required)
(defmethod nt-gray:stream-file-position
    ((stream test-string-input-stream)
     #+gray-streams-file-position-optional &optional position)
  (record-invocation :stream-file-position stream position)
  (if position
      (let ((typespec `(integer 0 ,(1- (length (value stream))))))
        (assert (typep position typespec) (position)
                'type-error :datum position :expected-type typespec)
        (setf (index stream) position)
        t)
      (index stream)))

#+gray-streams-file-position-setf
(defmethod nt-gray:stream-file-position ((stream test-string-input-stream))
  (record-invocation :stream-file-position stream)
  (index stream))

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

(define-test character-input.read-sequence.01
  (skip-on ((not (or :gray-streams-sequence-optional
                     :gray-streams-sequence-key
                     :gray-streams-sequence-required
                     :gray-streams-vector)))
           "Implementation does not support sequence extensions."
           (with-invocations
             (let ((stream (make-instance 'test-string-input-stream :value "ab"))
                   (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
               (is eql 2 (read-sequence sequence stream))
               (is equalp sequence #(#\a #\b nil))
               (true (or (invoked-p :stream-read-sequence stream sequence 0 nil)
                         (invoked-p :stream-read-sequence stream sequence 0 3)))))))

(define-test character-input.read-sequence.02
  (skip-on ((not (or :gray-streams-sequence-optional
                     :gray-streams-sequence-key
                     :gray-streams-sequence-required
                     :gray-streams-vector)))
           "Implementation does not support sequence extensions."
           (with-invocations
             (let ((stream (make-instance 'test-string-input-stream :value "ab"))
                   (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
               (is eql 1 (read-sequence sequence stream :end 1))
               (is equalp sequence #(#\a nil nil))
               (true (invoked-p :stream-read-sequence stream sequence 0 1))))))

(define-test character-input.read-sequence.03
  (skip-on ((not (or :gray-streams-sequence-optional
                     :gray-streams-sequence-key
                     :gray-streams-sequence-required
                     :gray-streams-vector)))
           "Implementation does not support sequence extensions."
           (with-invocations
             (let ((stream (make-instance 'test-string-input-stream :value "ab"))
                   (sequence (make-array 3 :element-type '(or character null) :initial-element nil)))
               (is eql 3 (read-sequence sequence stream :start 1))
               (is equalp sequence #(nil #\a #\b))
               (true (or (invoked-p :stream-read-sequence stream sequence 1 nil)
                         (invoked-p :stream-read-sequence stream sequence 1 3)))))))

#+gray-streams-file-length
(define-test character-input.file-length.01
  :compile-at :execute
  (with-invocations
    (let ((stream (make-instance 'test-string-input-stream :value "ab")))
      (is eql (file-length stream) 2)
      (true (invoked-p :stream-file-length stream)))))

(define-test character-input.file-position.01
  :compile-at :execute
  (skip-on ((not :gray-streams-file-position-optional))
           "Implementation does not support file-position extensions."
           (with-invocations
             (let ((stream (make-instance 'test-string-input-stream :value "ab")))
               (is equal 0 (file-position stream))
               (is eql #\a (peek-char nil stream nil))
               (is equal 0 (file-position stream))
               (is eql #\a (read-char stream nil))
               (is equal 1 (file-position stream))
               (true (invoked-p :stream-file-position stream nil))))))

(define-test character-input.file-position.02
  :compile-at :execute
  (skip-on ((not :gray-streams-file-position-optional))
           "Implementation does not support file-position extensions."
           (with-invocations
             (let ((stream (make-instance 'test-string-input-stream :value "ab")))
               (true (file-position stream 1))
               (is equal 1 (file-position stream))
               (is eql #\b (read-char stream nil))
               (true (invoked-p :stream-file-position stream nil))))))

  #|(defclass test-stream
    (nt-gray:fundamental-binary-input-stream
     nt-gray:fundamental-binary-output-stream
     nt-gray:fundamental-character-input-stream
     nt-gray:fundamental-character-output-stream)
  ())

(defmacro define-invoked-test (name form &rest calls)
  `(define-test ,name
     (let ((stream (make-instance 'test-stream))
           *invocations*)
       ,form
       (is equal
          (list ,@(mapcar (lambda (call)
                            `(list ',(first call) ,@(rest call)))
                          calls))
          *invocations*))))

(defmethod nt-gray:stream-read-char ((stream test-stream))
  (record-invocation :stream-read-char stream)
  #\a)

(defmethod nt-gray:stream-unread-char ((stream test-stream) char)
  (record-invocation :stream-unread-char stream char)
  char)

(defmethod nt-gray:stream-read-char-no-hang ((stream test-stream))
  (record-invocation :stream-read-char-no-hang stream)
  nil)

(defmethod nt-gray:stream-peek-char ((stream test-stream))
  (record-invocation :stream-peek-char stream)
  #\b)

(defmethod nt-gray:stream-listen ((stream test-stream))
  (record-invocation :stream-listen stream)
  t)

(defmethod nt-gray:stream-read-line ((stream test-stream))
  (record-invocation :stream-read-line stream)
  "ab")

(defmethod nt-gray:stream-clear-input ((stream test-stream))
  (record-invocation :stream-clear-input stream)
  nil)

(defmethod nt-gray:stream-write-char ((stream test-stream) char)
  (record-invocation :stream-write-char stream char)
  char)

(defmethod nt-gray:stream-line-column ((stream test-stream))
  (record-invocation :stream-line-column stream)
  0)

(defmethod nt-gray:stream-start-line-p ((stream test-stream))
  (record-invocation :stream-start-line-p stream)
  nil)

(defmethod nt-gray:stream-write-string ((stream test-stream) string &optional start end)
  (record-invocation :stream-write-string stream string start end))

(defmethod nt-gray:stream-terpri ((stream test-stream))
  (record-invocation :stream-terpri stream))

(defmethod nt-gray:stream-fresh-line ((stream test-stream))
  (record-invocation :stream-fresh-line stream))

(defmethod nt-gray:stream-finish-output ((stream test-stream))
  (record-invocation :stream-finish-output stream))

(defmethod nt-gray:stream-force-output ((stream test-stream))
  (record-invocation :stream-force-output stream))

(defmethod nt-gray:stream-clear-output ((stream test-stream))
  (record-invocation :stream-clear-output stream))

(defmethod nt-gray:stream-advance-to-column ((stream test-stream) column)
  (record-invocation :stream-advance-to-column stream column))

(defmethod nt-gray:stream-read-byte ((stream test-stream))
  (record-invocation :stream-read-byte stream))

(defmethod nt-gray:stream-write-byte ((stream test-stream) byte)
  (record-invocation ':stream-write-byte stream byte))

(define-invoked-test invoke.stream-read-char
    (read-char stream)
  (nt-gray:stream-read-char stream))

(define-invoked-test invoke.stream-unread-char
    (unread-char #\a stream)
  (nt-gray:stream-unread-char stream #\a))

(define-invoked-test invoke.stream-read-char-no-hang
    (read-char-no-hang stream)
  (nt-gray:stream-read-char-no-hang stream))

(define-invoked-test invoke.stream-peek-char
    (peek-char nil stream)
  (nt-gray:stream-peek-char stream))

(define-invoked-test invoke.stream-listen
    (listen stream)
  (nt-gray:stream-listen stream))

(define-invoked-test invoke.stream-read-line
    (read-line stream)
  (nt-gray:stream-read-line stream))

(define-invoked-test invoke.clear-input
    (clear-input stream)
  (nt-gray:stream-clear-input stream))

(define-invoked-test invoke.write-char
    (write-char #\a stream)
  (nt-gray:stream-write-char stream #\a))

(define-invoked-test invoke.stream-line-column
    (format stream "~10,t")
  (nt-gray:stream-line-column stream))

(define-invoked-test invoke.stream-start-line-p
    (fresh-line stream)
  (nt-gray:stream-start-line-p stream))

(define-invoked-test invoke.stream-write-string
    (write-string "hello" stream :start 1 :end 4)
  (nt-gray:stream-write-string stream "hello" 1 4))

(define-invoked-test invoke.stream-terpri
    (terpri stream)
  (nt-gray:stream-terpri stream))

(define-invoked-test invoke.stream-fresh-line
    (fresh-line stream)
  (nt-gray:stream-fresh-line stream))

(define-invoked-test invoke.stream-finish-output
    (finish-output stream)
  (nt-gray:stream-finish-output stream))

(define-invoked-test invoke.stream-force-output
    (force-output stream)
  (nt-gray:stream-force-output stream))

(define-invoked-test invoke.stream-clear-output
    (clear-output stream)
  (nt-gray:stream-clear-output stream))

(define-invoked-test invoke.stream-advance-to-column
    (format stream "~10,t")
  (nt-gray:stream-advance-to-column stream 10))

(define-invoked-test invoke.stream-read-byte
    (read-byte stream)
  (nt-gray:stream-read-byte stream))

(define-invoked-test invoke.stream-write-byte
    (write-byte 1 stream)
  (nt-gray:stream-write-byte stream 1))
|#
