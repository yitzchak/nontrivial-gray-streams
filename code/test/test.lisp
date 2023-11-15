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
          :initform 0)
   (interactive :reader interactive-p
                :initform nil
                :initarg :interactive)))

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

#+gray-streams-file-length
(defmethod nt-gray:stream-file-length ((stream test-string-input-stream))
  (record-invocation :stream-file-length stream)
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

#+gray-streams-file-position/variant-3
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
      (true (invoked-p :stream-file-length stream)))))

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

(defclass test-string-output-stream
    (nt-gray:fundamental-character-output-stream)
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

(defmethod nt-gray:stream-line-column ((stream test-string-output-stream))
  (record-invocation :stream-line-column stream)
  (line-column stream))

(defmethod nt-gray:stream-start-line-p ((stream test-string-output-stream))
  (record-invocation :stream-start-line-p stream)
  (zerop (line-column stream)))

(defmethod nt-gray:stream-write-char ((stream test-string-output-stream) char)
  (record-invocation :stream-write-char stream char)
  (vector-push-extend char (value stream))
  (if (char= char #\Newline)
      (setf (line-column stream) 0)
      (incf (line-column stream)))
  char)

(defmethod nt-gray:stream-write-string
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
          (adjust-array value end1 :fill-pointer end1)
          (setf (fill-pointer value) end1))
      (replace value sequence
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(defmethod nt-gray:stream-terpri ((stream test-string-output-stream))
  (record-invocation :stream-terpri stream)
  (vector-push-extend #\Newline (value stream))
  (setf (line-column stream) 0)
  nil)

(defmethod nt-gray:stream-fresh-line ((stream test-string-output-stream))
  (record-invocation :stream-fresh-line stream)
  (unless (zerop (line-column stream))
    (vector-push-extend #\Newline (value stream))
    (setf (line-column stream) 0))
  nil)

(defmethod nt-gray:stream-clear-output ((stream test-string-output-stream))
  (record-invocation :stream-clear-output stream)
  nil)

(defmethod nt-gray:stream-finish-output ((stream test-string-output-stream))
  (record-invocation :stream-finish-output stream)
  nil)

(defmethod nt-gray:stream-force-output ((stream test-string-output-stream))
  (record-invocation :stream-force-output stream)
  nil)

(defmethod nt-gray:close ((stream test-string-output-stream) &key abort)
  (record-invocation :close stream)
  (cond ((openp stream)
         (clear-output stream)
         (setf (openp stream) nil)
         t)
        (t
         nil)))

#+gray-streams-line-length
(defmethod nt-gray:stream-line-length ((stream test-string-output-stream))
  (record-invocation :stream-line-length stream)
  (line-length stream))

#+gray-streams-sequence
(defmethod nt-gray:stream-write-sequence
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
          (adjust-array value end1 :fill-pointer end1)
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
      (true (invoked-p :stream-write-sequence stream "abcd" 0 nil))
      (true (invoked-p :stream-write-sequence stream "efgh" 1 3)))))

#+gray-streams-line-length
(define-test character-output.line-length.01
  (with-invocations
    (let ((stream (make-instance 'test-string-output-stream))
          (*print-right-margin* nil)
          (*print-pretty* nil))
      (format stream "~<aaaa~:;bbbb~>")
      (true (invoked-p :stream-line-length stream)))))
