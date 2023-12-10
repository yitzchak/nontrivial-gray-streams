(in-package #:nontrivial-gray-streams/test)

(defclass invocation-mixin ()
  ((invocations :accessor invocations
                :initform (make-array 10
                                      :fill-pointer 0
                                      :adjustable t
                                      :initial-element nil
                                      :element-type 'list))))

(defun invoked-p (stream &rest args)
  (and (position args (invocations stream) :test #'equalp) t))

(defmethod ngray:stream-read-char :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-read-char stream)
                      (invocations stream)))

(defmethod ngray:stream-unread-char :before ((stream invocation-mixin) character)
  (vector-push-extend (list :stream-unread-char stream character)
                      (invocations stream)))

(defmethod ngray:stream-read-char-no-hang :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-read-char-no-hang stream)
                      (invocations stream)))

(defmethod ngray:stream-peek-char :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-peek-char stream)
                      (invocations stream)))

(defmethod ngray:stream-listen :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-listen stream)
                      (invocations stream)))

(defmethod ngray:stream-read-line :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-read-line stream)
                      (invocations stream)))

(defmethod ngray:stream-clear-input :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-clear-input stream)
                      (invocations stream)))

(defmethod ngray:stream-write-char :before ((stream invocation-mixin) char)
  (vector-push-extend (list :stream-write-char stream char)
                      (invocations stream)))

(defmethod ngray:stream-line-column :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-line-column stream)
                      (invocations stream)))

(defmethod ngray:stream-start-line-p :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-start-line-p stream)
                      (invocations stream)))

(defmethod ngray:stream-write-string :before ((stream invocation-mixin) string &optional start end)
  (vector-push-extend (list :stream-write-string stream string start end)
                      (invocations stream)))

(defmethod ngray:stream-terpri :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-terpri stream)
                      (invocations stream)))

(defmethod ngray:stream-fresh-line :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-fresh-line stream)
                      (invocations stream)))

(defmethod ngray:stream-finish-output :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-finish-output stream)
                      (invocations stream)))

(defmethod ngray:stream-force-output :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-force-output stream)
                      (invocations stream)))

(defmethod ngray:stream-clear-output :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-clear-output stream)
                      (invocations stream)))

(defmethod ngray:stream-advance-to-column :before ((stream invocation-mixin) column)
  (vector-push-extend (list :stream-advance-to-column stream column)
                      (invocations stream)))

(defmethod ngray:close :before ((stream invocation-mixin) &key abort)
  (vector-push-extend (list :close stream abort)
                      (invocations stream)))

(defmethod ngray:open-stream-p :before ((stream invocation-mixin))
  (vector-push-extend (list :open-stream-p stream)
                      (invocations stream)))

(defmethod ngray:stream-element-type :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-element-type stream)
                      (invocations stream)))

#+gray-streams-streamp
(defmethod ngray:streamp :before ((stream invocation-mixin))
  (vector-push-extend (list :streamp stream)
                      (invocations stream)))

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p :before ((stream invocation-mixin))
  (vector-push-extend (list :input-stream-p stream)
                      (invocations stream)))

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p :before ((stream invocation-mixin))
  (vector-push-extend (list :output-stream-p stream)
                      (invocations stream)))

#+gray-streams-pathname
(defmethod ngray:pathname :before ((stream invocation-mixin))
  (vector-push-extend (list :pathname stream)
                      (invocations stream)))

#+gray-streams-truename
(defmethod ngray:truename :before ((stream invocation-mixin))
  (vector-push-extend (list :truename stream)
                      (invocations stream)))

(defmethod ngray:stream-read-byte :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-read-byte stream)
                      (invocations stream)))

(defmethod ngray:stream-write-byte :before ((stream invocation-mixin) integer)
  (vector-push-extend (list :stream-write-byte stream integer)
                      (invocations stream)))

#+gray-streams-element-type
(defmethod (setf ngray:stream-element-type) :before (new-value (stream invocation-mixin))
  (vector-push-extend (list :stream-element-type stream new-value)
                      (invocations stream)))

#+gray-streams-external-format
(defmethod ngray:stream-external-format :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-external-format stream)
                      (invocations stream)))

#+gray-streams-external-format
(defmethod (setf ngray:stream-external-format) :before (new-value (stream invocation-mixin))
  (vector-push-extend (list :stream-external-format stream new-value)
                      (invocations stream)))

#+gray-streams-interactive
(defmethod ngray:interactive-stream-p :before ((stream invocation-mixin))
  (vector-push-extend (list :interactive-stream-p stream)
                      (invocations stream)))

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence :before
    #+gray-streams-sequence/variant-1
    ((stream invocation-mixin) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream invocation-mixin) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream invocation-mixin) &key start end)
  (vector-push-extend (list :stream-read-sequence stream sequence start end)
                      (invocations stream)))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence :before
    #+gray-streams-sequence/variant-1
    ((stream invocation-mixin) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream invocation-mixin) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream invocation-mixin) &key start end)
  (vector-push-extend (list :stream-write-sequence stream sequence start end)
                      (invocations stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod ngray:stream-file-position :before
    ((stream invocation-mixin)
     #+gray-streams-file-position/variant-1 &optional position)
  (vector-push-extend (list :stream-file-position stream position)
                      (invocations stream)))

#+gray-streams-file-position/variant-3
(defmethod ngray:stream-file-position :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-file-position stream nil)
                      (invocations stream)))

#+gray-streams-file-position/variant-4
(defmethod (setf ngray:stream-file-position) :before (position (stream invocation-mixin))
  (vector-push-extend (list :stream-file-position stream position)
                      (invocations stream)))

#+(or gray-streams-file-length/variant-1
      gray-streams-file-length/variant-2)
(defmethod ngray:stream-file-length :before
    ((stream invocation-mixin)
     #+gray-streams-file-length/variant-1 &optional length)
  (vector-push-extend (list :stream-file-length stream length)
                      (invocations stream)))

#+gray-streams-file-length/variant-3
(defmethod ngray:stream-file-length :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-file-length stream nil)
                      (invocations stream)))

#+gray-streams-file-length/variant-4
(defmethod (setf ngray:stream-file-length) :before (length (stream invocation-mixin))
  (vector-push-extend (list :stream-file-length stream length)
                      (invocations stream)))

#+gray-streams-line-length
(defmethod ngray:stream-line-length :before ((stream invocation-mixin))
  (vector-push-extend (list :stream-line-length stream)
                      (invocations stream)))

(defclass stream-mixin-b ()
  ((openp :accessor openp
          :initform t)
   (pathname :accessor %pathname
             :initarg :pathname
             :initform nil)
   (truename :accessor %truename
             :initarg :truename
             :initform nil)))

(defmethod ngray:close ((stream stream-mixin-b) &key abort)
  (declare (ignore abort))
  (cond ((openp stream)
         (setf (openp stream) nil)
         t)
        (t
         nil)))

#+gray-streams-pathname
(defmethod ngray:pathname ((stream stream-mixin-b))
  (%pathname stream))

#+gray-streams-truename
(defmethod ngray:truename ((stream stream-mixin-b))
  (%truename stream))

(defclass interactive-mixin ()
  ((interactive :reader interactive-p
                :initform nil
                :initarg :interactive)))

#+gray-streams-interactive
(defmethod ngray:interactive-stream-p ((stream interactive-mixin))
  (interactive-p stream))

(defclass binary-input-mixin-a (invocation-mixin)
  ((input-value :accessor input-value
                :initarg :input-value
                :initform "")
   (input-index :accessor input-index
                :initarg :input-index
                :initform 0)))

(defmethod ngray:stream-read-byte ((stream binary-input-mixin-a))
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (char-code (char input-value input-index))
          (incf input-index))
        :eof)))

(defmethod ngray:stream-listen ((stream binary-input-mixin-a))
  (< (input-index stream) (length (input-value stream))))

(defmethod ngray:stream-element-type ((stream binary-input-mixin-a))
  'integer)

(defclass binary-input-mixin-b (binary-input-mixin-a
                                interactive-mixin
                                stream-mixin-b)
  ())

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-input-mixin-b))
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-input-mixin-b))
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-input-mixin-b))
  nil)

(defmethod ngray:stream-clear-input ((stream binary-input-mixin-b))
  (setf (input-index stream) 0
        (input-value stream) "")
  nil)

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence
    #+gray-streams-sequence/variant-1
    ((stream binary-input-mixin-b) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream binary-input-mixin-b) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream binary-input-mixin-b) &key start end)
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
(defmethod ngray:stream-file-length ((stream binary-input-mixin-b))
  (length (input-value stream)))

#+gray-streams-file-length/variant-1
(defmethod ngray:stream-file-length ((stream binary-input-mixin-b) &optional length)
  (declare (ignore length))
  (length (input-value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod ngray:stream-file-position
    ((stream binary-input-mixin-b)
     #+gray-streams-file-position/variant-1 &optional position)
  (if position
      (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
        (assert (typep position typespec) (position)
                'type-error :datum position :expected-type typespec)
        (setf (input-index stream) position)
        t)
      (input-index stream)))

#+gray-streams-file-position/variant-3
(defmethod ngray:stream-file-position ((stream binary-input-mixin-b))
  (input-index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf ngray:stream-file-position) (position (stream binary-input-mixin-b))
  (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (input-index stream) position)))

(defclass binary-output-mixin-a (invocation-mixin)
  ((output-value :accessor output-value
                 :initform (make-array 16 :element-type 'character
                                          :adjustable t :fill-pointer 0))))

(defmethod ngray:stream-write-byte ((stream binary-output-mixin-a) byte)
  (vector-push-extend (code-char byte) (output-value stream))
  byte)

(defmethod ngray:stream-element-type ((stream binary-output-mixin-a))
  'integer)

(defclass binary-output-mixin-b (binary-output-mixin-a
                                 stream-mixin-b)
  ())

(defmethod ngray:stream-clear-output ((stream binary-output-mixin-b))
  nil)

(defmethod ngray:stream-finish-output ((stream binary-output-mixin-b))
  nil)

(defmethod ngray:stream-force-output ((stream binary-output-mixin-b))
  nil)

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
    #+gray-streams-sequence/variant-1
    ((stream binary-output-mixin-b) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream binary-output-mixin-b) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream binary-output-mixin-b) &key start end)
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
      (replace output-value (map 'string #'code-char sequence)
               :start1 start1 :end1 end1
               :start2 start :end2 end))))

(defclass binary-io-mixin-a (binary-input-mixin-a
                             binary-output-mixin-a)
  ())

(defclass binary-io-mixin-b (binary-io-mixin-a
                             binary-input-mixin-b
                             binary-output-mixin-b)
  ())

#+gray-streams-streamp
(defmethod ngray:streamp ((stream binary-io-mixin-b))
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream binary-io-mixin-b))
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream binary-io-mixin-b))
  t)

(defclass character-input-mixin-a (invocation-mixin)
  ((input-value :accessor input-value
                :initarg :input-value
                :initform "")
   (input-index :accessor input-index
                :initarg :input-index
                :initform 0)))

(defmethod ngray:stream-read-char ((stream character-input-mixin-a))
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (char input-value input-index)
          (incf input-index))
        :eof)))

(defmethod ngray:stream-unread-char ((stream character-input-mixin-a) character)
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (when (zerop input-index)
      (error "Stream is at beginning, cannot unread character"))
    (when (char/= character (char input-value (decf input-index)))
      (error "Cannot unread a character that does not match."))
    nil))

(defclass character-input-mixin-b (character-input-mixin-a
                                   interactive-mixin
                                   stream-mixin-b)
  ())

#+gray-streams-interactive
(defmethod ngray:interactive-stream-p ((stream character-input-mixin-b))
  (interactive-p stream))

(defmethod ngray:stream-read-char-no-hang ((stream character-input-mixin-b))
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (prog1 (char input-value input-index)
          (incf input-index))
        (if (interactive-p stream) nil :eof))))

(defmethod ngray:stream-peek-char ((stream character-input-mixin-b))
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< input-index (length input-value))
        (char input-value input-index)
        :eof)))

(defmethod ngray:stream-listen ((stream character-input-mixin-b))
  (< (input-index stream) (length (input-value stream))))

(defmethod ngray:stream-read-line ((stream character-input-mixin-b))
  (with-accessors ((input-value input-value)
                   (input-index input-index))
      stream
    (if (< (input-index stream) (length (input-value stream)))
        (let ((pos (position #\Newline input-value :start input-index)))
          (if pos
              (multiple-value-prog1
                  (values (subseq input-value input-index pos) nil)
                (setf input-index (1+ pos)))
              (multiple-value-prog1
                  (values (subseq input-value input-index) t)
                (setf input-index (length input-value)))))
        (values "" t))))

(defmethod ngray:stream-clear-input ((stream character-input-mixin-b))
  (setf (input-index stream) 0
        (input-value stream) "")
  nil)

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence
    #+gray-streams-sequence/variant-1
    ((stream character-input-mixin-b) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream character-input-mixin-b) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream character-input-mixin-b) &key start end)
  (unless end
    (setf end (length sequence)))
  (prog ((input-index (or start 0)) ch)
   next
     (when (< input-index end)
       (setf ch (ngray:stream-read-char stream))
       (unless (eq ch :eof)
         (setf (elt sequence input-index) ch)
         (incf input-index)
         (go next)))
     (return input-index)))

#+gray-streams-file-length/variant-3
(defmethod ngray:stream-file-length ((stream character-input-mixin-b))
  (length (input-value stream)))

#+gray-streams-file-length/variant-1
(defmethod ngray:stream-file-length ((stream character-input-mixin-b) &optional length)
  (length (input-value stream)))

#+(or gray-streams-file-position/variant-1
      gray-streams-file-position/variant-2)
(defmethod ngray:stream-file-position
    ((stream character-input-mixin-b)
     #+gray-streams-file-position/variant-1 &optional position)
  (if position
      (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
        (assert (typep position typespec) (position)
                'type-error :datum position :expected-type typespec)
        (setf (input-index stream) position)
        t)
      (input-index stream)))

#+gray-streams-file-position/variant-3
(defmethod ngray:stream-file-position ((stream character-input-mixin-b))
  (input-index stream))

#+gray-streams-file-position/variant-4
(defmethod (setf ngray:stream-file-position) (position (stream character-input-mixin-b))
  (let ((typespec `(integer 0 ,(1- (length (input-value stream))))))
    (assert (typep position typespec) (position)
            'type-error :datum position :expected-type typespec)
    (setf (input-index stream) position)))

(defclass character-output-mixin-a (invocation-mixin)
  ((output-value :accessor output-value
                 :initform (make-array 16 :element-type 'character
                                          :adjustable t :fill-pointer 0))))

(defmethod ngray:stream-write-char ((stream character-output-mixin-a) char)
  (vector-push-extend char (output-value stream))
  char)

(defmethod ngray:stream-line-column ((stream character-output-mixin-a))
  nil)

(defclass character-output-mixin-b (character-output-mixin-a
                                    stream-mixin-b)
  ((line-column :accessor line-column
                :initform 0)
   (line-length :reader line-length
                :initform nil
                :initarg :line-length)))

(defmethod ngray:stream-line-column ((stream character-output-mixin-b))
  (line-column stream))

(defmethod ngray:stream-start-line-p ((stream character-output-mixin-b))
  (zerop (ngray:stream-line-column stream)))

(defmethod ngray:stream-write-char ((stream character-output-mixin-b) char)
  (vector-push-extend char (output-value stream))
  (if (char= char #\Newline)
      (setf (line-column stream) 0)
      (incf (line-column stream)))
  char)

(defmethod ngray:stream-write-string
    ((stream character-output-mixin-b) sequence &optional start end)
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

(defmethod ngray:stream-terpri ((stream character-output-mixin-b))
  (ngray:stream-write-char stream #\Newline)
  nil)

(defmethod ngray:stream-fresh-line ((stream character-output-mixin-b))
  (unless (zerop (line-column stream))
    (ngray:stream-write-char stream #\Newline)
    t))

(defmethod ngray:stream-clear-output ((stream character-output-mixin-b))
  nil)

(defmethod ngray:stream-finish-output ((stream character-output-mixin-b))
  nil)

(defmethod ngray:stream-force-output ((stream character-output-mixin-b))
  nil)

(defmethod ngray:stream-advance-to-column ((stream character-output-mixin-b) column)
  (prog ((current (ngray:stream-line-column stream)))
   repeat
     (when (and current (< column current))
       (ngray:stream-write-char stream #\Space)
       (setf current (ngray:stream-line-column stream))
       (go repeat))))

#+gray-streams-line-length
(defmethod ngray:stream-line-length ((stream character-output-mixin-b))
  (or (line-length stream)
      (call-next-method)))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
    #+gray-streams-sequence/variant-1
    ((stream character-output-mixin-b) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream character-output-mixin-b) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream character-output-mixin-b) &key start end)
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

(defclass character-io-mixin-a (character-input-mixin-a
                                character-output-mixin-a)
  ())

(defclass character-io-mixin-b (character-io-mixin-a
                                character-input-mixin-b
                                character-output-mixin-b)
  ())

#+gray-streams-streamp
(defmethod ngray:streamp ((stream character-io-mixin-b))
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream character-io-mixin-b))
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream character-io-mixin-b))
  t)

(defclass bivalent-mixin-a ()
  ((element-type :accessor element-type
                 :initarg :element-type
                 :initform 'character)))

(defun character-stream-p (stream)
  (subtypep (element-type stream) 'character))

(defun check-character-stream (stream)
  (unless (character-stream-p stream)
    (error "Not a character stream")))

(defun binary-stream-p (stream)
  (subtypep (element-type stream) 'integer))

(defun check-binary-stream (stream)
  (unless (binary-stream-p stream)
    (error "Not a binary stream")))

(defmethod ngray:stream-element-type ((stream bivalent-mixin-a))
  (element-type stream))

#+gray-streams-element-type
(defmethod (setf ngray:stream-element-type) (new-value (stream bivalent-mixin-a))
  (setf (element-type stream) new-value))

(defclass bivalent-input-mixin-a (bivalent-mixin-a
                                  character-input-mixin-a
                                  binary-input-mixin-a)
  ())

(defmethod ngray:stream-read-byte ((stream bivalent-input-mixin-a))
  (check-binary-stream stream)
  (call-next-method))

(defmethod ngray:stream-read-char ((stream bivalent-input-mixin-a))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-unread-char ((stream bivalent-input-mixin-a) char)
  (check-character-stream stream)
  (call-next-method))

#+gray-streams-sequence
(defmethod ngray:stream-read-sequence
    #+gray-streams-sequence/variant-1
    ((stream bivalent-input-mixin-a) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream bivalent-input-mixin-a) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream bivalent-input-mixin-a) &key start end)
  (cond ((character-stream-p stream)
         (call-next-method))
        ((binary-stream-p stream)
         (unless end
           (setf end (length sequence)))
         (prog ((input-index (or start 0)) value)
          next
            (when (< input-index end)
              (setf value (ngray:stream-read-byte stream))
              (unless (eq value :eof)
                (setf (elt sequence input-index) value)
                (incf input-index)
                (go next)))
            (return input-index)))
        (t
         (error "Unknown stream element type"))))

(defclass bivalent-input-mixin-b (bivalent-input-mixin-a
                                  character-input-mixin-b
                                  binary-input-mixin-b)
  ())

(defmethod ngray:stream-read-char-no-hang ((stream bivalent-input-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-peek-char ((stream bivalent-input-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-read-line ((stream bivalent-input-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defclass bivalent-output-mixin-a (bivalent-mixin-a
                                  character-output-mixin-a
                                  binary-output-mixin-a)
  ())

(defmethod ngray:stream-write-byte ((stream bivalent-output-mixin-a) byte)
  (check-binary-stream stream)
  (call-next-method))

(defmethod ngray:stream-write-char ((stream bivalent-output-mixin-a) char)
  (check-character-stream stream)
  (call-next-method))

#+gray-streams-sequence
(defmethod ngray:stream-write-sequence
    #+gray-streams-sequence/variant-1
    ((stream bivalent-output-mixin-a) sequence &optional start end)
    #+gray-streams-sequence/variant-2
    ((stream bivalent-output-mixin-a) sequence start end)
    #+gray-streams-sequence/variant-3
    (sequence (stream bivalent-output-mixin-a) &key start end)
  (cond ((character-stream-p stream)
         (call-next-method))
        ((binary-stream-p stream)
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
             (replace output-value (map 'string #'code-char sequence)
                      :start1 start1 :end1 end1
                      :start2 start :end2 end))))
        (t
         (error "Unknown stream element type"))))

(defmethod ngray:stream-advance-to-column ((stream bivalent-output-mixin-a) column)
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-start-line-p ((stream bivalent-output-mixin-a))
  (check-character-stream stream)
  (call-next-method))

(defclass bivalent-output-mixin-b (bivalent-output-mixin-a
                                   character-output-mixin-b
                                   binary-output-mixin-b)
  ())

(defmethod ngray:stream-line-column ((stream bivalent-output-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-fresh-line ((stream bivalent-output-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-terpri ((stream bivalent-output-mixin-b))
  (check-character-stream stream)
  (call-next-method))

(defmethod ngray:stream-write-string ((stream bivalent-output-mixin-b) string &optional start end)
  (declare (ignore string start end))
  (check-character-stream stream)
  (call-next-method))

(defclass bivalent-io-mixin-a (bivalent-input-mixin-a
                               bivalent-output-mixin-a)
  ())

(defclass bivalent-io-mixin-b (bivalent-io-mixin-a
                               bivalent-input-mixin-b
                               bivalent-output-mixin-b)
  ())

#+gray-streams-streamp
(defmethod ngray:streamp ((stream bivalent-io-mixin-b))
  t)

#+gray-streams-input-stream-p
(defmethod ngray:input-stream-p ((stream bivalent-io-mixin-b))
  t)

#+gray-streams-output-stream-p
(defmethod ngray:output-stream-p ((stream bivalent-io-mixin-b))
  t)
