(in-package #:nontrivial-gray-streams)

#+(or ccl clisp)
(setf (fdefinition 'stream-file-position) #'stream-position)

#+ccl
(setf (fdefinition 'stream-read-sequence) #'stream-read-vector
      (fdefinition 'stream-write-sequence) #'stream-write-vector
      (fdefinition 'stream-file-length) #'stream-length
      (fdefinition 'pathname) #'stream-filename)

#+(or clasp ecl mkcl)
(setf (fdefinition 'nontrivial-gray-streams:interactive-stream-p)
      #'stream-interactive-p)

#+lispworks
(setf (fdefinition 'nontrivial-gray-streams:stream-line-length)
      #'stream-output-width)

#+allegro
(with-compilation-unit (:override t)
  ;; Allegro CL is missing this default method
  (defmethod stream-read-char-no-hang
      ((stream fundamental-character-input-stream))
    (stream-read-char stream))

  ;; Allegro CL default method is incorrect
  (defmethod stream-start-line-p
      ((stream fundamental-character-output-stream))
    (equal (stream-line-column stream) 0)))

#+(or allegro clasp ecl mkcl sbcl)
(defmethod stream-clear-input (stream)
  nil)

#+(or abcl clasp ecl mkcl)
(defmethod interactive-stream-p ((stream fundamental-input-stream))
  nil)

#+cmucl
(when (< c::byte-fasl-file-version #x21f)
  (defmethod input-stream-p (stream)
    (declare (ignore stream))
    nil)

  (defmethod output-stream-p (stream)
    (declare (ignore stream))
    nil)

  (defmethod stream-clear-input ((stream fundamental-input-stream))
    nil)

  (defmethod stream-read-sequence
      ((stream fundamental-character-input-stream) sequence &optional start end)
    (prog ((pos (or start 0))
           (end (or end (length sequence)))
           value)
       (declare (fixnum pos end))
     next
       (when (< pos end)
         (setf value (stream-read-char stream))
         (unless (eq value :eof)
           (setf (elt sequence pos) value)
           (incf pos)
           (go next)))
       (return pos)))

  (defmethod stream-read-sequence
      ((stream fundamental-binary-input-stream) sequence &optional start end)
    (prog ((pos (or start 0))
           (end (or end (length sequence)))
           value)
       (declare (fixnum pos end))
     next
       (when (< pos end)
         (setf value (stream-read-byte stream))
         (unless (eq value :eof)
           (setf (elt sequence pos) value)
           (incf pos)
           (go next)))
       (return pos)))

  (defmethod stream-write-sequence
      ((stream fundamental-character-output-stream) sequence &optional start end)
    (prog ((pos (or start 0))
           (end (or end (length sequence))))
       (declare (fixnum pos end))
     next
       (when (< pos end)
         (stream-write-char stream (elt sequence pos))
         (incf pos)
         (go next)))
    sequence)

  (defmethod stream-write-sequence
      ((stream fundamental-binary-output-stream) sequence &optional start end)
    (prog ((pos (or start 0))
           (end (or end (length sequence))))
       (declare (fixnum pos end))
     next
       (when (< pos end)
         (stream-write-byte stream (elt sequence pos))
         (incf pos)
         (go next)))
    sequence))

#+ccl
(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (and (not (null char))
	 (not (eq char :eof))
	 (progn (stream-unread-char stream char) t))))
