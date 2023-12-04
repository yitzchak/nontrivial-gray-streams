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

#+cmucl
(defmethod input-stream-p (stream)
  (declare (ignore stream))
  nil)

#+cmucl
(defmethod output-stream-p (stream)
  (declare (ignore stream))
  nil)

#+(or allegro clasp cmucl ecl mkcl sbcl)
(defmethod stream-clear-input (stream)
  nil)

#+(or abcl clasp ecl mkcl)
(defmethod interactive-stream-p ((stream fundamental-input-stream))
  nil)
