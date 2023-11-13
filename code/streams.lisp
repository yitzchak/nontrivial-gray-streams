(in-package #:nontrivial-gray-streams)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or abcl allegro clasp cmucl ecl genera mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-sequence-optional *features*)

  #+lispworks
  (pushnew :gray-streams-sequence-required *features*)

  #+clisp
  (pushnew :gray-streams-sequence-key *features*)

  #+ccl
  (pushnew :gray-streams-vector *features*)

  #+(or abcl allegro ccl clasp ecl mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-file-position-optional *features*)

  #+ccl
  (pushnew :gray-streams-position-optional *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position-setf *features*)

  #+clasp
  (when (find-symbol (string '#:stream-file-length) '#:gray)
    (pushnew :gray-streams-file-length *features*))

  #+(or mezzano sicl)
  (pushnew :gray-streams-file-length *features*))

#+(and clasp gray-streams-file-length)
(progn
  (shadowing-import '(gray:stream-file-length))
  (export '(stream-file-length)))
