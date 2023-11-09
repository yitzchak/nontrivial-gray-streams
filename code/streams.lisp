(in-package #:nontrivial-gray-streams)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or abcl allegro clisp clasp cmucl ecl genera lispworks mezzano mkcl mocl sbcl)
  (pushnew :gray-streams-sequence *features*)

  #+(or abcl allegro clasp cmucl ecl genera mezzano mkcl mocl sbcl)
  (pushnew :gray-streams-sequence-optional *features*)

  #+clisp
  (pushnew :gray-streams-sequence-key *features*)

  #+ccl
  (pushnew :gray-streams-vector *features*))
