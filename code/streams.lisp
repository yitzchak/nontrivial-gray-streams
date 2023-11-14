(in-package #:nontrivial-gray-streams)

#+(or ccl clisp)
(setf (fdefinition 'stream-file-position) #'stream-position)

#+ccl
(setf (fdefinition 'stream-read-sequence) #'stream-read-vector
      (fdefinition 'stream-write-sequence) #'stream-write-vector)

#+(or clasp ecl mkcl)
(setf (fdefinition 'nontrivial-gray-streams:interactive-stream-p)
      #'stream-interactive-p)
