(in-package #:cl-user)

#+(and (or ecl clasp) (not gray-streams-module))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(defpackage #:nontrivial-gray-streams
  (:use #:common-lisp)
  (:nicknames #:nt-gray)
  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
  (:import-from #+abcl #:gray-streams
                #+allegro #:excl
                #+ccl #:ccl
                #+clasp #:gray
                #+clisp #:gray
                #+cmucl #:ext
                #+ecl #:gray
                #+genera #:gray-streams
                #+lispworks #:stream
                #+mezzano #:mezzano.gray
                #+mkcl #:gray
                #+mocl #:gray
                #+sicl #:cyclosis
                #+sbcl #:sb-gray
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream
                #:fundamental-binary-stream
                #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:fundamental-character-stream
                #:fundamental-input-stream
                #:fundamental-output-stream
                #:fundamental-stream
                #:stream-advance-to-column
                #:stream-clear-input
                #:stream-clear-output
                #+(or mezzano sicl)
                #:stream-file-length
                #+(or abcl allegro clasp cmucl ecl general lispworks mezzano mkcl mocl sicl sbcl)
                #:stream-file-position
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-line-column
                #:stream-listen
                #:stream-peek-char
                #+(or ccl clisp)
                #:stream-position
                #:stream-read-byte
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-line
                #+(or abcl allegro clisp clasp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
                #:stream-read-sequence
                #+ccl
                #:stream-read-vector
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-byte
                #:stream-write-char
                #+(or abcl allegro clisp clasp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
                #:stream-write-sequence
                #+ccl
                #:stream-write-vector
                #:stream-write-string)
  (:export #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-character-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-stream
           #:stream-advance-to-column
           #:stream-clear-input
           #:stream-clear-output
           #+(or mezzano sicl)
           #:stream-file-length
           #+(or abcl allegro clasp cmucl ecl general lispworks mezzano mkcl mocl sicl sbcl)
           #:stream-file-position
           #:stream-finish-output
           #:stream-force-output
           #:stream-fresh-line
           #:stream-line-column
           #:stream-listen
           #:stream-peek-char
           #+(or ccl clisp)
           #:stream-position
           #:stream-read-byte
           #:stream-read-char
           #:stream-read-char-no-hang
           #:stream-read-line
           #+(or abcl allegro clisp clasp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
           #:stream-read-sequence
           #+ccl
           #:stream-read-vector
           #:stream-start-line-p
           #:stream-terpri
           #:stream-unread-char
           #:stream-write-byte
           #:stream-write-char
           #+(or abcl allegro clisp clasp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
           #:stream-write-sequence
           #+ccl
           #:stream-write-vector
           #:stream-write-string))
