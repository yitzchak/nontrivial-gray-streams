(in-package #:cl-user)

#+(and (or ecl clasp) (not gray-streams-module))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-sequence *features*)

  #+(or abcl allegro clasp cmucl ecl genera mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-sequence-optional *features*)

  #+clisp
  (pushnew :gray-streams-sequence-key *features*)

  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-file-position *features*)

  #+(or abcl allegro ccl clasp ecl mezzano mkcl mocl sicl sbcl)
  (pushnew :gray-streams-file-position-optional *features*)

  #+clisp
  (pushnew :gray-streams-file-position-required *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position-setf *features*)

  #+clasp
  (when (find-symbol (string '#:stream-file-length) '#:gray)
    (pushnew :gray-streams-file-length *features*))

  #+(or mezzano sicl)
  (pushnew :gray-streams-file-length *features*)

  #+(or ccl clasp ecl mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-interactive *features*))

(defpackage #:nontrivial-gray-streams
  (:use #:common-lisp)
  (:nicknames #:nt-gray)
  (:shadow #+(or clasp ecl mkcl)
           #:interactive-stream-p)
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
                #+gray-streams-file-length
                #:stream-file-length
                #+(and gray-streams-file-position
                       (not (or ccl clisp)))
                #:stream-file-position
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #+(or clasp ecl mkcl)
                #:stream-interactive-p
                #:stream-line-column
                #:stream-listen
                #:stream-peek-char
                #+(or ccl clisp)
                #:stream-position
                #:stream-read-byte
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-line
                #+(and gray-streams-sequence (not ccl))
                #:stream-read-sequence
                #+ccl
                #:stream-read-vector
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-byte
                #:stream-write-char
                #+(and gray-streams-sequence (not ccl))
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
           #+gray-streams-file-length
           #:stream-file-length
           #+gray-streams-file-position
           #:stream-file-position
           #:stream-finish-output
           #:stream-force-output
           #:stream-fresh-line
           #+gray-streams-interactive
           #:interactive-stream-p
           #:stream-line-column
           #:stream-listen
           #:stream-peek-char
           #:stream-read-byte
           #:stream-read-char
           #:stream-read-char-no-hang
           #:stream-read-line
           #+gray-streams-sequence
           #:stream-read-sequence
           #:stream-start-line-p
           #:stream-terpri
           #:stream-unread-char
           #:stream-write-byte
           #:stream-write-char
           #+gray-streams-sequence
           #:stream-write-sequence
           #:stream-write-string))
