(in-package #:cl-user)

#+(and (or ecl clasp) (not gray-streams-module))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+clisp
  (pushnew :gray-streams-element-type/setf *features*)

  #+abcl
  (when (fboundp '(setf gray-streams::gray-stream-element-type))
    (pushnew :gray-streams-element-type/setf *features*))

  #+(or clasp ecl)
  (when (fboundp '(setf gray:stream-element-type))
    (pushnew :gray-streams-element-type/setf *features*))

  #+cmucl
  (when (fboundp '(setf stream-element-type))
    (pushnew :gray-streams-element-type/setf *features*))

  #+(or abcl ccl clasp ecl lispworks mezzano mkcl sicl)
  (pushnew :gray-streams-streamp *features*)

  #+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-directionp *features*)

  #+(or ccl mezzano sicl)
  (pushnew :gray-streams-pathname *features*)

  #+(or clasp ecl)
  (when (typep (fdefinition 'cl:pathname) 'generic-function)
    (pushnew :gray-streams-pathname *features*))

  #+(or clasp ecl sicl)
  (when (typep (fdefinition 'cl:truename) 'generic-function)
    (pushnew :gray-streams-truename *features*))

  #+abcl
  (when (find-symbol (string '#:gray-pathname) '#:gray-streams)
    (pushnew :gray-streams-pathname *features*)
    (pushnew :gray-streams-truename *features*))

  #+mezzano
  (pushnew :gray-streams-truename *features*)

  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-sequence *features*)

  #+(or abcl allegro clasp cmucl ecl genera mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-sequence/optional *features*)

  #+(or ccl lispworks)
  (pushnew :gray-streams-sequence/required *features*)

  #+clisp
  (pushnew :gray-streams-sequence/key *features*)

  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-file-position *features*)

  #+(or abcl allegro ccl clasp ecl mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-file-position/optional *features*)

  #+clisp
  (pushnew :gray-streams-file-position/required *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position/get *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position/setf *features*)

  #+(or abcl clasp cmucl ecl sicl)
  (when (find-symbol (string '#:stream-file-length)
                     #+abcl '#:gray-streams
                     #+cmucl '#:ext
                     #+(or clasp ecl) '#:gray)
    (pushnew :gray-streams-file-length *features*)
    (pushnew :gray-streams-file-length/get *features*))

  #+(or ccl mezzano sicl)
  (pushnew :gray-streams-file-length *features*)

  #+ccl
  (pushnew :gray-streams-file-length/optional *features*)

  #+(or mezzano sicl)
  (pushnew :gray-streams-file-length/get *features*)

  #+(or abcl clasp ecl)
  (when (find-symbol (string '#:stream-file-string-length)
                     #+abcl '#:gray-streams
                     #+cmucl '#:ext
                     #+(or clasp ecl) '#:gray)
    (pushnew :gray-streams-file-string-length *features*))

  #+abcl
  (when (find-symbol (string '#:gray-stream-external-format)
                     '#:gray-streams)
    (pushnew :gray-streams-external-format *features*)
    (pushnew :gray-streams-external-format/setf *features*))

  #+(or ccl clasp)
  (progn
    (pushnew :gray-streams-external-format *features*)
    (pushnew :gray-streams-external-format/setf *features*))

  #+(or cmucl ecl)
  (when (typep (fdefinition 'cl:stream-external-format) 'generic-function)
    (pushnew :gray-streams-external-format *features*)
    (pushnew :gray-streams-external-format/setf *features*))

  #+mezzano
  (pushnew :gray-streams-external-format *features*)

  #+abcl
  (when (find-symbol (string '#:gray-interactive-stream-p)
                     '#:gray-streams)
    (pushnew :gray-streams-interactive *features*))

  #+(or ccl clasp ecl mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-interactive *features*)

  #+cmucl
  (when  (typep (fdefinition 'cl:interactive-stream-p) 'generic-function)
    (pushnew :gray-streams-interactive *features*))

  #+(or abcl ecl)
  (when (find-symbol (string '#:stream-line-length)
                     #+abcl '#:gray-streams
                     #+ecl '#:gray)
    (pushnew :gray-streams-line-length *features*))

  #+(or allegro ccl clasp cmucl mezzano lispworks sbcl sicl)
  (pushnew :gray-streams-line-length *features*))

(defpackage #:nontrivial-gray-streams
  (:use #:common-lisp)
  (:nicknames :ngray)
  (:documentation "A compatibility layer for Gray streams including extensions")
  #+(or ccl clasp ecl mezzano mkcl sicl)
  (:shadow #+(or clasp ecl mkcl)
           #:interactive-stream-p
           #+(or ccl mezzano sicl)
           #:pathname
           #+(or mezzano sicl)
           #:truename)
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
                #+(and gray-streams-file-length
                       (not ccl))
                #:stream-file-length
                #+(and gray-streams-file-position
                       (not (or ccl clisp)))
                #:stream-file-position
                #+gray-streams-file-string-length
                #:stream-file-string-length
                #+ccl
                #:stream-filename
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #+(or clasp ecl mkcl)
                #:stream-interactive-p
                #+ccl
                #:stream-length
                #:stream-line-column
                #+(and gray-streams-line-length (not (or allegro lispworks)))
                #:stream-line-length
                #:stream-listen
                #+(or allegro lispworks)
                #:stream-output-width
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
  (:export #:close
           #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-character-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-stream
           #:input-stream-p
           #:open-stream-p
           #:output-stream-p
           #+gray-streams-pathname
           #:pathname
           #:stream-advance-to-column
           #:stream-clear-input
           #:stream-clear-output
           #:stream-element-type
           #+gray-streams-external-format
           #:stream-external-format
           #+gray-streams-file-length
           #:stream-file-length
           #+gray-streams-file-position
           #:stream-file-position
           #+gray-streams-file-string-length
           #:stream-file-string-length
           #:stream-finish-output
           #:stream-force-output
           #:stream-fresh-line
           #+gray-streams-interactive
           #:interactive-stream-p
           #:stream-line-column
           #+gray-streams-line-length
           #:stream-line-length
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
           #:stream-write-string
           #:streamp
           #+gray-streams-truename
           #:truename))
