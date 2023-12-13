(in-package #:cl-user)

#+(and (or ecl clasp) (not gray-streams-module))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+clisp
  (pushnew :gray-streams-element-type *features*)

  #+clasp
  (when (fboundp '(setf gray:stream-element-type))
    (pushnew :gray-streams-element-type *features*))

  #+(or abcl ccl clasp ecl lispworks mezzano mkcl sicl)
  (pushnew :gray-streams-streamp *features*)

  #+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-input-stream-p *features*)

  #+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-output-stream-p *features*)

  #+ccl
  (pushnew :gray-streams-pathname *features*)

  #+clasp
  (when (typep (fdefinition 'cl:pathname) 'generic-function)
    (pushnew :gray-streams-pathname *features*))

  #+clasp
  (when (typep (fdefinition 'cl:truename) 'generic-function)
    (pushnew :gray-streams-truename *features*))

  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-sequence *features*)

  #+(or abcl allegro clasp cmucl ecl genera mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-sequence/variant-1 *features*)

  #+(or ccl lispworks)
  (pushnew :gray-streams-sequence/variant-2 *features*)

  #+clisp
  (pushnew :gray-streams-sequence/variant-3 *features*)

  #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-file-position *features*)

  #+(or abcl allegro ccl clasp ecl mezzano mkcl sicl sbcl)
  (pushnew :gray-streams-file-position/variant-1 *features*)

  #+clisp
  (pushnew :gray-streams-file-position/variant-2 *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position/variant-3 *features*)

  #+(or cmucl genera lispworks)
  (pushnew :gray-streams-file-position/variant-4 *features*)

  #+(or abcl clasp cmucl ecl)
  (when (find-symbol (string '#:stream-file-length)
                     #+abcl '#:gray-streams
                     #+cmucl '#:ext
                     #+(or clasp ecl) '#:gray)
    (pushnew :gray-streams-file-length *features*)
    (pushnew :gray-streams-file-length/variant-3 *features*))

  #+(or ccl mezzano sicl)
  (pushnew :gray-streams-file-length *features*)

  #+ccl
  (pushnew :gray-streams-file-length/variant-1 *features*)

  #+(or mezzano sicl)
  (pushnew :gray-streams-file-length/variant-3 *features*)

  #+ccl
  (pushnew :gray-streams-external-format *features*)

  #+ccl
  (pushnew :gray-streams-external-format/variant-3 *features*)

  #+ccl
  (pushnew :gray-streams-external-format/variant-4 *features*)

  #+abcl
  (when (find-symbol (string '#:gray-interactive-stream-p)
                     '#:gray-streams)
    (pushnew :gray-streams-interactive *features*))

  #+(or ccl clasp ecl mezzano mkcl sbcl sicl)
  (pushnew :gray-streams-interactive *features*)

  #+(or abcl ecl)
  (when (find-symbol (string '#:stream-line-length)
                     #+abcl '#:gray-streams
                     #+ecl '#:gray)
    (pushnew :gray-streams-line-length *features*))

  #+(or ccl clasp cmucl mezzano lispworks sbcl sicl)
  (pushnew :gray-streams-line-length *features*))

(defpackage #:nontrivial-gray-streams
  (:use #:common-lisp)
  (:nicknames :ngray)
  #+(or ccl clasp ecl mkcl)
  (:shadow #+(or clasp ecl mkcl)
           #:interactive-stream-p
           #+ccl
           #:pathname)
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
                #+gray-streams-external-format
                #:stream-external-format
                #+(and gray-streams-file-length
                       (not ccl))
                #:stream-file-length
                #+(and gray-streams-file-position
                       (not (or ccl clisp)))
                #:stream-file-position
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
                #+gray-streams-line-length
                #:stream-line-length
                #:stream-listen
                #+lispworks
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
