# Classes

The following classes are to be used as super classes of user-defined
stream classes.  They are not intended to be directly instantiated;
they just provide places to hang default methods.

## FUNDAMENTAL-STREAM
[Class]

This class is a subclass of STREAM and of STANDARD-OBJECT.  STREAMP
will return true for an instance of any class that includes this.  (It
may return true for some other things also.)

## FUNDAMENTAL-INPUT-STREAM
[Class]

A subclass of FUNDAMENTAL-STREAM.  Its inclusion causes INPUT-STREAM-P
to return true.

## FUNDAMENTAL-OUTPUT-STREAM
[Class]

A subclass of FUNDAMENTAL-STREAM.  Its inclusion causes
OUTPUT-STREAM-P to return true.  Bi-direction streams may be formed by
including both FUNDAMENTAL-OUTPUT-STREAM and FUNDAMENTAL-INPUT-STREAM.

## FUNDAMENTAL-CHARACTER-STREAM
[Class]

A subclass of FUNDAMENTAL-STREAM.  It provides a method for
STREAM-ELEMENT-TYPE which returns CHARACTER.

## FUNDAMENTAL-BINARY-STREAM
[Class]
    
A subclass of FUNDAMENTAL-STREAM.  Any instantiable class that
includes this needs to define a method for STREAM-ELEMENT-TYPE.

## FUNDAMENTAL-CHARACTER-INPUT-STREAM
[Class]

Includes FUNDAMENTAL-INPUT-STREAM and FUNDAMENTAL-CHARACTER-STREAM.
It provides default methods for several generic functions used for
character input.

## FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
[Class]

Includes FUNDAMENTAL-OUTPUT-STREAM and FUNDAMENTAL-CHARACTER-STREAM.
It provides default methods for several generic functions used for
character output.

## FUNDAMENTAL-BINARY-INPUT-STREAM
[Class]

Includes FUNDAMENTAL-INPUT-STREAM and FUNDAMENTAL-BINARY-STREAM.

## FUNDAMENTAL-BINARY-OUTPUT-STREAM
[Class]

Includes FUNDAMENTAL-OUTPUT-STREAM and FUNDAMENTAL-BINARY-STREAM.


# Character Input

A character input stream can be created by defining a class that
includes FUNDAMENTAL-CHARACTER-INPUT-STREAM and defining methods for
the generic functions below.

## STREAM-READ-CHAR
[Generic Function]

```common-lisp
(stream-read-char stream) ; → (or character (eql :eof))
```

This reads one character from the stream.  It returns either a
character object, or the symbol :EOF if the stream is at end-of-file.
Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM must define a
method for this function.

Note that for all of these generic functions, the stream argument must
be a stream object, not T or NIL.

## STREAM-UNREAD-CHAR
[Generic Function]

```common-lisp
(stream-unread-char stream character) ; → null
```

Un-does the last call to STREAM-READ-CHAR, as in UNREAD-CHAR.  Returns
NIL.  Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM must define
a method for this function.

## STREAM-READ-CHAR-NO-HANG
[Generic Function]

```common-lisp
(stream-read-char-no-hang stream) ; → (or character nil (eql :eof))
```

This is used to implement READ-CHAR-NO-HANG.  It returns either a
character, or NIL if no input is currently available, or :EOF if
end-of-file is reached.  The default method provided by
FUNDAMENTAL-CHARACTER-INPUT-STREAM simply calls STREAM-READ-CHAR; this
is sufficient for file streams, but interactive streams should define
their own method.
  
## STREAM-PEEK-CHAR
[Generic Function]

```common-lisp
(stream-peek-char stream) ; → (or character (eql :eof))
```

Used to implement PEEK-CHAR; this corresponds to peek-type of NIL.  It
returns either a character or :EOF.  The default method calls
STREAM-READ-CHAR and STREAM-UNREAD-CHAR.

## STREAM-LISTEN
[Generic Function]

```common-lisp
(stream-listen stream) ; → boolean
```

Used by LISTEN.  Returns true or false.  The default method uses
STREAM-READ-CHAR-NO-HANG and STREAM-UNREAD-CHAR.  Most streams should
define their own method since it will usually be trivial and will
always be more efficient than the default method.

## STREAM-READ-LINE
[Generic Function]

```common-lisp
(stream-read-line stream) ; → string, boolean
```

Used by READ-LINE.  A string is returned as the first value.  The
second value is true if the string was terminated by end-of-file
instead of the end of a line.  The default method uses repeated calls
to STREAM-READ-CHAR.

## STREAM-CLEAR-INPUT
[Generic Function]

```common-lisp
(stream-clear-input stream) ; → null 
```

Implements CLEAR-INPUT for the stream, returning NIL.  The default
method does nothing.

# Character Output

A character output stream can be created by defining a class that
includes FUNDAMENTAL-CHARACTER-OUTPUT-STREAM and defining methods for
the generic functions below.

## STREAM-WRITE-CHAR
[Generic Function]

```common-lisp
(stream-write-char stream character) ; → character
```

Writes character to the stream and returns the character.  Every
subclass of FUNDAMENTAL-CHARACTER-OUTPUT-STREAM must have a method
defined for this function.

## STREAM-LINE-COLUMN
[Generic Function]

```common-lisp
(stream-line-column stream) ; → (or real null)
```

This function returns the column number where the next character will
be written, or NIL if that is not meaningful for this stream.  The
first column on a line is numbered 0.  This function is used in the
implementation of PPRINT and the FORMAT ~T directive.  For every
character output stream class that is defined, a method must be
defined for this function, although it is permissible for it to always
return NIL.

## STREAM-START-LINE-P
[Generic Function]

```common-lisp
(stream-start-line-p stream) ; → boolean
```

This is a predicate which returns T if the stream is positioned at the
beginning of a line, else NIL.  It is permissible to always return
NIL.  This is used in the implementation of FRESH-LINE.  Note that
while a value of 0 from STREAM-LINE-COLUMN also indicates the
beginning of a line, there are cases where STREAM-START-LINE-P can be
meaningfully implemented although STREAM-LINE-COLUMN can't be.  For
example, for a window using variable-width characters, the column
number isn't very meaningful, but the beginning of the line does have
a clear meaning.  The default method for STREAM-START-LINE-P on class
FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses STREAM-LINE-COLUMN, so if
that is defined to return NIL, then a method should be provided for
either STREAM-START-LINE-P or STREAM-FRESH-LINE.

## STREAM-WRITE-STRING
[Generic Function]

```common-lisp
(stream-write-string stream string &optional start end) ; → string
```

This is used by WRITE-STRING.  It writes the string to the stream,
optionally delimited by start and end, which default to 0 and NIL.
The string argument is returned.  The default method provided by
FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses repeated calls to
STREAM-WRITE-CHAR.

## STREAM-TERPRI
[Generic Function]

```common-lisp
(stream-terpri stream) ; → null
```

Writes an end of line, as for TERPRI.  Returns NIL.  The default
method does (STREAM-WRITE-CHAR stream #\NEWLINE).

## STREAM-FRESH-LINE
[Generic Function]

```common-lisp
(stream-fresh-line stream) ; → null
```

Used by FRESH-LINE.  The default method uses STREAM-START-LINE-P and
STREAM-TERPRI.

## STREAM-FINISH-OUTPUT
[Generic Function]

```common-lisp
(stream-finish-output stream) ; → null
```

Implements FINISH-OUTPUT.  The default method does nothing.

## STREAM-FORCE-OUTPUT
[Generic Function]

```common-lisp
(stream-force-output stream) ; → null
```

Implements FORCE-OUTPUT.  The default method does nothing.

## STREAM-CLEAR-OUTPUT
[Generic Function]

```common-lisp
(stream-clear-output stream) ; → null
```

Implements CLEAR-OUTPUT.  The default method does nothing.

## STREAM-ADVANCE-TO-COLUMN
[Generic Function]

```common-lisp
(stream-advance-to-column stream column) ; → boolean
```

Writes enough blank space so that the next character will be written
at the specified column.  Returns true if the operation is
successful, or NIL if it is not supported for this stream.    
This is intended for use by by PPRINT and FORMAT ~T.  The default
method uses STREAM-LINE-COLUMN and repeated calls to
STREAM-WRITE-CHAR with a #\SPACE character; it returns NIL if
STREAM-LINE-COLUMN returns NIL.

# Other Functions

## CLOSE
[Generic Function]

```common-lisp
(close stream &key abort) ; → t
```

The existing function CLOSE is redefined to be a generic function, but
otherwise behaves the same.  The default method provided by class
FUNDAMENTAL-STREAM sets a flag for OPEN-STREAM-P.  The value returned
by CLOSE will be as specified by the issue CLOSED-STREAM-OPERATIONS.

## OPEN-STREAM-P
[Generic Function]

```common-lisp
(open-stream-p stream) ; → boolean
```

This function
[from proposal STREAM-ACCESS] is made generic.  A
default method is provided by class FUNDAMENTAL-STREAM which returns
true if CLOSE has not been called on the stream.

## STREAMP
[Generic Function]

```common-lisp
(streamp stream) ; → boolean
```

## INPUT-STREAM-P
[Generic Function]

```common-lisp
(input-stream-p stream) ; → boolean
```

## OUTPUT-STREAM-P
[Generic Function]

```common-lisp
(output-stream-p stream) ; → boolean
```

These three existing predicates may optionally be implemented as
generic functions for implementations that want to permit users to
define streams that are not STANDARD-OBJECTs.  Normally, the default
methods provided by classes FUNDAMENTAL-INPUT-STREAM and
FUNDAMENTAL-OUTPUT-STREAM are sufficient.  Note that, for example,
(INPUT-STREAM-P x) is not equivalent to (TYPEP x
'FUNDAMENTAL-INPUT-STREAM) because implementations may have additional
ways of defining their own streams even if they don't make that
visible by making these predicates generic.

## STREAM-ELEMENT-TYPE
[Generic Function]

```common-lisp
(stream-element-type stream) ; → typespec
```

This existing function is made generic, but otherwise behaves the
same.  Class FUNDAMENTAL-CHARACTER-STREAM provides a default method
which returns CHARACTER.

PATHNAME and TRUENAME are also permitted to be implemented as generic
functions.  There is no default method since these are not valid for
all streams.

# Binary Streams

Binary streams can be created by defining a class that includes either
FUNDAMENTAL-BINARY-INPUT-STREAM or FUNDAMENTAL-BINARY-OUTPUT-STREAM
(or both) and defining a method for STREAM-ELEMENT-TYPE and for one or
both of the following generic functions.

## STREAM-READ-BYTE
[Generic Function]

```common-lisp
(stream-read-byte stream) ; → (or integer (eql :eof))
```

Used by READ-BYTE; returns either an integer, or the symbol :EOF if
the stream is at end-of-file.

## STREAM-WRITE-BYTE
[Generic Function]

```common-lisp
(stream-write-byte stream integer) ; → integer
```

Implements WRITE-BYTE; writes the integer to the stream and returns
the integer as the result.

# Sequence Extensions

Generic functions that provide the implementation for
[read-sequence](https://novaspec.org/cl/f_red-sequence) and
[write-sequence](https://novaspec.org/cl/f_write-sequence). Indicated
by the feature `:gray-streams-sequence`. This extension is not
consistently defined by the implementations that expose it. Some
implementations have the start and end arguments as required, some
have them as optional, and some have them as keyword arguments. Given
that that
[STREAM-WRITE-STRING](#STREAM-WRITE-STRING) has start and
end as optional arguments this is probably the choice that is more
consistent with the Gray stream protocol.

## STREAM-READ-SEQUENCE
[Generic Function]

```common-lisp
;; Variant with optional start and end. Indicated by presence of
;; feature :gray-streams-sequence
(stream-read-sequence stream sequence &optional start end) ; → integer

;; Variant with all required arguments. Indicated by presence of
;; feature :gray-streams-sequence-required
(stream-read-sequence stream sequence start end) ; → integer

;; Variant with all required arguments. Indicated by presence of
;; feature :gray-streams-sequence-key
(stream-read-sequence stream sequence &key start end) ; → integer

;; Variant with named stream-read-vector with all required
;; arguments. Indicated by feature :gray-streams-read-vector-required
(stream-read-vector stream sequence start end) ; → integer
```

## STREAM-WRITE-SEQUENCE
[Generic Function]

```common-lisp
;; Variant with optional start and end. Indicated by presence of
;; feature :gray-streams-sequence
(stream-write-sequence stream sequence &optional start end) ; → integer

;; Variant with all required arguments. Indicated by presence of
;; feature :gray-streams-sequence-required
(stream-write-sequence stream sequence start end) ; → integer

;; Variant with all required arguments. Indicated by presence of
;; feature :gray-streams-sequence-key
(stream-write-sequence stream sequence &key start end) ; → integer

;; Variant with named stream-write-vector with all required
;; arguments. Indicated by feature :gray-streams-write-vector-required
(stream-write-vector stream sequence start end) ; → integer
```

# File Position Extensions

## STREAM-FILE-POSITION
[Generic Function]

```common-lisp
; abcl allegro ccl clasp ecl mezzano mkcl sbcl sicl
(stream-file-position stream &optional position-spec)

(stream-position stream position-spec)

(stream-position stream &optional position-spec)

; cmucl
(stream-position stream)
((setf stream-position) position stream)
```

; clasp mezzano sicl
stream-file-length

; ccl
stream-length

; ccl clasp cmucl sbcl sicl
(stream-line-length stream)

; ecl clasp
(stream-interactive-p stream)
; ccl sbcl sicl
(interactive-stream-p stream)

; clasp
(defgeneric stream-file-descriptor (stream &optional direction)

; sicl
(defgeneric stream-read-byte-no-hang (stream))

; sicl
(defgeneric stream-listen-byte (stream))

; sicl
(defgeneric stream-peek-char-skip-whitespace (stream))

; ccl
stream-direction

; ccl
stream-domain

; ccl
stream-maybe-force-output

; ccl
stream-external-format
(setf stream-external-format)

; ccl
stream-filename
