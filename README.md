# nontrivial-gray-streams

nontrivial-gray-streams is a compability system for [Gray streams][]
which is an extension to Common Lisp that makes it possible to
implement Common Lisp streams using generic functions. The original
proposal was not accepted into the CL specification, but all modern CL
implmentations include support for this protocol because of its
usefulness.

nontrivial-gray-streams performs a simalar function as
[trivial-gray-streams][] albeit with some different
philosophies. 

Firstly, nontrivial-gray-streams exposes and documents the most
common extensions to the Gray stream protocol. In exposing each
extension it adds feature words for conditional compilation that
indicate the level and type of support for that extension.

Secondly, unlike [trivial-gray-streams][], it does not introduce its
own subclasses of the fundamental stream classes. Instead it exports
the CL implementation's fundamental stream classes
directly. [trivial-gray-streams][] subclasses these classes so that it
can define its own version of the [Sequence Extensions][Sequence] and
the [File Position Extensions][File Position]. There is some variation
in the signatures of the generic functions of those extensions in the
various CL implmentations, which [trivial-gray-streams][] tries to
work around via this mechanism. nontrivial-gray-streams exports these
extensions exactly as they appear and adds feature keywords as needed
to distinguish the differences.

Lastly, nontrivial-gray-streams includes a test suite that attempts to
address not just the core functionality of the Gray stream protocol,
but also the details of the various extensions.

## Systems and Packages

The core functionality of nontrivial-gray-streams is in the
nontrivial-gray-streams ASDF system, which contains the
nontrivial-gray-streams package. This package also has the nickname
"ngray" for brevity.

The exported classes are functions are documented below along with
some notes regarding implementation or interface issues.

## Classes

The following classes are to be used as super classes of user-defined
stream classes.  They are not intended to be directly instantiated;
they just provide places to hang default methods.

### FUNDAMENTAL-STREAM
*Class*

This class is a subclass of STREAM and of STANDARD-OBJECT.  [STREAMP][]
will return true for an instance of any class that includes this.  (It
may return true for some other things also.)

### FUNDAMENTAL-INPUT-STREAM
*Class*

A subclass of [FUNDAMENTAL-STREAM][].  Its inclusion causes
INPUT-STREAM-P to return true.

### FUNDAMENTAL-OUTPUT-STREAM
*Class*

A subclass of [FUNDAMENTAL-STREAM][].  Its inclusion causes
[OUTPUT-STREAM-P][] to return true.  Bi-direction streams may be
formed by including both [FUNDAMENTAL-OUTPUT-STREAM][] and
[FUNDAMENTAL-INPUT-STREAM][].

### FUNDAMENTAL-CHARACTER-STREAM
*Class*

A subclass of [FUNDAMENTAL-STREAM][].  It provides a method for
[STREAM-ELEMENT-TYPE][] which returns CHARACTER.

### FUNDAMENTAL-BINARY-STREAM
*Class*
    
A subclass of [FUNDAMENTAL-STREAM][].  Any instantiable class that
includes this needs to define a method for [STREAM-ELEMENT-TYPE][].

### FUNDAMENTAL-CHARACTER-INPUT-STREAM
*Class*

Includes [FUNDAMENTAL-INPUT-STREAM][] and
[FUNDAMENTAL-CHARACTER-STREAM][].  It provides default methods for
several generic functions used for character input.

### FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
*Class*

Includes [FUNDAMENTAL-OUTPUT-STREAM][] and
[FUNDAMENTAL-CHARACTER-STREAM][].  It provides default methods for
several generic functions used for character output.

### FUNDAMENTAL-BINARY-INPUT-STREAM
*Class*

Includes [FUNDAMENTAL-INPUT-STREAM][] and
[FUNDAMENTAL-BINARY-STREAM][].

### FUNDAMENTAL-BINARY-OUTPUT-STREAM
*Class*

Includes [FUNDAMENTAL-OUTPUT-STREAM][] and
[FUNDAMENTAL-BINARY-STREAM][].


## Character Input

A character input stream can be created by defining a class that
includes [FUNDAMENTAL-CHARACTER-INPUT-STREAM][] and defining methods
for the generic functions below.

### STREAM-READ-CHAR
*Generic Function*

```common-lisp
(stream-read-char stream) ; → (or character (eql :eof))
```

This reads one character from the stream.  It returns either a
character object, or the symbol :EOF if the stream is at end-of-file.
Every subclass of [FUNDAMENTAL-CHARACTER-INPUT-STREAM][] must define a
method for this function.

Note that for all of these generic functions, the stream argument must
be a stream object, not T or NIL.

### STREAM-UNREAD-CHAR
*Generic Function*

```common-lisp
(stream-unread-char stream character) ; → null
```

Un-does the last call to [STREAM-READ-CHAR][], as in
[CL:UNREAD-CHAR][].  Returns NIL.  Every subclass of
[FUNDAMENTAL-CHARACTER-INPUT-STREAM][] must define a method for this
function.

### STREAM-READ-CHAR-NO-HANG
*Generic Function*

```common-lisp
(stream-read-char-no-hang stream) ; → (or character nil (eql :eof))
```

This is used to implement [CL:READ-CHAR-NO-HANG][].  It returns either
a character, or NIL if no input is currently available, or :EOF if
end-of-file is reached.  The default method provided by
[FUNDAMENTAL-CHARACTER-INPUT-STREAM][] simply calls
[STREAM-READ-CHAR][]; this is sufficient for file streams, but
interactive streams should define their own method.
  
### STREAM-PEEK-CHAR
*Generic Function*

```common-lisp
(stream-peek-char stream) ; → (or character (eql :eof))
```

Used to implement [CL:PEEK-CHAR][]; this corresponds to peek-type of
NIL.  It returns either a character or :EOF.  The default method calls
[STREAM-READ-CHAR][] and [STREAM-UNREAD-CHAR][].

### STREAM-LISTEN
*Generic Function*

```common-lisp
(stream-listen stream) ; → boolean
```

Used by [CL:LISTEN][].  Returns true or false.  The default method
uses [STREAM-READ-CHAR-NO-HANG][] and [STREAM-UNREAD-CHAR][].  Most
streams should define their own method since it will usually be
trivial and will always be more efficient than the default method.

> The default implementation described by the Gray stream protocol is
> flawed since binary streams do not support reading or unreading
> characters. This implementation is probably only appropriate for
> [FUNDAMENTAL-CHARACTER-INPUT-STREAM][]. This means that
> [FUNDAMENTAL-BINARY-INPUT-STREAM][] subclasses would need to
> specialize this generic function at a minimum. A default
> implementation that followed the pattern described by the Gray
> stream protocol would require the addition of
> STREAM-READ-BYTE-NO-HANG and STREAM-UNREAD-BYTE generic
> functions. These functions do not have a parallel in the ANSI
> specification. Also, the description of [CL:LISTEN][] seems to
> assume that the stream is an interactive character input stream
> versus other types of streams that listening would be used on,
> i.e. networked binary streams.

### STREAM-READ-LINE
*Generic Function*

```common-lisp
(stream-read-line stream) ; → string, boolean
```

Used by [CL:READ-LINE][].  A string is returned as the first value.  The
second value is true if the string was terminated by end-of-file
instead of the end of a line.  The default method uses repeated calls
to [STREAM-READ-CHAR][].

> The proposal does not explicitly state what should be returned if
> end-of-file is encountered when no characters, newline or otherwise,
> have been read. Presumably it is `(values "" t)` since the example
> method provided in the proposal returns these values in this
> situation.

### STREAM-CLEAR-INPUT
*Generic Function*

```common-lisp
(stream-clear-input stream) ; → null 
```

Implements [CL:CLEAR-INPUT][] for the stream, returning NIL.  The
default method does nothing.

## Character Output

A character output stream can be created by defining a class that
includes [FUNDAMENTAL-CHARACTER-OUTPUT-STREAM][] and defining methods
for the generic functions below.

### STREAM-WRITE-CHAR
*Generic Function*

```common-lisp
(stream-write-char stream character) ; → character
```

Writes character to the stream and returns the character.  Every
subclass of [FUNDAMENTAL-CHARACTER-OUTPUT-STREAM][] must have a method
defined for this function.

### STREAM-LINE-COLUMN
*Generic Function*

```common-lisp
(stream-line-column stream) ; → (or real null)
```

This function returns the column number where the next character will
be written, or NIL if that is not meaningful for this stream.  The
first column on a line is numbered 0.  This function is used in the
implementation of [CL:PPRINT][] and the [CL:FORMAT ~T][] directive.  For
every character output stream class that is defined, a method must be
defined for this function, although it is permissible for it to always
return NIL.

> The orignal proposal does not specify the numerical type of columns
> in this function or in [STREAM-ADVANCE-TO-COLUMN][]. The ANSI Common
> Lisp specification exclicitly allows real numbers for the purpose of
> typesetting variable-width characters. Many implementations seem to
> assume that the return from STREAM-LINE-COLUMN or the column number
> passed to [STREAM-ADVANCE-TO-COLUMN][] is an integer, but this does
> not comply with the specification.

> This generic function does not have a very good name. There is no
> allowance in the Gray stream proposal for the tracking of column or
> line number for input streams even though this is implemented by all
> Common Lisp implementations for the purpose of compile source
> information. If there was an additional generic function
> STREAM-LINE-NUMBER one would do this by using the existing
> STREAM-LINE-COLUMN for input streams. Although that would work for
> unidirectional streams it is not clear how it would apply to
> bidirectional streams. Specifically, in the case of TWO-WAY-STREAM,
> for which stream should STREAM-LINE-COLUMN be forwarded to? In order
> work seamlessly for bidirectional streams, perhapsi t would be
> better if this generic function had been named
> STREAM-OUTPUT-COLUMN. Then the corresponding STREAM-OUTPUT-LINE,
> STREAM-INPUT-COLUMN, and STREAM-INPUT-LINE could be added and fit
> the naming pattern naturally.

### STREAM-START-LINE-P
*Generic Function*

```common-lisp
(stream-start-line-p stream) ; → boolean
```

This is a predicate which returns T if the stream is positioned at the
beginning of a line, else NIL.  It is permissible to always return
NIL.  This is used in the implementation of [CL:FRESH-LINE][].  Note
that while a value of 0 from [STREAM-LINE-COLUMN][] also indicates the
beginning of a line, there are cases where [STREAM-START-LINE-P][] can
be meaningfully implemented although [STREAM-LINE-COLUMN][] can't be.
For example, for a window using variable-width characters, the column
number isn't very meaningful, but the beginning of the line does have
a clear meaning.  The default method for [STREAM-START-LINE-P][] on
class [FUNDAMENTAL-CHARACTER-OUTPUT-STREAM][] uses
[STREAM-LINE-COLUMN][], so if that is defined to return NIL, then a
method should be provided for either [STREAM-START-LINE-P][] or
[STREAM-FRESH-LINE][].

> This generic function is superfluous. The statement that "for a
> window using variable-width characters, the column number isn't very
> meaningful" is just not true. Real valued column numbers are
> explicitly permitted in the specification of the pretty printer and
> are very useful in the typesetting of variable-width characters,
> especially when STREAM-ADVANCE-TO-COLUMN can move to real valued
> columns using a mechanism other then the insertion of spaces. Most
> implementations of the pretty printer assemble the output text into
> a string buffer and interpret the index of the characters to be
> related to the column number. In other words they do not allow for
> the possibility of real valued columns that are not non-negative
> integers. But it is possible to create an implementation of the
> pretty printer that can typeset using variable-width characters,
> i.e. [Inravina][]. Furthermore, [STREAM-LINE-COLUMN][] has already
> specified that the first column at the start of a line is to be
> numbered 0 which implies that STREAM-START-LINE-P will always just
> be equivalent to `(eql column 0)`.

### STREAM-WRITE-STRING
*Generic Function*

```common-lisp
(stream-write-string stream string &optional start end) ; → string
```

This is used by [CL:WRITE-STRING][].  It writes the string to the
stream, optionally delimited by start and end, which default to 0 and
NIL.  The string argument is returned.  The default method provided by
[FUNDAMENTAL-CHARACTER-OUTPUT-STREAM][] uses repeated calls to
[STREAM-WRITE-CHAR][].

### STREAM-TERPRI
*Generic Function*

```common-lisp
(stream-terpri stream) ; → null
```

Writes an end of line, as for [CL:TERPRI][].  Returns NIL.  The
default method does `(STREAM-WRITE-CHAR stream #\NEWLINE)`.

> The default method described in the proposal should probably be only
> for fundamental-character-output-stream since non-bivalent binary
> streams do not have a concept of columns. In this case any signaled
> error would be appropriate including NO-APPLICABLE-METHOD.

### STREAM-FRESH-LINE
*Generic Function*

```common-lisp
(stream-fresh-line stream) ; → null
```

Used by [CL:FRESH-LINE][].  The default method uses
[STREAM-START-LINE-P][] and [STREAM-TERPRI][].

> The default method described in the proposal should probably be only
> for fundamental-character-output-stream since non-bivalent binary
> streams do not have a concept of columns. In this case any signaled
> error would be appropriate including NO-APPLICABLE-METHOD.
>
> Additionally. the description of the default method and the example
> method given in the proposal do not follow the specification as per
> [CL:FRESH-LINE][]. There it states that "If for some reason this
> cannot be determined, then a newline is output anyway." Does this
> mean that a newline is output if 
> `(or (stream-start-line-p stream) (null (stream-line-column stream)))` 
> is non-NIL?

### STREAM-FINISH-OUTPUT
*Generic Function*

```common-lisp
(stream-finish-output stream) ; → null
```

Implements [CL:FINISH-OUTPUT][].  The default method does nothing.

### STREAM-FORCE-OUTPUT
*Generic Function*

```common-lisp
(stream-force-output stream) ; → null
```

Implements [CL:FORCE-OUTPUT][].  The default method does nothing.

### STREAM-CLEAR-OUTPUT
*Generic Function*

```common-lisp
(stream-clear-output stream) ; → null
```

Implements [CL:CLEAR-OUTPUT][].  The default method does nothing.

### STREAM-ADVANCE-TO-COLUMN
*Generic Function*

```common-lisp
(stream-advance-to-column stream column) ; → boolean
```

Writes enough blank space so that the next character will be written
at the specified column.  Returns true if the operation is
successful, or NIL if it is not supported for this stream.    
This is intended for use by by PPRINT and FORMAT ~T.  The default
method uses [STREAM-LINE-COLUMN][] and repeated calls to
[STREAM-WRITE-CHAR][] with a #\SPACE character; it returns NIL if
[STREAM-LINE-COLUMN][] returns NIL.

> The default method described in the proposal should probably be only
> for fundamental-character-output-stream since non-bivalent binary
> streams do not have a concept of columns. In this case any signaled
> error would be appropriate including NO-APPLICABLE-METHOD.

## Other Functions

### CLOSE
*Generic Function*

```common-lisp
(close stream &key abort) ; → t
```

The existing function CLOSE is redefined to be a generic function, but
otherwise behaves the same.  The default method provided by class
[FUNDAMENTAL-STREAM][] sets a flag for [OPEN-STREAM-P][].  The value
returned by CLOSE will be as specified by the issue
CLOSED-STREAM-OPERATIONS.

### OPEN-STREAM-P
*Generic Function*

```common-lisp
(open-stream-p stream) ; → boolean
```

This function [from proposal STREAM-ACCESS] is made generic.  A
default method is provided by class [FUNDAMENTAL-STREAM][] which
returns true if CLOSE has not been called on the stream.

### STREAM-ELEMENT-TYPE
*Generic Function*

```common-lisp
(stream-element-type stream) ; → typespec
```

This existing function is made generic, but otherwise behaves the
same.  Class [FUNDAMENTAL-CHARACTER-STREAM][] provides a default
method which returns CHARACTER.

### PATHNAME

[CL:PATHNAME][] is also permitted to be implemented as generic
functions.  There is no default method since it is not valid for all
streams. If [CL:PATHNAME][] is made generic then the feature
`:gray-streams-pathname` will be present.

### TRUENAME

[CL:TRUENAME][] is also permitted to be implemented as generic
functions.  There is no default method since it is not valid for all
streams. If [CL:TRUENAME][] is made generic then the feature
`:gray-streams-truename` will be present.


## Optional Predicates

These three existing predicates may optionally be implemented as
generic functions for implementations that want to permit users to
define streams that are not STANDARD-OBJECTs.  Normally, the default
methods provided by classes [FUNDAMENTAL-INPUT-STREAM][] and
[FUNDAMENTAL-OUTPUT-STREAM][] are sufficient.  Note that, for example,
`(INPUT-STREAM-P x)` is not equivalent to 
`(TYPEP x 'FUNDAMENTAL-INPUT-STREAM)` because implementations may have
additional ways of defining their own streams even if they don't make
that visible by making these predicates generic.

> If the implementation does not support a generic STREAMP then
> classes that implement the Gray stream protocol must subclass
> [STREAM][]. In practice this means they will probably actually need
> to subclass [FUNDAMENTAL-STREAM][] since [STREAM][] may be a
> [BUILT-IN-CLASS][]. Even in the cases that [STREAM][] is not a
> [BUILT-IN-CLASS][] it may not be [STANDARD-CLASS][] since streams
> are required very early in the bootstrapping of some Common Lisp
> implementations.
>
> The absence of support for generic versions of INPUT-STREAM-P and
> OUTPUT-STREAM-P implies some of the same issues for subclassing of
> [FUNDAMENTAL-INPUT-STREAM][] and [FUNDAMENTAL-OUTPUT-STREAM][] with
> the additional note that no implementation provides a generic
> STREAMP but does not provide generic versions of INPUT-STREAM-P and
> OUTPUT-STREAM-P. This means that either an implementation supports
> all three generic predicates, it supports INPUT-STREAM-P and
> OUTPUT-STREAM-P but not STREAMP, or it does not support any of these
> three generic predicates. Therefore, on implementations that do not
> support these three generic predicates subclassing
> [FUNDAMENTAL-INPUT-STREAM][] or [FUNDAMENTAL-OUTPUT-STREAM][] is
> required.

### STREAMP
*Generic Function*

```common-lisp
(streamp stream) ; → boolean
```

Indicated by the presence of feature `:gray-streams-streamp`.

### INPUT-STREAM-P
*Generic Function*

```common-lisp
(input-stream-p stream) ; → boolean
```

Indicated by the presence of feature `:gray-streams-directionp`.

### OUTPUT-STREAM-P
*Generic Function*

```common-lisp
(output-stream-p stream) ; → boolean
```

Indicated by the presence of feature `:gray-streams-directionp`.

## Binary Streams

Binary streams can be created by defining a class that includes either
[FUNDAMENTAL-BINARY-INPUT-STREAM][] or
[FUNDAMENTAL-BINARY-OUTPUT-STREAM][] (or both) and defining a method
for [STREAM-ELEMENT-TYPE][] and for one or both of the following
generic functions.

### STREAM-READ-BYTE
*Generic Function*

```common-lisp
(stream-read-byte stream) ; → (or integer (eql :eof))
```

Used by [CL:READ-BYTE][]; returns either an integer, or the symbol
:EOF if the stream is at end-of-file.

### STREAM-WRITE-BYTE
*Generic Function*

```common-lisp
(stream-write-byte stream integer) ; → integer
```

Implements [CL:WRITE-BYTE][]; writes the integer to the stream and
returns the integer as the result.

## Implementation Support of Extensions and Optional Interfaces

| Interface/Extension          | [ABCL][] | [Allegro][] | [CCL][] | [Clasp][] | [CLISP][] | [CMUCL][] | [ECL][] | [LispWorks][] | [Mezzano][] | [MKCL][] | [SBCL][] |
|------------------------------|----------|-------------|---------|-----------|-----------|-----------|---------|---------------|-------------|----------|----------|
| [STREAMP][]                  | ✓        |             | ✓¹      | ✓         |           |           | ✓       | ✓             | ✓           | ✓        |          |
| [INPUT-STREAM-P][]           | ✓        | ✓           | ✓       | ✓         |           | ✓         | ✓       | ✓             | ✓           | ✓        | ✓        |
| [OUTPUT-STREAM-P][]          | ✓        | ✓           | ✓       | ✓         |           | ✓         | ✓       | ✓             | ✓           | ✓        | ✓        |
| [PATHNAME][]                 | ✓        |             | ✓       | ✓         |           | ✓         | ✓       |               | ✓           |          |          |
| [TRUENAME][]                 | ✓        |             |         | ✓         |           | ✓         | ✓       |               | ✓           |          |          |
| [SETF STREAM-ELEMENT-TYPE][] | ✓        |             |         | ✓         | ✓         | ✓         |         |               |             |          |          |
| [Sequence][]                 | ✓        | ✓           | ✓       | ✓         | ✓         | ✓         | ✓       | ✓             | ✓           | ✓        | ✓        |
| [File Position][]            | ✓        | ✓           | ✓       | ✓         | ✓         | ✓         | ✓       | ✓             | ✓           | ✓        | ✓        |
| [File Length][]              | ✓        |             | ✓¹      | ✓         |           | ✓         | ✓       |               | ✓           |          |          |
| [File String Length][]       |          |             |         | ✓         |           |           | ✓       |               | ✓           |          |          |
| [External Format][]          |          |             | ✓       | ✓         |           |           |         |               | ✓           |          |          |
| [INTERACTIVE-STREAM-P][]     | ✓        |             | ✓       | ✓         |           | ✓         | ✓       |               | ✓           | ✓        | ✓        |
| [Line Length][]              | ✓        | ✓           | ✓       | ✓         |           | ✓         | ✓       | ✓             | ✓           |          | ✓        |

1. The generic versions of STREAMP and FILE-LENGTH are in conflict
   with each other in CCL. The ANSI specification requires FILE-LENGTH
   to signal an TYPE-ERROR when the stream is not a FILE-STREAM. This
   requires subclassing FILE-STREAM in any class which wishes to
   implement FILE-LENGTH, but subclassing FILE-STREAM would make
   specializing STREAMP extraneous. This is probably only resolvable
   by the addition of a generic FILE-STREAM-P, which has no
   corresponding function in the ANSI specification.

## Bivalent Extensions

### SETF STREAM-ELEMENT-TYPE

The Gray stream protocol makes [STREAM-ELEMENT-TYPE][] a generic
function but does not provide for bivalent streams which can change
the element type at any time. In order support bivalent streams one
needs a SETF for [CL:STREAM-ELEMENT-TYPE][]. The existance of this
extension is indicated by the feature
`:gray-streams-element-type/setf`.

```common-lisp
((setf stream-element-type) new-value stream) ; → new-value
```

## Sequence Extensions

Generic functions that provide the implementation for
[CL:READ-SEQUENCE][] and [CL:WRITE-SEQUENCE][]. Indicated by the
presence of the feature `:gray-streams-sequence`. This extension is
not consistently defined by the implementations that expose it. Some
implementations have the start and end arguments as required, some
have them as optional, and some have them as keyword arguments. Given
that that [STREAM-WRITE-STRING][] has start and end as optional
arguments this is probably the choice that is more consistent with the
Gray stream protocol.

### STREAM-READ-SEQUENCE
*Generic Function*

### Variants

1. Variant with optional start and end arguments. Indicated by
   presence of feature `:gray-streams-sequence/optional`.

   ```common-lisp
   (stream-read-sequence stream sequence &optional start end) ; → integer
   ```

2. Variant with all required arguments. Indicated by presence of
   feature `:gray-streams-sequence/required`.
   
   ```common-lisp
   (stream-read-sequence stream sequence start end) ; → integer
   ```

3. Variant with keyword arguments and reversed sequence and stream
   arguments. Indicated by presence of feature
   `:gray-streams-sequence/key`.

   ```common-lisp
   (stream-read-sequence sequence stream &key start end) ; → integer
   ```

### STREAM-WRITE-SEQUENCE
*Generic Function*

### Variants

1. Variant with optional start and end arguments. Indicated by
   presence of feature `:gray-streams-sequence/optional`.

   ```common-lisp
   (stream-write-sequence stream sequence &optional start end) ; → integer
   ```

2. Variant with all required arguments. Indicated by presence of feature
   `:gray-streams-sequence/required`.
   
   ```common-lisp
   (stream-write-sequence stream sequence start end) ; → integer
   ```

3. Variant with keyword arguments and reversed sequence and stream
   arguments. Indicated by presence of feature
   `:gray-streams-sequence/key`.

   ```common-lisp
   (stream-write-sequence sequence stream &key start end) ; → integer
   ```

## File Position Extensions

### STREAM-FILE-POSITION
*Generic Function*

### Variants

1. Variant with optional position argument. Indicated by presence of
   feature `:gray-streams-file-position/optional`.

   ```common-lisp
   (stream-file-position stream &optional position) ; → (or integer boolean)
   ```

2. Variant with required position argument. Indicated by presence of
   feature `:gray-streams-file-position/required`.

   ```common-lisp
   (stream-file-position stream position) ; → (or integer boolean)
   ```

3. Variant without position argument. Indicated by presence of feature
   `:gray-streams-file-position/get`.

   ```common-lisp
   (stream-file-position stream) ; → (or integer null)
   ```

3. Variant with SETF function. Indicated by presence of feature
   `:gray-streams-file-position/setf`.

   ```common-lisp
   ((setf stream-file-position) position stream) ; → boolean
   ```

Generic functions that allow implementing [CL:FILE-POSITION][] for
Gray streams. Indicated by feature `:gray-streams-file-position`.

## File Length Extensions

### STREAM-FILE-LENGTH
*Generic Function*

### Variants

1. Variant with optional length argument. Indicated by presence of
   feature `:gray-streams-file-length/optional`.

   ```common-lisp
   (stream-file-length stream &optional length) ; → (or integer boolean)
   ```

2. Variant without length argument. Indicated by presence of feature
   `:gray-streams-file-length/get`.

   ```common-lisp
   (stream-file-length stream) ; → (or integer null)
   ```

Allows implementing [CL:FILE-LENGTH][] for Gray streams.  Indicated by
the presence of feature `:gray-streams-file-length`. The default
method signals a `type-error` with an expected type of `file-stream`
as required by the ANSI specification.

## File String Length Extensions

### STREAM-FILE-STRING-LENGTH
*Generic Function*

```common-lisp
(stream-file-string-length stream object) ; → (or integer null)
```

Allows implementing [CL:FILE-STRING-LENGTH][] for Gray streams.
Indicated by the presence of feature
`:gray-streams-file-string-length`. The default for
[FUNDAMENTAL-CHARACTER-OUTPUT-STREAM][] returns NIL.

## External Format Extensions

### STREAM-EXTERNAL-FORMAT

*Generic Function*

```common-lisp
(stream-external-format stream) ; → format
 ```
Generic functions that allow implementing [CL:STREAM-EXTERNAL-FORMAT][] for
Gray streams. Indicated by feature `:gray-streams-external-format`.

### SETF STREAM-EXTERNAL-FORMAT

```common-lisp
((setf stream-external-format) format stream) ; → format
 ```

Generic functions that allow SETF on [CL:STREAM-EXTERNAL-FORMAT][] for
Gray streams. Indicated by feature
`:gray-streams-external-format/setf`.

## Interactive Stream Extensions

### INTERACTIVE-STREAM-P
*Generic Function*

```common-lisp
(interactive-stream-p stream) ; → boolean
```

Allows implementing [CL:INTERACTIVE-STREAM-P][] for Gray
streams. Indicated by the presence of feature
`:gray-streams-interactive`.

## Line Length Extensions

### STREAM-LINE-LENGTH
*Generic Function*

```common-lisp
(stream-line-length stream) ; → (or real null)
```

Allows stream specific line length for Gray streams. Indicated by the
presence of feature `:gray-streams-line-length`. Used primarily for
the [CL:FORMAT ~<][] directive and the [pretty printer][].

> Some implementations do not permit returning `nil`. These
> implementations tend to assume that the return is the default line
> length which itself is implementation specific. One solution to this
> is to `(call-next-method)` when a `nil` return is desired.

[ABCL]: https://armedbear.common-lisp.dev/
[Allegro]: https://franz.com/products/allegro-common-lisp/
[BUILT-IN-CLASS]: https://novaspec.org/cl/t_built-in-class
[CCL]: https://github.com/Clozure/ccl
[CL:CLEAR-INPUT]: https://novaspec.org/cl/f_clear-input
[CL:CLEAR-OUTPUT]: https://novaspec.org/cl/f_finish-ouput
[CL:CLOSE]: #CLOSE
[CL:FILE-LENGTH]: https://novaspec.org/cl/f_file-length
[CL:FILE-POSITION]: https://novaspec.org/cl/f_file-position
[CL:FILE-STRING-LENGTH]: https://novaspec.org/cl/f_file-string-length
[CL:FINISH-OUTPUT]: https://novaspec.org/cl/f_finish-output
[CL:FORCE-OUTPUT]: https://novaspec.org/cl/f_finish-output
[CL:FORMAT ~<]: https://novaspec.org/cl/22_3_Formatted_Output#sec_22_3_6_2
[CL:FORMAT ~T]: https://novaspec.org/cl/22_3_Formatted_Output#sec_22_3_6_1
[CL:FRESH-LINE]: https://novaspec.org/cl/f_terpri
[CL:INTERACTIVE-STREAM-P]: https://novaspec.org/cl/f_interactive-stream-p
[CL:LISTEN]: https://novaspec.org/cl/f_listen
[CL:PATHNAME]: https://novaspec.org/cl/f_pathname
[CL:PEEK-CHAR]: https://novaspec.org/cl/f_peek-char
[CL:PPRINT]: https://novaspec.org/cl/f_write
[CL:READ-BYTE]: https://novaspec.org/cl/f_read-byte
[CL:READ-CHAR-NO-HANG]: https://novaspec.org/cl/f_read-char-no-hang
[CL:READ-LINE]: https://novaspec.org/cl/f_read-line
[CL:READ-SEQUENCE]: https://novaspec.org/cl/f_read-sequence
[CL:STREAM-ELEMENT-TYPE]: https://novaspec.org/cl/f_stream-element-type
[CL:STREAM-EXTERNAL-FORMAT]: https://novaspec.org/cl/f_stream-external-format
[CL:TERPRI]: https://novaspec.org/cl/f_terpri
[CL:TRUENAME]: https://novaspec.org/cl/f_truename
[CL:UNREAD-CHAR]: https://novaspec.org/cl/f_unread-char
[CL:WRITE-BYTE]: https://novaspec.org/cl/f_write-byte
[CL:WRITE-SEQUENCE]: https://novaspec.org/cl/f_write-sequence
[CL:WRITE-STRING]: https://novaspec.org/cl/f_write-string
[CLISP]: https://gitlab.com/gnu-clisp/clisp
[CMUCL]: https://gitlab.common-lisp.net/cmucl/cmucl
[Clasp]: https://clasp-developers.github.io/
[ECL]: https://ecl.common-lisp.dev/
[External Format]: #EXTERNAL-FORMAT-EXTENSIONS
[FUNDAMENTAL-BINARY-INPUT-STREAM]: #FUNDAMENTAL-BINARY-INPUT-STREAM
[FUNDAMENTAL-BINARY-OUTPUT-STREAM]: #FUNDAMENTAL-BINARY-OUTPUT-STREAM
[FUNDAMENTAL-BINARY-STREAM]: #FUNDAMENTAL-BINARY-STREAM
[FUNDAMENTAL-CHARACTER-INPUT-STREAM]: #FUNDAMENTAL-CHARACTER-INPUT-STREAM
[FUNDAMENTAL-CHARACTER-OUTPUT-STREAM]: #FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
[FUNDAMENTAL-CHARACTER-STREAM]: #FUNDAMENTAL-CHARACTER-STREAM
[FUNDAMENTAL-INPUT-STREAM]: #FUNDAMENTAL-INPUT-STREAM
[FUNDAMENTAL-OUTPUT-STREAM]: #FUNDAMENTAL-OUTPUT-STREAM
[FUNDAMENTAL-STREAM]: #FUNDAMENTAL-STREAM
[File Length]: #FILE-LENGTH-EXTENSIONS
[File Position]: #FILE-POSITION-EXTENSIONS
[File String Length]: #FILE-STRING-LENGTH-EXTENSIONS
[Gray streams]: PROPOSAL.txt
[INPUT-STREAM-P]: #INPUT-STREAM-P
[INTERACTIVE-STREAM-P]: #INTERACTIVE-STREAM-P
[Inravina]: https://github.com/yitzchak/Inravina
[Line Length]: #LINE-LENGTH-EXTENSIONS
[LispWorks]: https://www.lispworks.com/products/lispworks.html
[MKCL]: https://mkcl.common-lisp.dev/
[Mezzano]: https://github.com/froggey/Mezzano
[OPEN-STREAM-P]: #OPEN-STREAM-P
[OUTPUT-STREAM-P]: #OUTPUT-STREAM-P
[PATHNAME]: #PATHNAME
[SBCL]: http://sbcl.org/
[SETF STREAM-ELEMENT-TYPE]: #SETF-STREAM-ELEMENT-TYPE
[STANDARD-CLASS]: https://novaspec.org/cl/t_standard-class
[STREAM-ADVANCE-TO-COLUMN]: #STREAM-ADVANCE-TO-COLUMN
[STREAM-CLEAR-INPUT]: #STREAM-CLEAR-INPUT
[STREAM-CLEAR-OUTPUT]: #STREAM-CLEAR-OUTPUT
[STREAM-ELEMENT-TYPE]: #STREAM-ELEMENT-TYPE
[STREAM-FILE-LENGTH]: #STREAM-FILE-LENGTH
[STREAM-FILE-POSITION]: #STREAM-FILE-POSITION
[STREAM-FINISH-OUTPUT]: #STREAM-FINISH-OUTPUT
[STREAM-FORCE-OUTPUT]: #STREAM-FORCE-OUTPUT
[STREAM-FRESH-LINE]: #STREAM-FRESH-LINE
[STREAM-LINE-COLUMN]: #STREAM-LINE-COLUMN
[STREAM-LISTEN]: #STREAM-LISTEN
[STREAM-PEEK-CHAR]: #STREAM-PEEK-CHAR
[STREAM-READ-BYTE]: #STREAM-READ-BYTE
[STREAM-READ-CHAR-NO-HANG]: #STREAM-READ-CHAR-NO-HANG
[STREAM-READ-CHAR]: #STREAM-READ-CHAR
[STREAM-READ-LINE]: #STREAM-READ-LINE
[STREAM-READ-SEQUENCE]: #STREAM-READ-SEQUENCE
[STREAM-START-LINE-P]: #STREAM-START-LINE-P
[STREAM-TERPRI]: #STREAM-TERPRI
[STREAM-UNREAD-CHAR]: #STREAM-UNREAD-CHAR
[STREAM-WRITE-BYTE]: #STREAM-WRITE-BYTE
[STREAM-WRITE-CHAR]: #STREAM-WRITE-CHAR
[STREAM-WRITE-SEQUENCE]: #STREAM-WRITE-SEQUENCE
[STREAM-WRITE-STRING]: #STREAM-WRITE-STRING
[STREAMP]: #STREAMP
[STREAM]: https://novaspec.org/cl/t_stream
[Sequence]: #SEQUENCE-EXTENSIONS
[TRUENAME]: #TRUENAME
[pretty printer]: https://novaspec.org/cl/22_2_The_Lisp_Pretty_Printer#_j5
[trivial-gray-streams]: https://github.com/trivial-gray-streams/trivial-gray-streams
