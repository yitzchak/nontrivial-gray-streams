(in-package #:nontrivial-gray-streams/test)

;;; Basic Binary Input Stream

(defclass binary-input-stream-a
    (binary-input-mixin-a
     ngray:fundamental-binary-input-stream)
  ())

(define-stream-tests binary-input-a
  :class binary-input-stream-a
  :input t
  :binary t)

;;; Extended Binary Input Stream

(defclass binary-input-stream-b
    (binary-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-input-stream)
  ())

(define-stream-tests binary-input-b
  :class binary-input-stream-b
  :input t
  :binary t
  :extended t)

;;; Extended Binary Input Stream which relies on support for generic
;;; INPUT-STREAM-P

(defclass binary-input-stream-c
    (binary-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-stream)
  ())

#+gray-streams-input-stream-p
(define-stream-tests binary-input-c
  :class binary-input-stream-c
  :input t
  :binary t
  :extended t)

;;; Extended Binary Input Stream which relies on support for generic
;;; STREAMP

(defclass binary-input-stream-d
    (binary-input-mixin-b)
  ())

#+(and gray-streams-streamp (not ccl))
(define-stream-tests binary-input-d
  :class binary-input-stream-d
  :input t
  :binary t
  :extended t)

;;; Basic Binary Output Stream

(defclass binary-output-stream-a
    (binary-output-mixin-a
     ngray:fundamental-binary-output-stream)
  ())

(define-stream-tests binary-output-a
  :class binary-output-stream-a
  :output t
  :binary t)

;;; Extended Binary Output Stream

(defclass binary-output-stream-b
    (binary-output-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-output-stream)
  ())

(define-stream-tests binary-output-b
  :class binary-output-stream-b
  :output t
  :binary t
  :extended t)

;;; Extended Binary Output Stream which relies on support for generic
;;; OUTPUT-STREAM-P.

(defclass binary-output-stream-c
    (binary-output-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-stream)
  ())

#+gray-streams-output-stream-p
(define-stream-tests binary-output-c
  :class binary-output-stream-c
  :output t
  :binary t
  :extended t)

;;; Basic Binary Input/Output Stream

(defclass binary-io-stream-a
    (binary-io-mixin-a
     ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream)
  ())

(define-stream-tests binary-io-a
  :class binary-io-stream-a
  :input t
  :output t
  :binary t)

;;; Extended Binary Input/Output Stream

(defclass binary-io-stream-b
    (binary-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream)
  ())

(define-stream-tests binary-io-b
  :class binary-io-stream-b
  :input t
  :output t
  :binary t
  :extended t)

;;; Extended Binary Input/Output Stream which relies on support for generic
;;; INPUT-STREAM-P and OUTPUT-STREAM-P.

(defclass binary-io-stream-c
    (binary-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-binary-stream)
  ())

#+(and gray-streams-input-stream-p gray-streams-output-stream-p)
(define-stream-tests binary-io-c
  :class binary-io-stream-b
  :input t
  :output t
  :binary t
  :extended t)

;;; Basic Character Input Stream

(defclass character-input-stream-a
    (character-input-mixin-a
     ngray:fundamental-character-input-stream)
  ())

(define-stream-tests character-input-a
  :class character-input-stream-a
  :input t
  :character t)

;;; Extended Character Input Stream

(defclass character-input-stream-b
    (character-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream)
  ())

(define-stream-tests character-input-b
  :class character-input-stream-b
  :input t
  :character t
  :extended t)

;;; Extended Character Input Stream which relies on support for
;;; generic INPUT-STREAM-P.

(defclass character-input-stream-c
    (character-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-stream)
  ())

#+gray-streams-input-stream-p
(define-stream-tests character-input-c
  :class character-input-stream-c
  :input t
  :character t
  :extended t)

;;; Basic Character Output Stream

(defclass character-output-stream-a
    (character-output-mixin-a
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests character-output-a
  :class character-output-stream-a
  :output t
  :character t)

;;; Extended Character Output Stream

(defclass character-output-stream-b
    (character-output-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests character-output-b
  :class character-output-stream-b
  :output t
  :character t
  :extended t)

;;; Extended Character Output Stream which relies on support for
;;; generic OUTPUT-STREAM-P.

(defclass character-output-stream-c
    (character-output-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-stream)
  ())

#+gray-streams-output-stream-p
(define-stream-tests character-output-c
  :class character-output-stream-c
  :output t
  :character t
  :extended t)

;;; Basic Character Input/Output Stream

(defclass character-io-stream-a
    (character-io-mixin-a
     ngray:fundamental-character-input-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests character-io-a
  :class character-io-stream-a
  :input t
  :output t
  :character t)

;;; Extended Character Input/Output Stream

(defclass character-io-stream-b
    (character-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests character-io-b
  :class character-io-stream-b
  :input t
  :output t
  :character t
  :extended t)

;;; Extended Character Input/Output Stream which relies on support for
;;; generic INPUT-STREAM-P and OUTPUT-STREAM-P.

(defclass character-io-stream-c
    (character-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-stream)
  ())

#+(and gray-streams-input-stream-p gray-streams-output-stream-p)
(define-stream-tests character-io-c
  :class character-io-stream-c
  :input t
  :output t
  :character t
  :extended t)

;;; Basic Bivalent Input Stream

(defclass bivalent-input-stream-a
    (bivalent-input-mixin-a
     ngray:fundamental-character-input-stream)
  ())

(define-stream-tests bivalent-input-a
  :class bivalent-input-stream-a
  :input t
  :binary t
  :character t)

;;; Extended Bivalent Input Stream

(defclass bivalent-input-stream-b
    (bivalent-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream)
  ())

(define-stream-tests bivalent-input-b
  :class bivalent-input-stream-b
  :input t
  :binary t
  :character t
  :extended t)

;;; Extended Bivalent Input Stream which relies on support for
;;; generic INPUT-STREAM-P.

(defclass bivalent-input-stream-c
    (bivalent-input-mixin-b
     #+ccl file-stream
     ngray:fundamental-stream)
  ()
  #+allegro
  (:default-initargs :element-type 'character))

#+gray-streams-input-stream-p
(define-stream-tests bivalent-input-c
  :class bivalent-input-stream-c
  :input t
  :binary t
  :character t
  :extended t)

;;; Basic Bivalent Output Stream

(defclass bivalent-output-stream-a
    (bivalent-output-mixin-a
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests bivalent-output-a
  :class bivalent-output-stream-a
  :output t
  :binary t
  :character t)

;;; Extended Bivalent Output Stream

(defclass bivalent-output-stream-b
    (bivalent-output-mixin-b
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests bivalent-output-b
  :class bivalent-output-stream-b
  :output t
  :binary t
  :character t
  :extended t)

;;; Extended Bivalent Output Stream which relies on support for
;;; generic OUTPUT-STREAM-P.

(defclass bivalent-output-stream-c
    (bivalent-output-mixin-b
     ngray:fundamental-stream)
  ()
  #+allegro
  (:default-initargs :element-type 'character))

#+gray-streams-output-stream-p
(define-stream-tests bivalent-output-c
  :class bivalent-output-stream-c
  :output t
  :binary t
  :character t
  :extended t)

;;; Basic Bivalent Input/Output Stream

(defclass bivalent-io-stream-a
    (bivalent-io-mixin-a
     ngray:fundamental-character-input-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests bivalent-io-a
  :class bivalent-io-stream-a
  :input t
  :output t
  :binary t
  :character t)

;;; Extended Bivalent Input/Output Stream

(defclass bivalent-io-stream-b
    (bivalent-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-character-input-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests bivalent-io-b
  :class bivalent-io-stream-b
  :input t
  :output t
  :binary t
  :character t
  :extended t)

;;; Extended Bivalent Input/Output Stream which relies on support for
;;; generic INPUT-STREAM-P and OUTPUT-STREAM-P.

(defclass bivalent-io-stream-c
    (bivalent-io-mixin-b
     #+ccl file-stream
     ngray:fundamental-stream)
  ()
  #+allegro
  (:default-initargs :element-type 'character))

#+(and gray-streams-input-stream-p gray-streams-output-stream-p)
(define-stream-tests bivalent-io-c
  :class bivalent-io-stream-c
  :input t
  :output t
  :binary t
  :character t
  :extended t)
