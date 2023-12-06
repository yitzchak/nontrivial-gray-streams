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
     ngray:fundamental-binary-input-stream
     ngray:fundamental-binary-output-stream)
  ())

(define-stream-tests binary-io-b
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
     ngray:fundamental-character-input-stream
     ngray:fundamental-character-output-stream)
  ())

(define-stream-tests character-io-b
  :class character-io-stream-b
  :input t
  :output t
  :character t
  :extended t)
