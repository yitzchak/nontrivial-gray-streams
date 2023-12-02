(in-package #:nontrivial-gray-streams/test)

(defparameter *invocations* nil)

(defmacro with-invocations (&body body)
  `(let (*invocations*) ,@body))

(defun record-invocation (&rest args)
  (pushnew args *invocations* :test #'equal))

(defun invoked-p (&rest args)
  (and (position args *invocations* :test #'equalp) t))

(defclass binary-input-mixin ()
  ((input-value :reader input-value
                :initarg :input-value
                :initform #())
   (input-index :accessor input-index
                :initform 0)))

(defclass binary-output-mixin ()
  ((output-value :accessor output-value
                 :initform (make-array 16 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0))))
