(in-package #:nontrivial-gray-streams/test)

(defparameter *invocations* nil)

(defmacro with-invocations (&body body)
  `(let (*invocations*) ,@body))

(defun record-invocation (&rest args)
  (pushnew args *invocations* :test #'equal))

(defun invoked-p (&rest args)
  (and (position args *invocations* :test #'equal) t))
