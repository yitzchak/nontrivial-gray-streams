(in-package #:nontrivial-gray-streams/test)

(defparameter *invocations* nil)

(defmacro with-invocations (&body body)
  `(let (*invocations*) ,@body))

(defun record-invocation (&rest args)
  (pushnew args *invocations* :test #'equal))

(defun invoked-p (&rest args)
  (and (position args *invocations* :test #'equal) t))

(defun run-all-tests (&optional exitp)
  (uiop:quit (if (lisp-unit2::failed-assertions
                  (with-summary ()
                    (run-tests :package :nontrivial-gray-streams/test)))
                 1
                 0)))
