(in-package #:asdf-user)

(defsystem "nontrivial-gray-streams"
  :license "MIT"
  :author "Tarn W. Burton"
  :depends-on ((:feature (:or :abcl :cmucl :genera
                              (:and :clasp :gray-streams-module))
                         (:require #:gray-streams)))
  :in-order-to ((asdf:test-op (asdf:test-op #:nontrivial-gray-streams/test)))
  :components ((:module "code"
                :components ((:file "packages")
                             (:file "streams")))))

(defsystem "nontrivial-gray-streams/test"
  :license "MIT"
  :author "Tarn W. Burton"
  :depends-on ("nontrivial-gray-streams"
               "parachute")
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :nontrivial-gray-streams/test))
  :components ((:module "code"
                :pathname "code/test/"
                :components ((:file "packages")
                             (:file "test")))))
