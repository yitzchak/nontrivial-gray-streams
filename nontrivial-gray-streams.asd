(in-package #:asdf-user)

(defsystem "nontrivial-gray-streams"
  :license "MIT"
  :author "Tarn W. Burton"
  :depends-on ((:feature (:or :abcl :cmucl :genera
                              (:and (:or :clasp :ecl) :gray-streams-module))
                         (:require #:gray-streams)))
  :if-feature (:or :abcl :allegro :ccl :clasp :clisp :cmucl :ecl :genera
                   :lispworks :mezzano :mkcl :sicl :sbcl)
  :in-order-to ((asdf:test-op (asdf:test-op #:nontrivial-gray-streams/test)))
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "streams")))))

(defsystem "nontrivial-gray-streams/test"
  :license "MIT"
  :author "Tarn W. Burton"
  :depends-on ("alexandria"
               "nontrivial-gray-streams"
               "parachute")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :nontrivial-gray-streams/test))
  :components ((:module "code"
                :pathname "code/test/"
                :serial t
                :components ((:file "packages")
                             (:file "mixins")
                             (:file "macros")
                             (:file "test")))))
