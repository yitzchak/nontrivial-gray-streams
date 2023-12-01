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
                :components ((:file "packages")
                             (:file "streams")))))

(defsystem "nontrivial-gray-streams/test"
  :license "MIT"
  :author "Tarn W. Burton"
  :depends-on ("nontrivial-gray-streams"
               "lisp-unit2")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :nontrivial-gray-streams/test :run-all-tests))
  :components ((:module "code"
                :pathname "code/test/"
                :components ((:file "packages")
                             (:file "utility")
                             (:file "binary-input")
                             #+(or)(:file "character-input")
                             #+(or)(:file "character-output")))))
