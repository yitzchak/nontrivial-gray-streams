(in-package #:nontrivial-gray-streams/test)

(defmacro define-stream-tests (parent
                               &key class input output extended
                                    character binary)
  (flet ((test-name (name)
           (alexandria:symbolicate parent "." name)))
    `(progn
       (define-test ,parent)

       (define-test ,(test-name '#:input-stream-p.01)
         :parent ,parent
         (let ((stream (make-instance ',class)))
           (,(if input 'true 'false) (input-stream-p stream))
           (skip-on
            ((not :gray-streams-input-stream-p))
            "INPUT-STREAM-P extension not present"
            (true (invoked-p stream :input-stream-p stream)))))

       (define-test ,(test-name '#:output-stream-p.01)
         :parent ,parent
         (let ((stream (make-instance ',class)))
           (,(if output 'true 'false) (output-stream-p stream))
           (skip-on
            ((not :gray-streams-output-stream-p))
            "OUTPUT-STREAM-P extension not present"
            (true (invoked-p stream :output-stream-p stream)))))

       (define-test ,(test-name '#:streamp.01)
         :parent ,parent
         (let ((stream (make-instance ',class)))
           (true (streamp stream))
           #+(and gray-streams-streamp (not ccl))
           (true (invoked-p stream :streamp stream))))

       ,@(when extended
           `(#+gray-streams-pathname
             (define-test ,(test-name '#:pathname.01)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :pathname #P"fu.bar")))
                 (is equalp #P"fu.bar" (pathname stream))
                 (true (invoked-p stream :pathname stream))))

             #+gray-streams-truename
             (define-test ,(test-name '#:truehname.01)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :pathname #P"fu.bar")))
                 (is equalp #P"fu.bar" (truename stream))
                 (true (invoked-p stream :truename stream))))))

       ,@(when character
           `((define-test ,(test-name '#:stream-element-type.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eq 'character (stream-element-type stream))
                 (true (invoked-p stream :stream-element-type stream))))))

       ,@(when binary
           `((define-test ,(test-name '#:stream-element-type.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eq 'integer (stream-element-type stream))
                 (true (invoked-p stream :stream-element-type stream))))))

       ,@(when input
           `((define-test ,(test-name '#:clear-input.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (clear-input stream))
                 (true (invoked-p stream :stream-clear-input stream))))

             (define-test ,(test-name '#:interactive-stream-p.01)
               :parent ,parent
               (skip-on
                ((not :gray-streams-interactive))
                "Interactive extension not present"
                (let ((stream (make-instance ',class)))
                  (false (interactive-stream-p stream))
                  (true (invoked-p stream :interactive-stream-p stream)))))

             (define-test ,(test-name '#:listen.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (true (listen stream))
                 (true (invoked-p stream :stream-listen stream))
                 ,@(unless (or binary extended)
                     `((true (invoked-p stream :stream-read-char stream))
                       (true (invoked-p stream :stream-unread-char stream #\a))))))

             (define-test ,(test-name '#:listen.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (listen stream))
                 (true (invoked-p stream :stream-listen stream))
                 ,@(unless (or binary extended)
                     `((true (invoked-p stream :stream-read-char stream))))))))

       ,@(when (and input (not extended))
           `(#+gray-streams-file-length
             (define-test ,(test-name '#:file-length.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (fail (file-length stream) 'type-error)))))

       ,@(when (and input extended)
           `(#+gray-streams-file-length
             (define-test ,(test-name '#:file-length.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eql 0 (file-length stream))
                 (true (invoked-p stream :stream-file-length stream nil))))

             (define-test ,(test-name '#:interactive-stream-p.02)
               :parent ,parent
               (skip-on
                ((not :gray-streams-interactive))
                "Interactive extension not present"
                (let ((stream (make-instance ',class
                                             :input-value "ab"
                                             :interactive t)))
                  (true (interactive-stream-p stream))
                  (true (invoked-p stream :interactive-stream-p stream)))))))

       ,@(when (and input binary)
           `((define-test ,(test-name '#:read-byte.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (is eql 97 (read-byte stream nil))
                 (is eql 98 (read-byte stream nil))
                 (false (read-byte stream nil))
                 (true (invoked-p stream :stream-read-byte stream))))

             (define-test ,(test-name '#:read-byte.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (read-byte stream nil))
                 (true (invoked-p stream :stream-read-byte stream))))

             (define-test ,(test-name '#:read-byte.03)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (eql :wibble (read-byte stream nil :wibble))
                 (true (invoked-p stream :stream-read-byte stream))))

             (define-test ,(test-name '#:read-byte.04)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (fail (read-byte stream))
                 (true (invoked-p stream :stream-read-byte stream))))

             #+gray-streams-sequence
             (define-test ,(test-name '#:read-sequence.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab"))
                     (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
                 (is eql 2 (read-sequence sequence stream))
                 (is equalp #(97 98 nil) sequence)
                 (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
                           (invoked-p stream :stream-read-sequence stream sequence 0 3)
                           (invoked-p stream :stream-read-sequence stream sequence nil nil)
                           (invoked-p stream :stream-read-sequence stream sequence nil 3)))))

             #+gray-streams-sequence
             (define-test ,(test-name '#:read-sequence.02)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab"))
                     (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
                 (is eql 1 (read-sequence sequence stream :end 1))
                 (is equalp #(97 nil nil) sequence)
                 (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
                           (invoked-p stream :stream-read-sequence stream sequence nil 1)))))

             #+gray-streams-sequence
             (define-test ,(test-name '#:read-sequence.03)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab"))
                     (sequence (make-array 3 :element-type '(or (unsigned-byte 8) null) :initial-element nil)))
                 (is eql 3 (read-sequence sequence stream :start 1))
                 (is equalp #(nil 97 98) sequence)
                 (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
                           (invoked-p stream :stream-read-sequence stream sequence 1 3)))))))

       ,@(when (and input character)
           `((define-test ,(test-name '#:peek-char.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (is equal #\a (peek-char nil stream))
                 (true (invoked-p stream :stream-peek-char stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))
                       (true (invoked-p stream :stream-unread-char stream #\a))))))

             (define-test ,(test-name '#:peek-char.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (fail (peek-char nil stream))
                 (true (invoked-p stream :stream-peek-char stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:peek-char.03)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eql :wibble (peek-char nil stream nil :wibble))
                 (true (invoked-p stream :stream-peek-char stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:peek-char.04)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (is equal #\b (peek-char #\b stream))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:peek-char.05)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (fail (peek-char #\b stream))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:peek-char.06)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (is equal :wibble (peek-char #\b stream nil :wibble))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:read-char.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (is equal #\a (read-char stream))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:read-char.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (read-char stream nil))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:read-char.03)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (eql :wibble (read-char stream nil :wibble))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:read-char.04)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (fail (read-char stream))
                 (true (invoked-p stream :stream-read-char stream))))

             (define-test ,(test-name '#:read-char-no-hang.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (is equal #\a (read-char-no-hang stream))
                 (true (invoked-p stream :stream-read-char-no-hang stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:read-line.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a")))
                 (is-values (read-line stream)
                            (equal "a")
                            (eql t))
                 (true (invoked-p stream :stream-read-line stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:read-line.02)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "a
b")))
                 (is-values (read-line stream)
                            (equal "a")
                            (eql nil))
                 (true (invoked-p stream :stream-read-line stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:read-line.03)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (fail (read-line stream))
                 (true (invoked-p stream :stream-read-line stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:read-line.04)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is-values (read-line stream nil :wibble)
                            (eql :wibble)
                            (eql t))
                 (true (invoked-p stream :stream-read-line stream))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-read-char stream))))))

             (define-test ,(test-name '#:read-sequence.04)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :input-value "ab"))
                     (sequence (make-array 3 :element-type 'character
                                             :initial-element #\Space)))
                 (is eql 2 (read-sequence sequence stream))
                 (is equalp sequence "ab ")
                 (skip-on ((not :gray-streams-sequence))
                          "Sequence extension not present"
                          (true (or (invoked-p stream :stream-read-sequence stream sequence 0 nil)
                                    (invoked-p stream :stream-read-sequence stream sequence 0 3)
                                    (invoked-p stream :stream-read-sequence stream sequence nil nil)
                                    (invoked-p stream :stream-read-sequence stream sequence nil 3))))))

             (define-test ,(test-name '#:read-sequence.05)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :input-value "ab"))
                     (sequence (make-array 3 :element-type 'character
                                             :initial-element #\Space)))
                 (is eql 1 (read-sequence sequence stream :end 1))
                 (is equalp sequence "a  ")
                 (skip-on ((not :gray-streams-sequence))
                          "Sequence extension not present"
                          (true (or (invoked-p stream :stream-read-sequence stream sequence 0 1)
                                    (invoked-p stream :stream-read-sequence stream sequence nil 1))))))

             (define-test ,(test-name '#:read-sequence.06)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :input-value "ab"))
                     (sequence (make-array 3 :element-type 'character
                                             :initial-element #\Space)))
                 (is eql 3 (read-sequence sequence stream :start 1))
                 (is equalp sequence " ab")
                 (skip-on ((not :gray-streams-sequence))
                          "Sequence extension not present"
                          (true (or (invoked-p stream :stream-read-sequence stream sequence 1 nil)
                                    (invoked-p stream :stream-read-sequence stream sequence 1 3))))))

             (define-test ,(test-name '#:unread-char.01)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :input-value "ab")))
                 (is equal #\a (read-char stream))
                 (false (unread-char #\a stream))
                 (is equal #\a (read-char stream))
                 (true (invoked-p stream :stream-read-char stream))
                 (true (invoked-p stream :stream-unread-char stream #\a))))

             (define-test ,(test-name '#:unread-char.02)
               :parent ,parent
               (let ((stream (make-instance ',class
                                            :input-value "ab")))
                 (is equal #\a (read-char stream))
                 (fail (unread-char #\b stream))
                 (true (invoked-p stream :stream-read-char stream))
                 (true (invoked-p stream :stream-unread-char stream #\b))))))

       ,@(when (and extended input binary)
           `(#+gray-streams-file-position
             (define-test ,(test-name '#:file-position.01)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (is equal 0 (file-position stream))
                 (is eql 97 (read-byte stream nil))
                 (is equal 1 (file-position stream))
                 (true (invoked-p stream :stream-file-position stream nil))))

             #+gray-streams-file-position
             (define-test ,(test-name '#:file-position.02)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (true (file-position stream 1))
                 (is equal 1 (file-position stream))
                 (is eql 98 (read-byte stream nil))
                 (true (invoked-p stream :stream-file-position stream nil))))))

       ,@(when (and extended input character)
           `(#+gray-streams-file-position
             (define-test ,(test-name '#:file-position.03)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (is equal 0 (file-position stream))
                 (is eql #\a (peek-char nil stream nil))
                 (is equal 0 (file-position stream))
                 (is eql #\a (read-char stream nil))
                 (is equal 1 (file-position stream))
                 (true (invoked-p stream :stream-file-position stream nil))))

             #+gray-streams-file-position
             (define-test ,(test-name '#:file-position.04)
               :parent ,parent
               (let ((stream (make-instance ',class :input-value "ab")))
                 (true (file-position stream 1))
                 (is equal 1 (file-position stream))
                 (is eql #\b (read-char stream nil))
                 (true (invoked-p stream :stream-file-position stream nil))))))

       ,@(when output
           `((define-test ,(test-name '#:finish-output.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (finish-output stream))
                 (true (invoked-p stream :stream-finish-output stream))))

             (define-test ,(test-name '#:force-output.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (force-output stream))
                 (true (invoked-p stream :stream-force-output stream))))

             (define-test ,(test-name '#:clear-output.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (clear-output stream))
                 (true (invoked-p stream :stream-clear-output stream))))))

       ,@(when (and output binary)
           `((define-test ,(test-name '#:write-byte.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is equal 97 (write-byte 97 stream))
                 (is equalp #(97) (output-value stream))
                 (true (invoked-p stream :stream-write-byte stream 97))))

             #+gray-streams-sequence
             (define-test ,(test-name '#:write-sequence.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (write-sequence #(10 11 12 13) stream)
                 (write-sequence #(14 15 16 17) stream :start 1 :end 3)
                 (is equalp #(10 11 12 13 15 16) (output-value stream))
                 (true (or (invoked-p stream :stream-write-sequence stream #(10 11 12 13) nil nil)
                           (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 nil)
                           (invoked-p stream :stream-write-sequence stream #(10 11 12 13) 0 4)))
                 (true (invoked-p stream :stream-write-sequence stream #(14 15 16 17) 1 3))))))

       ,@(when (and output character)
           `((define-test ,(test-name '#:fresh-line.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eql #\a (write-char #\a stream))
                 (true (fresh-line stream))
                 (true (invoked-p stream :stream-write-char stream #\Newline))))

             (define-test ,(test-name '#:write-char.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is eql #\a (write-char #\a stream))
                 (is eql #\b (write-char #\b stream))
                 (is equal "ab" (output-value stream))
                 (true (invoked-p stream :stream-write-char stream #\a))
                 (true (invoked-p stream :stream-write-char stream #\b))))

             #+gray-streams-sequence
             (define-test ,(test-name '#:write-sequence.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (write-sequence "abcd" stream)
                 (write-sequence "efgh" stream :start 1 :end 3)
                 (is equal "abcdfg" (output-value stream))
                 (true (or (invoked-p stream :stream-write-sequence stream "abcd" nil nil)
                           (invoked-p stream :stream-write-sequence stream "abcd" 0 nil)
                           (invoked-p stream :stream-write-sequence stream "abcd" 0 4)))
                 (true (invoked-p stream :stream-write-sequence stream "efgh" 1 3))))

             (define-test ,(test-name '#:write-string.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (is equal "ab" (write-string "ab" stream))
                 (is equal "ab" (output-value stream))
                 (true (or (invoked-p stream :stream-write-string stream "ab" nil nil)
                           (invoked-p stream :stream-write-string stream "ab" 0 nil)
                           (invoked-p stream :stream-write-string stream "ab" 0 2)))
                 ,@(unless extended
                     `((true (invoked-p stream :stream-write-char stream #\a))
                       (true (invoked-p stream :stream-write-char stream #\b))))))

             (define-test ,(test-name '#:stream-advance-to-column.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (ngray:stream-advance-to-column stream 10))))

             (define-test ,(test-name '#:stream-start-line-p.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (write-char #\a stream)
                 (false (ngray:stream-start-line-p stream))
                 (true (invoked-p stream :stream-start-line-p stream))
                 (true (invoked-p stream :stream-line-column stream))))

             (define-test ,(test-name '#:terpri.01)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (false (terpri stream))
                 (print (invocations stream))
                 (skip-on (:ccl)
                          "CCL skips the call to stream-terpri"
                          (true (invoked-p stream :stream-terpri stream)))
                 (true (invoked-p stream :stream-write-char stream #\Newline))))))

       ,@(when (and output character extended)
           `(#+gray-streams-line-length
             (define-test ,(test-name '#:stream-line-length.01)
               :parent ,parent
               (let ((stream (make-instance ',class))
                     (*print-right-margin* nil)
                     (*print-pretty* nil))
                 (format stream "~<aaaa~:;bbbb~>")
                 (true (invoked-p stream :stream-line-length stream))))

             (define-test ,(test-name '#:stream-start-line-p.02)
               :parent ,parent
               (let ((stream (make-instance ',class)))
                 (true (ngray:stream-start-line-p stream))
                 (true (invoked-p stream :stream-start-line-p stream))
                 (true (invoked-p stream :stream-line-column stream)))))))))
