(load-library "./src/toml.so")
(import (chez-toml))

(define fp '())

(define (test-file-input-port)
  (set! fp (open-input-file "./test/example_1.toml")))

(define (colorful-print s)
  (display (format "\n\n[35m[TEST] [1;32m~a[0m\n\n" s)))

(colorful-print "Tokenizer Result:")
(test-file-input-port)
(pretty-print (tokenizer fp))

(colorful-print "Parser:")
(test-file-input-port)
(pretty-print (toml-load fp))

(test-file-input-port)
(define tree (toml-load fp))
(define ht (to-builtin tree))

(colorful-print "Assoc list ref test:")
(pretty-print (toml-ref tree '(clients data 0 0)))

(colorful-print "Hashtable ref test:")
(display (format "~s\n" (toml-ref ht '(owner prompt))))

(colorful-print "Hashtable dump test:")
(toml-dump ht (current-output-port))

(colorful-print "Assoc list dump test:")
(toml-dump tree (current-output-port))
