(load-library "./src/toml.ss")
(import (chez-toml))

(display "\nTokenizer Result:\n")
(define fp (open-input-file "./test/example.toml"))
(pretty-print (tokenizer fp))

(display "\nParser:\n")
(define fp (open-input-file "./test/example.toml"))
(pretty-print (parser fp))

(define fp (open-input-file "./test/example.toml"))
(define tree (parser fp))

(define ht (to-builtin tree))

(display "\nHashtable ref test:\n")
(pretty-print (toml-ref ht '(clients data 0 0)))

(display "\nAssoc list ref test:\n")
(pretty-print (toml-ref tree '(clients data 0 0)))

