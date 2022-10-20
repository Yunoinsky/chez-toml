(load-library "./src/toml.ss")
(import (chez-toml))

(display "\nTokenizer Result:\n")
(define fp (open-input-file "./test/example2.toml"))
(pretty-print (tokenizer fp))

(display "\nParser:\n")
(define fp (open-input-file "./test/example.toml"))
(pretty-print (toml-load fp))

(define fp (open-input-file "./test/example.toml"))

(define tree (toml-load fp))
(define ht (to-builtin tree))

(display "\nAssoc list ref test:\n")
(pretty-print (toml-ref tree '(clients data 0 0)))

(display "\nHashtable ref test:\n")
(display (format "~s\n\n" (toml-ref ht '(owner prompt))))



(toml-dump ht (current-output-port))


