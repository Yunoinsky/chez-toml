;; Chez-TOML
;; =========
;; Author: Yunoinsky Chen
;; Description: TOML v1.0.0 parser implemented in Chez Scheme 9.5.8
;; version: λ-0.1
;; URL: https://github.com/Yunoinsky/chez-toml

(library (chez-toml (0 1))
  (export toml-load tokenizer to-builtin
          toml-ref toml-set!
          ht-toml-deep-cells toml-display
          toml-dump)
  (import (chezscheme))

  (define-syntax push!
    (syntax-rules ()
      [(_ rl item) (set! rl (cons item rl))]))

  (define-syntax push-cdr!
    (syntax-rules ()
      [(_ pair item) (set-cdr! pair (cons item (cdr pair)))]))

  (define-syntax pop!
    (syntax-rules ()
      [(_ rl) (let ([a (car rl)])
                (set! rl (cdr rl))
                a)]))

  (define-syntax inc!
    (syntax-rules ()
      [(_ x) (set! x (+ x 1))]
      [(_ x a) (set! x (+ x a))]))

  (define (toml-load fp)
    (let ([tk-buffer '()]
          [root (cons 'root '())])
      (define (next)
        (if (null? tk-buffer)
            (take-token fp)
            (pop! tk-buffer)))
      (define (ref cursor key)
        (or (assoc key (cdr cursor))
            (and (not (integer? key))
                 (not (null? (cdr cursor)))
                 (pair? (cadr cursor))
                 (integer? (caadr cursor))
                 (ref (cadr cursor) key))
            (let ([new-pair (cons key '())])
              (push-cdr! cursor new-pair)
              new-pair)))
      (define (parse-newline)
        (let ([token (next)])
          (unless (or (eq? token 'newline)
                      (eq? token 'eof))
            (error token "Parser Error: need newline"))))
      (define (parse-line cursor)
        (let ([token (next)])
          (case token
            ['eof 'eof]
            ['newline (parse-line cursor)]
            ['o-sbracket (parse-table-head)]
            ['o-d-sbracket (parse-list-head)]
            [else
             (push! tk-buffer token)
             (parse-pair cursor)
             (parse-newline)
             (parse-line cursor)])))
      (define (parse-pair cursor)
        (let ([key (parse-key cursor 'pair)])
          (set-cdr! key
                    (parse-value))))
      (define (parse-table-head)
        (let ([table-stem (parse-key root 'table-head)])
          (parse-newline)
          (parse-line table-stem)))
      (define (parse-list-head)
        (let* ([list-stem (parse-key root 'list-head)]
               [ind (length (cdr list-stem))])
          (parse-newline)
          (parse-line (ref list-stem ind))))
      (define (parse-key cursor type #;'(pair table-head list-head))
        (let ([token (next)])
          (unless (pair? token)
            (error token "Parser Error: invalid key"))
          (case (car token)
            ['literal
             (set! tk-buffer
                   (append (literal-dot-split token)
                           tk-buffer))
             (parse-key cursor type)]
            ['str
             (parse-key-punc (ref cursor (cdr token)) type)]
            [else (error token "Parser Error: invalid key")])))
      (define (parse-key-punc cursor type)
        (let ([token (next)])
          (if (eq? token 'dot)
              (parse-key cursor type)
              (if (eq? type (case token
                              ['equal 'pair]
                              ['c-sbracket 'table-head]
                              ['c-d-sbracket 'list-head]))
                  cursor
                  (error
                   token "Parser Error: invalid key punct")))))
      (define (parse-literal token)
        (let ([v (literal-eval token)])
          (when (and (pair? v) (eq? (car v) 'date))
            (let* ([next-token (next)]
                   [t
                    (and (pair? next-token)
                         (eq? (car next-token) 'literal)
                         (string->time-list (cdr next-token)))])
              (if t
                  (set! v (compose-date-time v t))
                  (push! tk-buffer next-token))))
          v))
      (define (parse-value)
        (let ([token (next)])
          (case token
            ['o-sbracket (parse-list)]
            ['o-d-sbracket (push! tk-buffer 'o-sbracket)
                           (parse-list)]
            ['o-bracket (parse-table)]
            ['c-sbracket 'end-list]
            ['c-d-sbracket (push! tk-buffer 'c-sbracket)
                           (push! tk-buffer 'c-sbracket)
                           (parse-value)]
            [else
             (unless (pair? token)
               (error
                token
                "Parser Error: invalid value, not pair"))
             (let ([s (cdr token)])
               (case (car token)
                 ['str s]
                 ['literal (parse-literal token)]
                 [else
                  token
                  "Parser Error: invalid literal or str"]))])))
      (define (parse-list)
        (let ([result '()])
          (consume-spaces&newlines fp)
          (let loop ([ind 0])
            (let ([v (parse-value)])
              (or
               (and (eq? v 'end-list)
                    result)
               (and (set! result (cons (cons ind v)
                                       result))
                    (consume-spaces&newlines fp)
                    (case (parse-comma)
                      ['next (begin
                               (consume-spaces&newlines fp)
                               (loop (+ ind 1)))]
                      ['end-list result]
                      [else
                       (error
                        #f
                        "Parse Error: invalid list")])))))))
      (define (parse-table)
        (let ([table-stem (cons 'head '())])
          (or 
           (let ([token (next)])
             (if (eq? token 'c-bracket)
                 (cdr table-stem)
                 (begin
                   (push! tk-buffer token)
                   #f)))
           (let loop ()
             (parse-pair table-stem)
             (case (parse-comma)
               ['next (loop)]
               ['end-table (cdr table-stem)]
               [else
                (error #f
                       "Parser Error: invalid table")])))))
      (define (parse-comma)
        (let ([token (next)])
          (case token
            ['comma 'next]
            ['c-sbracket 'end-list]
            ['c-bracket 'end-table]
            [else
             (error token "Parser Error: need comma")])))
      (parse-line root)
      (deep-reverse root)))
  
  (define (tokenizer fp)
    (do ([token (take-token fp) (take-token fp)]
         [is-primary-key #t (if is-primary-key
                                (not (eq? token 'equal))
                                (and (null? br-stack)
                                     (eq? token 'newline)))]
         [is-inline-key
          #f
          (if is-inline-key
              (not (eq? token 'equal))
              (or (and (not (null? br-stack))
                       (eq? (car br-stack) 'o-bracket)
                       (eq? token 'comma))
                  (eq? token 'o-bracket)))]
         [last-token '() token]
         [br-stack '() (if is-primary-key
                           '()
                           (bracket-update br-stack token))]
         [tokens '()
                 (if (and (eq? token 'newline)
                          (or (eq? last-token 'newline)
                              (null? last-token)))
                     tokens
                     (if (and (pair? token)
                              (eq? (car token) 'literal))
                         (if (or is-primary-key
                                 is-inline-key)
                             (append (literal-dot-split token)
                                     tokens)
                             (cons (literal-eval token)
                                   tokens))
                         (cons token
                               tokens)))])
        ((eq? token 'eof) (reverse tokens))))

  (define (to-builtin root)
    (to-builtin-content (cdr root)))

  (define (toml-ref v path)
    (when (symbol? path)
      (set! path (cons path '())))
    (cond
     [(hashtable? v) (ht-toml-ref v path)]
     [(eq? (car v) 'root) (al-toml-ref (cdr v) path)]
     [else (error v "Invalid toml data")]))

  (define (toml-set! node path v)
    (when (symbol? path)
      (set! path (cons path '())))
    (cond
     [(hashtable? node) (ht-toml-set! node path v)]
     [(eq? (car node) 'root) (al-toml-set! node path v)]
     [else (error v "Invalid toml data")]))

  (define (ht-toml-deep-cells node)
    (cond
     [(hashtable? node)
      (vector->list
       (vector-map
       (lambda (p)
         (cons (car p)
               (ht-toml-deep-cells (cdr p))))
       (hashtable-cells node)))]
     [(vector? node)
      (vector-map
       ht-toml-deep-cells
       node)]
     [else node]))

  (define (toml-display root)
    (cond
     [(hashtable? root)
      (pretty-print (ht-toml-deep-cells root))]
     [(eq? (car root) 'root)
      (pretty-print (cdr root))]
     [else (error root "Invalid toml data")]))

  (define (toml-dump root fp)
    ((cond
      [(hashtable? root) ht-toml-dump]
      [(eq? (car root) 'root) al-toml-dump]
      [else (error root "Writer Error: invalid toml data")])
     root fp))
  
  (define (char-space? c)
    (and (char? c)
         (exists (lambda (c-i)
                   (char=? c c-i))
                 '(#\space
                   #\tab))))

  (define (char-newline? c)
    (and (char? c)
         (exists (lambda (c-i)
                   (char=? c c-i))
                 '(#\newline
                   #\return))))

  (define (char-dquote? c)
    (and (char? c) (char=? c #\")))

  (define (char-quote? c)
    (and (char? c) (char=? c #\')))

  (define (char-o-sbracket? c)
    (and (char? c) (char=? c #\[)))

  (define (char-c-sbracket? c)
    (and (char? c) (char=? c #\])))

  (define (char-bare? c)
    (and (char? c)
         (or (char-ci<=? #\a c #\z)
             (char-numeric? c)
             (char=? #\_ c)
             (char=? #\- c))))

  (define (char-literal? c)
    (and (char? c)
         (or (char-bare? c)
             (char=? #\+ c)
             (char=? #\. c)
             (char=? #\: c))))

  (define (char-hex? c)
    (and (char? c)
         (or (char-numeric? c)
             (char-ci<=? #\a c #\f))))

  (define (char-oct? c)
    (and (char? c)
         (char<=? #\0 c #\7)))

  (define (char-bin? c)
    (and (char? c)
         (or (char=? #\0)
             (char=? #\1))))

  (define (string-integer? str)
    (let ([i0 0]
          [char (string-ref str 0)]
          [not-int #f]
          [char-integer? char-numeric?]
          [str-l (string-length str)])
      (if (or (char=? char #\+)
              (char=? char #\-))
          (if (= str-l 1)
              (set! not-int #t)
              (if (and (char=? (string-ref str 1) #\0)
                       (> str-l 2))
                  (set! not-int #t)
                  (set! i0 1)))
          (when (char=? #\0 char)
            (if (= str-l 2)
                (set! not-int #t)
                (unless (= str-l 1)
                  (set! i0 2)
                  (case (string-ref str 1)
                    [#\b (set! char-integer? char-bin?)]
                    [#\o (set! char-integer? char-oct?)]
                    [#\x (set! char-integer? char-hex?)]
                    [else (set! not-int #t)])))))
      (do ([i i0 (+ i 1)])
          ((or not-int
               (= i str-l))
           (not not-int))
        (set! char (string-ref str i))
        (unless (or (char-integer? char)
                    (and (char=? char #\_)
                         (< i0 i (- str-l 1))
                         (char-integer?
                          (string-ref str (- i 1)))
                         (char-integer?
                          (string-ref str (+ i 1)))))
          (set! not-int #t)))))

  (define (string-extract str c)
    (let ([char-list '()]
          [str-l (string-length str)])
      (do ([i 0 (+ i 1)])
          ((= i str-l)
           (rlist->string char-list))
        (let ([char (string-ref str i)])
          (when (not (char=? char c))
            (push! char-list char))))))

  (define (string->integer str)
    (and
     (string-integer? str)
     (let ([str-e (string-extract str #\_)])
       (if (char=? (string-ref str 0) #\0)
           (let ([str-n (substring str-e
                                   2 (string-length str-e))])
             (case (string-ref str 1)
               [#\x (string->number str-n 16)]
               [#\o (string->number str-n 8)]
               [#\b (string->number str-n 2)]
               [else (string->number str-n)]))
           (string->number str-e)))))

  (define (string-float? str)
    (let ([str-l (string-length str)])
      (define (sign-step i)
        (if (= i str-l)
            #f
            (let ([c (string-ref str i)])
              (only-number-step (+ i (if (or (char=? c #\+)
                                             (char=? c #\-))
                                         1
                                         0))
                                'int))))
      (define (only-number-step i flag)
        (if (= i str-l)
            #f
            (let ([c (string-ref str i)])
              (if (char-numeric? c)
                  (if (and (char=? c #\0) (eq? flag 'int))
                      (only-flag-step (+ i 1))
                      (number-step (+ i 1) flag))
                  #f))))
      (define (only-flag-step i)
        (if (= i str-l)
            #f
            (let ([c (string-ref str i)])
              (case c
                [(#\e #\E) (only-number-step (+ i 1) 'exp)]
                [#\. (only-number-step (+ i 1) 'float)]
                [else #f]))))
      (define (number-step i flag)
        (if (= i str-l)
            (if (eq? flag 'int)
                #f
                #t)
            (let ([c (string-ref str i)])
              (case c
                [(#\_) (only-number-step (+ i 1) flag)]
                [(#\e #\E) (if (eq? flag 'exp)
                               #f
                               (only-number-step (+ i 1) 'exp))]
                [#\. (if (eq? flag 'int)
                         (only-number-step (+ i 1) 'float)
                         #f)]
                [else (if (char-numeric? c)
                          (number-step (+ i 1) flag)
                          #f)]))))
      (sign-step 0)))

  (define (string->float str)
    (and
     (string-float? str)
     (string->number (string-extract str #\_))))

  (define (second->utc-str s)
    (if (= s 0)
        "Z"
        (let-values
            ([(sign) (if (< s 0)
                         '-
                         '+)]
             [(h m) (div-and-mod (abs (/ s 60)) 60)])
          (format "~a~2,,,'0@s:~2,,,'0@s" sign h m))))

  
  (define (dt-l->dt-str dt-l)
    (let ([dt (cdr dt-l)]
          [affix (car dt-l)])
      (if (date? dt)
          (case affix
            ['date
             (format "~4,,,'0@s-~2,,,'0@s-~2,,,'0@s"
                     (date-year dt)
                     (date-month dt)
                     (date-day dt))]
            ['time
             (string-append
              (format "~2,,,'0@s:~2,,,'0@s:~2,,,'0@s"
                      (date-hour dt)
                      (date-minute dt)
                      (date-second dt))
              (let ([ns (date-nanosecond dt)])
                (if (= 0 ns)
                    ""
                    (let ([ns-str
                           (format "~f"
                                   (/ ns 10e9))])
                      (substring ns-str
                                 1
                                 (string-length ns-str)))))
              (if (date-zone-name dt)
                  ""
                  (second->utc-str (date-zone-offset dt))))]
            ['date-time
             (string-append
              (dt-l->dt-str (cons 'date dt))
              "T"
              (dt-l->dt-str (cons 'time dt)))])
        (case affix
          ['date
           (apply format
                  (cons
                   "~4,,,'0@s-~2,,,'0@s-~2,,,'0@s"
                   (cdr dt-l)))]
          ['time
           (string-append
            (apply format
                   (cons
                    "~2,,,'0@s:~2,,,'0@s:~2,,,'0@s"
                    (list-head (cdr dt-l) 3)))
            (let ([ns-l (cddddr dt-l)])
              (if (or (null? ns-l)
                      (= 0 (car ns-l)))
                  ""
                  (let ([ns-str (format "~f"
                                        (/ (car ns-l) 10e9))])
                    (substring ns-str
                               1
                               (string-length ns-str)))))
            (if (or (null? (cddddr dt-l))
                    (null? (cdr (cddddr dt-l))))
                ""
                (second->utc-str (cadr (cddddr dt-l)))))]
          ['date-time
           (string-append
            (dt-l->dt-str
             (cons 'date (list-head (cdr dt-l) 3)))
            "T"
            (dt-l->dt-str
             (cons 'time (cddddr dt-l))))]))))

  (define (string->time-list str)
    (let ([str-l (string-length str)]
          [h #f]
          [m #f]
          [s #f]
          [ns 0]
          [i-loc 8]
          [offset #f])
      (define (get-number-from i0)
        (do ([i (+ i0 1) (+ i 1)])
            ((or (= str-l i)
                 (not (char-numeric? (string-ref str i))))
             (if (= i (+ i0 1))
                 #f
                 (cons i
                       (flonum->fixnum
                        (* 1000000000
                           (string->number (substring str
                                                      i0 i)))))))))
      (define (utc-location c)
        (and (= str-l (+ 6 i-loc))
             (char=? #\: (string-ref str (+ 3 i-loc)))
             (let ([o-h (string->number
                         (substring str
                                    (+ 1 i-loc)
                                    (+ 3 i-loc)))]
                   [o-m (string->number
                         (substring str
                                    (+ 4 i-loc)
                                    (+ 6 i-loc)))])
               (and o-h o-m
                    (let ([valid-time #t])
                      (string-for-each
                       (lambda (c)
                         (set! valid-time
                               (or (char-numeric? c)
                                   (char=? c #\:))))
                       (substring str
                                  (+ 1 i-loc) (+ 6 i-loc)))
                      valid-time)
                    (< o-h 24)
                    (< o-m 60)
                    (set! offset (* 60 (+ (* 60 o-h) o-m)))
                    (list 'time h m
                          s ns
                          ((if (char=? #\- c) - +)
                           offset))))))
      (if (< str-l 8)
          #f
          (and (let ([valid-time #t]
                     [sub-s (substring str 0 8)])
                 (string-set! sub-s 2 #\0)
                 (string-set! sub-s 5 #\0)
                 (string-for-each (lambda (c)
                                    (set! valid-time
                                          (char-numeric? c)))
                                  sub-s)
                 valid-time)
               (char=? (string-ref str 2) #\:)
               (char=? (string-ref str 5) #\:)
               (set! h (string->number (substring str 0 2)))
               h (< h 24)
               (set! m (string->number (substring str 3 5)))
               m (< m 60)
               (set! s (string->number (substring str 6 8)))
               s (< s 60)
               (if (= str-l 8)
                   (list 'time h m s 0)
                   (let ([c (string-ref str 8)])
                     (when (char=? c #\.)
                       (let ([ns-pair (get-number-from 8)])
                         (and ns-pair
                              (set! ns (cdr ns-pair))
                              (set! i-loc (car ns-pair)))))
                     (if (= str-l i-loc)
                         (list 'time h m s ns)
                         (let ([c (string-ref str i-loc)])
                           ;; UTC information
                           (case c
                             [(#\+ #\-) (utc-location c)]
                             [#\Z (list 'time h m s ns 0)]
                             [else #f])))))))))

  (define (string->date-list str)
    (let ([str-l (string-length str)]
          [year #f]
          [month #f]
          [day #f])
      (and (> str-l 9)
           (char=? #\-
                   (string-ref str 4)
                   (string-ref str 7))
           (let ([date-s (substring str
                                    0 10)])
             (string-set! date-s 4 #\0)
             (string-set! date-s 7 #\0)
             (andmap char-numeric?
                     (string->list date-s)))
           (set! year (string->number (substring str 0 4)))
           year
           (set! month (string->number (substring str 5 7)))
           month
           (< 0 month 13)
           (set! day (string->number (substring str 8 10)))
           day
           (< 0 day (case month
                      [(1 3 5 7 8 10 12) 32]
                      [2 (if (or (= (mod year 400) 0)
                                 (and (= (mod year 4) 0)
                                      (not (= (mod year 100) 0))))
                             30 29)]
                      [else 31]))
           (if (= str-l 10)
               (list 'date year month day)
               (and (char=? (string-ref str 10)
                            #\T)
                    (let ([time (string->time-list (substring str 11 str-l))])
                      (and time
                           (append (list 'date-time
                                         year month day)
                                   (cdr time)))))))))

  (define (compose-date-time date time)
    (cons 'date-time
          (append (cdr date)
                  (cdr time))))

  (define (time-list->date t)
    (let ([r-time (reverse (cdr t))]
          [base-date '(1 1 1970)])
      (cons 'time      
            (apply make-date
                   (if (= (length r-time) 5)
                       (append (cdr r-time)
                               base-date
                               (list (car r-time)))
                       (append r-time
                               base-date))))))

  (define (date-list->date d)
    (cons 'date
          (apply make-date
                 (append '(0 0 0 0)
                         (reverse (cdr d))))))

  (define (date-time-list->date dt)
    (let ([r-date-time (reverse (cdr dt))])
      (cons 'date-time
            (apply make-date
                   (if (= (length r-date-time) 8)
                       (append (cdr r-date-time)
                               (list (car r-date-time)))
                       r-date-time)))))

  (define (literal-eval literal)
    (let ([str (cdr literal)])
      (case str
        ["true" #t]
        ["fasle" #f]
        [("+inf" "inf") +inf.0]
        ["-inf" -inf.0]
        [("nan" "+nan" "-nan") +nan.0]
        [else (or
               (string->integer str)
               (string->float str)
               (string->date-list str)
               (string->time-list str)
               (error #f "Token Error: invalid value"))])))

  (define (rlist->string rlist)
    (list->string (reverse rlist)))

  (define (consume-spaces fp)
    (do ([char (lookahead-char fp) (lookahead-char fp)])
        ((not (char-space? char)) fp)
      (get-char fp)))

  (define (consume-spaces&newlines fp)
    (do ([char (lookahead-char fp) (lookahead-char fp)])
        ((not (or (char-space? char) (char-newline? char))) fp)
      (get-char fp)))

  (define (take-newline fp)
    (unless (let ([c (get-char fp)])
              (or
               (eof-object? c)
               (char=? c #\newline)
               (char=? (get-char fp) #\newline)))
      (error #f "Token Error: invalid newline"))
    (consume-spaces&newlines fp)
    'newline)

  (define (take-comment fp)
    (do ([char (lookahead-char fp) (lookahead-char fp)])
        ((or (char-newline? char) (eof-object? char))
         (take-newline fp))
      (get-char fp)))

  (define (take-chars fp n)
    (do ([i 0 (+ i 1)]
         [clist '()
                (cons (get-char fp) clist)])
        ((= i n) (rlist->string clist))))

  (define (take-string fp)
    (do ([char (lookahead-char fp) (lookahead-char fp)]
         [dquote-n 0 (+ dquote-n 1)])
        ((or (not (char-dquote? char))
             (= dquote-n 3))
         (cons 'str
               (case dquote-n
                 [1 (take-s-string fp)]
                 [2 ""]
                 [3 (take-m-string fp)])))
      (get-char fp)))

  (define (take-lstring fp)
    (let ([quote-n 0])
      (do ([char (lookahead-char fp) (lookahead-char fp)]
           [quote-n 0 (+ quote-n 1)])
          ((or (not (char-quote? char))
               (= quote-n 3))
           (cons 'lstr
                 (case quote-n
                   [1 (take-s-lstring fp)]
                   [2 ""]
                   [3 (take-m-lstring fp)])))
        (get-char fp))))

  (define (take-m-lstring fp)
    (let ([char-list '()]
          [quote-n 0])
      (consume-spaces&newlines fp)
      (do ([char (lookahead-char fp) (lookahead-char fp)])
          ((and (not (char-quote? char))
                (>= quote-n 3))
           (rlist->string (cdddr char-list)))
        (when (and (eof-object? char)
                   (< quote-n 3))
          (error #f "Token Error: unpaired quotes"))
        (case char
          [#\'
           (inc! quote-n)
           (when (>= quote-n 6)
             (error #f "Token Error: too many quotes"))
           (push! char-list (get-char fp))]
          [else
           (set! quote-n 0)
           (push! char-list (get-char fp))]))))

  (define (take-m-string fp)
    (let ([char-list '()]
          [dquote-n 0])
      (consume-spaces&newlines fp)
      (do ([char (lookahead-char fp) (lookahead-char fp)])
          ((and (not (char-dquote? char))
                (>= dquote-n 3))
           (rlist->string (cdddr char-list)))
        (when (and (eof-object? char)
                   (< dquote-n 3))
          (error #f "Token Error: unpaired dquotes"))
        (case char
          [#\\
           (let ([escape (take-escape fp)])
             (when escape (push! char-list escape)))]
          [#\"
           (inc! dquote-n)
           (when (>= dquote-n 6)
             (error #f "Token Error: too many dquotes"))
           (push! char-list (get-char fp))]
          [else
           (set! dquote-n 0)
           (push! char-list (get-char fp))]))))

  (define (take-s-lstring fp)
    (let ([char-list '()])
      (do ([char (lookahead-char fp) (lookahead-char fp)])
          ((char-quote? char)
           (get-char fp)
           (rlist->string char-list))
        (when (char-newline? char)
          (error #f "Token Error: newline in single-line literal string"))
        (push! char-list char)
        (get-char fp))))

  (define (take-s-string fp)
    (let ([char-list '()])
      (do ([char (lookahead-char fp) (lookahead-char fp)])
          ((char-dquote? char)
           (get-char fp)
           (rlist->string char-list))
        (case char
          [(#\newline #\return)
           (error #f "Token Error: newline in single-line string")]
          [#\\
           (push! char-list (take-escape fp))]
          [else
           (push! char-list (get-char fp))]))))

  (define escape-codes
    '((#\b . #\backspace)
      (#\t . #\tab)
      (#\n . #\newline)
      (#\f . #\page)
      (#\r . #\return)
      (#\" . #\")
      (#\\ . #\\)))

  (define (take-escape fp)
    (get-char fp)
    (let ([c (get-char fp)])
      (case c
        [#\u (take-unicode fp 4)]
        [#\U (take-unicode fp 8)]
        [(#\tab #\space #\newline #\return)
         (consume-spaces&newlines fp)
         #f]
        [else
         (let ([apair (assv c escape-codes)])
           (if apair
               (cdr apair)
               (error #f "Token Error: unspecific escape")))])))

  (define (take-unicode fp n)
    (unless (or (= n 4)
                (= n 8))
      (error #f "Token Error: wrong unicode number"))
    (integer->char (string->number (take-chars fp n) 16)))

  (define (take-literal fp)
    (let ([char-list '()])
      (do ([char (lookahead-char fp) (lookahead-char fp)])
          ((not (char-literal? char))
           (cons 'literal (rlist->string char-list)))
        (push! char-list (get-char fp)))))

  (define (take-punctuation fp)
    (let ([char (get-char fp)])
      (case char
        [#\= 'equal]
        [#\. 'dot]
        [#\, 'comma]
        [#\{ 'o-bracket]
        [#\} 'c-bracket]
        [#\[ (let ([char (lookahead-char fp)])
               (if (char-o-sbracket? char)
                   (begin (get-char fp)
                          'o-d-sbracket)
                   'o-sbracket))]
        [#\] (let ([char (lookahead-char fp)])
               (if (char-c-sbracket? char)
                   (begin (get-char fp)
                          'c-d-sbracket)
                   'c-sbracket))])))
  (define (take-token fp)
    (consume-spaces fp)
    (let ([char (lookahead-char fp)])
      (case char
        [#\# (take-comment fp)]
        [#\" (take-string fp)]
        [#\' (take-lstring fp)]
        [(#\return #\newline) (take-newline fp)]
        [(#\= #\. #\,
          #\{ #\} #\[ #\]) (take-punctuation fp)]
        [else
         (if (eof-object? char)
             'eof
             (if (char-literal? char)
                 (take-literal fp)
                 (error
                  #f
                  "Token Error: Unmatched token character"
                  char)))])))
  (define-syntax switch!
    (syntax-rules ()
      [(_ x) (set! x (not x))]))

  (define (bracket-update br-stack token)
    (case token
      ['o-bracket (cons token br-stack)]
      ['c-bracket (if (eq? (car br-stack) 'o-bracket)
                      (cdr br-stack)
                      (error #f "Token Error: Unmatched bracket"))]
      ['o-sbracket (cons token br-stack)]
      ['c-sbracket (if (eq? (car br-stack) 'o-sbracket)
                       (cdr br-stack)
                       (error #f "Token Error: Unmatched sbracket"))]
      [else br-stack]))

  (define (literal-dot-split literal)
    (let ([str (cdr literal)]
          [keys '()]
          [char-list '()])
      (let ([str-l (string-length str)])
        (do ([i 0 (+ i 1)])
            ((= str-l i)
             (unless (null? char-list)
               (push! keys
                      (cons 'str
                            (rlist->string char-list))))
             (reverse keys))
          (let ([char (string-ref str i)])
            (if (eq? char #\.)
                (begin
                  (push! keys
                         (cons 'str
                               (rlist->string char-list)))
                  (push! keys 'dot)
                  (set! char-list '()))
                (if (char-bare? char)
                    (push! char-list char)
                    (error #f "Token Error: Invalid Charset for Key"))))))))

  
  (define (atom-node? node)
    (or (not (pair? (cdr node)))
        (symbol? (cadr node))))
  
  (define (deep-reverse node)
    (cons
     (car node)
     (let rev ([ls (cdr node)]
               [new '()])
       (if (null? ls)
           new
           (rev (cdr ls)
                (cons
                 (let ([new-node (car ls)])
                   (if (atom-node? new-node)
                       new-node
                       (deep-reverse new-node)))
                 new))))))
  
  (define (to-builtin-content al)
    (cond
     [(null? al) (make-eq-hashtable)]
     [(atom? al) al]
     [(symbol? (car al))
      ((case (car al)
         ['time time-list->date]
         ['date date-list->date]
         ['date-time date-time-list->date])
       al)]
     [(integer? (caar al))
      (list->vector
       (map (lambda (p)
              (to-builtin-content (cdr p)))
            al))]
     [else
      (let ([ht (make-eq-hashtable)])
        (map (lambda (p)
               (hashtable-set!
                ht
                (string->symbol
                 (car p))
                (to-builtin-content (cdr p))))
             al)
        ht)]))

  (define (ht-toml-ref v path)
    (if (null? path)
        v
        (cond
         [(hashtable? v)
          (ht-toml-ref (hashtable-ref v
                                   (car path)
                                   '())
                    (cdr path))]
         [(vector? v)
          (ht-toml-ref (vector-ref v
                                   (car path))
                    (cdr path))]
         [else
          (error v "Invalid toml hashtable")])))

  (define (al-toml-ref v path)
    (if (null? path)
        v
        (cond
         [(integer? (caar v))
          (al-toml-ref (cdr (list-ref v
                                      (car path)))
                       (cdr path))]
         [(string? (caar v))
          (al-toml-ref (cdr (assoc
                             (symbol->string (car path))
                             v))
                       (cdr path))]
         [else
          (error v "Invalid toml assoc list")])))

  (define (ht-toml-set! node path v)
    (if (null? (cdr path))
        (cond
         [(hashtable? node)
          (hashtable-set! node (car path) v)]
         [(vector? node)
          (vector-set! node (car path) v)]
         [else node "Invalid toml hashtable"])
        (cond
         [(hashtable? node)
          (ht-toml-set!
           (or (hashtable-ref node (car path) #f)
               (let ([ht (make-eq-hashtable)])
                 (hashtable-set! node (car path) ht)
                 ht))
           (cdr path) v)]
         [(vector? node)
          (ht-toml-set!
           (vector-ref node (car path))
           (cdr path)
           v)]
         [else node "Invalid toml hashtable"])))

  (define (al-toml-set! oc path v)
    (let* ([key (symbol->string (car path))]
           [cursor (assoc key
                          (cdr oc))])
      (if (null? (cdr path))
          (if cursor
              (set-cdr! cursor v)
              (if (integer? (caadr oc))
                  (error oc "Array index outof bound")
                  (push-cdr!
                   cursor
                   (cons key v))))
          (if cursor
              (al-toml-set! (cdr cursor) (cdr path) v)
              (if (integer? (caadr oc))
                  (error oc "Array index outof bound")
                  (let ([new-cursor
                         (cons key '())])
                    (push-cdr!
                     cursor
                     new-cursor)
                    (al-toml-set! new-cursor (cdr path) v)))))))
  
  (define (valid-bare-key? str)
    (let ([valid-key #t])
      (string-for-each
       (lambda (c)
         (unless (char-bare? c)
           (set! valid-key #f))) str)
      valid-key))
  (define (path->str path)
    (define (path->str-in path new)
      (if (null? path)
          new
          (path->str-in
           (cdr path)
           (cons (s->k-str (car path))
                 (if (null? new)
                     '()
                     (cons "." new))))))
    (apply string-append (path->str-in path '())))
  (define (s->k-str s)
    (let ([str (cond
                [(string? s) s]
                [(symbol? s) (symbol->string s)]
                [else
                 (error s "Writer Error: invalid key")])])
      (if (valid-bare-key? str)
          str
          (format "~s" str))))
  (define (obj->v-str obj)
    (cond
     [(null? obj) "{ }"]
     [(string? obj) (format "~s" obj)]
     [(atom? obj)
      (case obj
        [+inf.0 "+inf"]
        [-inf.0 "-inf"]
        [+nan.0 "nan"]
        [#t "true"]
        [#f "false"]
        [else (number->string obj)])]
     [else
      (dt-l->dt-str obj)]))
  (define (al-toml-dump tree fp)
    (define (al-dump-subs res-al path)
      (cond
       [(null? res-al) 'end]
       [(integer? (caadar res-al))
        (al-dump-sub-array (car res-al) path)
        (al-dump-subs (cdr res-al) path)]
       [(string? (caadar res-al))
        (al-dump-sub-table (car res-al) path)
        (al-dump-subs (cdr res-al) path)]))
    (define (al-dump res-al path new-al)
      (cond
       [(null? res-al)
        (al-dump-subs (reverse new-al) path)]
       [(or (atom? (cdar res-al)) ;; including null table
            (symbol? (cadar res-al)))
        (al-dump-pair (car res-al))
        (newline fp)
        (al-dump (cdr res-al) path new-al)]
       [(let ([first-key (caadar res-al)])
          (or (integer? first-key)
              (string? first-key)))
        (al-dump (cdr res-al) path
                 (cons (car res-al) new-al))]
       [else (error res-al
                    "Writer Error: invalid assoc list")]))

    (define (al-dump-value v)
      (cond
       [(or (atom? v)
            (symbol? (car v)))
        (display (obj->v-str v) fp)]
       [(integer? (caar v))
        (al-dump-inline-array v)]
       [(string? (caar v))
        (al-dump-inline-table v)]
       [else (error v "Writer Error: invalid value")]))
    (define (al-dump-inline-array array)
      (display "[ " fp)
      (let loop ([res-array array])
        (unless (null? res-array)
          (al-dump-value (cdar res-array))
          (unless (null? (cdr res-array))
            (display ", " fp))
          (loop (cdr res-array))))
      (display " ]" fp))
    (define (al-dump-inline-table al)
      (display "{ " fp)
      (let loop ([res-al al])
        (unless (null? res-al)
          (al-dump-pair (car al))
          (unless (null? (cdr res-al))
            (display ", " fp)))
        (loop (cdr res-al)))
      (display " }" fp))
    (define (al-dump-pair pair)
      (let ([k (car pair)]
            [v (cdr pair)])
        (display (s->k-str k) fp)
        (display " = " fp)
        (al-dump-value v)))
    (define (al-dump-array-head path)
      (display (format "\n[[~a]]\n" (path->str path)) fp))
    (define (al-dump-table-head path)
      (display (format "\n[~a]\n" (path->str path)) fp))
    (define (al-dump-sub-array array path)
      (if (andmap (lambda (p)
                    (or (null? (cdr p))
                        (and (pair? (cdr p))
                             (not (symbol? (cadr p)))
                             (string? (caadr p)))))
                  (cdr array))
          (let ([new-path
                 (cons (car array) path)])
            (for-each
             (lambda (al)
               (al-dump-array-head new-path)
               (al-dump (cdr al) new-path '()))
             (cdr array)))
          (begin
            (al-dump-pair array)
            (newline fp))))

    (define (al-dump-sub-table al path)
      (let ([new-path
             (cons (car al) path)])
        (al-dump-table-head new-path)
        (al-dump (cdr al)
                 new-path '())))
    (al-dump (cdr tree) '() '()))

  (define (ht-toml-dump ht fp)
    (define (ht-dump res-al path new-al)
      (cond
       [(null? res-al)
        (ht-dump-subs (reverse new-al) path)]       
       [(or (hashtable? (cdar res-al))
            (vector? (cdar res-al)))
        (ht-dump (cdr res-al) path
                 (cons (car res-al) new-al))]
       [(or (atom? (cdar res-al))
            (symbol? (cadar res-al)))
        (ht-dump-pair (car res-al))
        (newline fp)
        (ht-dump (cdr res-al) path new-al)]
       [else (error res-al
                    "Writer Error: invalid hashtable")]))
    (define (ht-dump-subs res-al path)
      (cond
       [(null? res-al) 'end]
       [(vector? (cdar res-al))
        (ht-dump-sub-array (car res-al) path)
        (ht-dump-subs (cdr res-al) path)]
       [(hashtable? (cdar res-al))
        (ht-dump-sub-table (car res-al) path)
        (ht-dump-subs (cdr res-al) path)]))
    (define (ht-dump-value v)
      (cond
       [(vector? v)
        (ht-dump-inline-array (vector->list v))]
       [(hashtable? v)
        (ht-dump-inline-table
         (vector->list (hashtable-cells v)))]       
       [(or (atom? v)
            (symbol? (car v)))
        (display (obj->v-str v) fp)]
       [else (error v "Writer Error: invalid value")]))
    (define (ht-dump-inline-array array)
      (display "[ " fp)
      (let loop ([res-array array])
        (unless (null? res-array)
          (ht-dump-value (car res-array))
          (unless (null? (cdr res-array))
            (display ", " fp))
          (loop (cdr res-array))))
      (display " ]" fp))
    (define (ht-dump-inline-table al)
      (display "{ " fp)
      (let loop ([res-al al])
        (unless (null? res-al)
          (ht-dump-pair (car al))
          (unless (null? (cdr res-al))
            (display ", " fp)))
        (loop (cdr res-al)))
      (display " }" fp))
    (define (ht-dump-pair pair)
      (let ([k (car pair)]
            [v (cdr pair)])
        (display (s->k-str k) fp)
        (display " = " fp)
        (ht-dump-value v)))
    (define (ht-dump-array-head path)
      (display (format "\n[[~a]]\n" (path->str path)) fp))
    (define (ht-dump-table-head path)
      (display (format "\n[~a]\n" (path->str path)) fp))
    (define (ht-dump-sub-array arrayp path)
      (let ([inline #f])
        (vector-for-each
         (lambda (item)
           (unless (hashtable? item)
             (set! inline #t))) (cdr arrayp))
        (if inline
            (begin
              (ht-dump-pair arrayp)
              (newline fp))
            (let ([new-path
                   (cons (car arrayp) path)])
              (vector-for-each
               (lambda (sub-ht)
                 (ht-dump-array-head new-path)
                 (ht-dump (vector->list
                           (hashtable-cells sub-ht))
                          new-path '()))
               (cdr arrayp))))))
    (define (ht-dump-sub-table htp path)
      (let ([new-path
             (cons (car htp) path)])
        (ht-dump-table-head new-path)
        (ht-dump (vector->list
                  (hashtable-cells (cdr htp)))
                 new-path '())))
    (ht-dump (vector->list
              (hashtable-cells ht)) '() '())))
