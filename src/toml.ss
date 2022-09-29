;; TOML
;; ====
;;
;; Tom's Obvious, Minimal Language.
;;
;; By Tom Preston-Werner.
;;
;; Latest tagged version:
;; [v1.0.0](https://github.com/toml-lang/toml)
;;
;;
;; Objectives
;; ----------
;; TOML aims to be a minimal configuration file format that's easy to read due to
;; obvious semantics. TOML is designed to map unambiguously to a hash table. TOML
;; should be easy to parse into data structures in a wide variety of languages.
;;


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

(define (string->time-list str)
  (let ([str-l (string-length str)]
        [h #f]
        [m #f]
        [s #f]
        [ns 0]
        [i-loc 8]
        [offset #f])
    ;; 
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
                                  (+ 1 i-loc) (+ 3 i-loc)))]
                 [o-m (string->number
                       (substring str
                                  (+ 4 i-loc) (+ 6 i-loc)))])
             (and o-h o-m
                  (string-for-each
                   (lambda (c)
                     (set! valid-time
                           (or (char-numeric? c)
                               (char=? c #\:))))
                   (substring str
                              (+ 1 i-loc) (+ 6 i-loc)))
                  (< o-h 24)
                  (< o-m 60)
                  (set! offset (* 60 (+ (* 24 o-h) o-m)))
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
             (begin
               (set! h (string->number (substring str 0 2)))
               (and h (< h 24)))
             (begin
               (set! m (string->number (substring str 3 5)))
               (and m (< m 60)))
             (begin
               (set! s (string->number (substring str 6 8)))
               (and s (< s 60)))
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
             

;; tokenizer

(define-syntax push!
  (syntax-rules ()
    [(_ rl item) (set! rl (cons item rl))]))

(define-syntax inc!
  (syntax-rules ()
    [(_ x) (set! x (+ x 1))]
    [(_ x a) (set! x (+ x a))]))

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
  (unless (or (char=? (get-char fp) #\newline)
              (char=? (get-char fp) #\newline))
    (error #f "Token Error: invalid newline"))
  (consume-spaces&newlines fp)
  'newline)

(define (take-comment fp)
  (do ([char (lookahead-char fp) (lookahead-char fp)])
      ((char-newline? char) (take-newline fp))
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
      [#!eof 'eof]
      [else (if (char-literal? char)
                (take-literal fp)
                (error #f "Token Error: Unmatched token character" char))])))

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
           keys)
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
                  (error #f "Token Error: Invalid Charset for key"))))))))


;; 等号左侧为 key
;; 双方括号内为 key
;; newline 后的单方括号内为 key
;; 等号右侧为 value
;; 等号右侧的方括号内为 value
;; 等号右侧换行后，不为 value
;; 等号右侧逗号后，不为 value

(define (tokenizer fp)
  (do ([token (take-token fp) (take-token fp)]
       [is-primary-key #t (if is-primary-key
                              (not (eq? token 'equal))
                              (and (null? br-stack)
                                   (eq? token 'newline)))]
       [is-inline-key #f (if is-inline-key
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

(define fp (open-input-file "./src/example.toml"))
(tokenizer fp)

(string->float "2.0")

