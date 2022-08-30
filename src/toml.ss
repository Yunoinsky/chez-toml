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
;; Learn a lot from caolan's [chicken-scheme](https://github.com/caolan/chicken-toml) 
;;
;; Objectives
;; ----------
;; TOML aims to be a minimal configuration file format that's easy to read due to
;; obvious semantics. TOML is designed to map unambiguously to a hash table. TOML
;; should be easy to parse into data structures in a wide variety of languages.
;;






;; 行首
;; - 所有的空格都被跳过
;; 在字符串外
;; - # 进入注释
;; 可跨行值
;;
;; \return \newline 或 \newline


(define (char-space? c)
  (exists (lambda (c-i)
            (char=? c c-i))
          '(#\space
            #\tab)))

(define (char-newline? c)
  (exists (lambda (c-i)
            (char=? c c-i))
          '(#\newline
            #\return)))

(define (char-dquote? c)
  (char=? c #\"))

(define (char-quote? c)
  (char=? c #\'))



;; tokenizer


(define fp (open-input-file "src/example.toml"))

(define-syntax push!
  (syntax-rules ()
    [(_ rl item) (set! rl (cons item rl))]))

(define (rlist->string rlist)
  (list->string (reverse rlist)))

(define (consume-spaces fp)
  (do ([char (lookahead-char fp) (lookahead-char fp)])
      ((not (char-space? char)) fp)
    (get-char fp)))

(define (take-newline fp)
  (if (or (char=? (get-char fp) #\newline)
          (char=? (get-char fp) #\newline))
      'newline
      (error #f "token error: invalid newline")))

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
  (let ([dquote-n 0])
    (do ([char (lookahead-char fp) (lookahead-char fp)]
         [dquote-n 0 (+ dquote-n 1)])
        ((or (not (char-dquote? char))
             (= dquote-n 3))
         (cons 'string
               (case dquote-n
                 [1 (take-s-string fp)]
                 [2 ""]
                 [3 (take-m-string fp)])))
      (get-char fp))))

(define (take-lstring fp)
  (let ([quote-n 0])
    (do ([char (lookahead-char fp) (lookahead-char fp)]
         [quote-n 0 (+ quote-n 1)])
        ((or (not (char-quote?))
             ())))))

(define (take-s-string fp)
  (let ([char-list '()])
    (do ([char (lookahead-char fp) (lookahead-char fp)])
        ((char-dquote? char)
         (get-char fp)
         (rlist->string char-list))
      (case char
        [(#\newline #\return)
         (error #f "token error: newline in single-line string")]
        [(#\\)
         (push! char-list (take-escape fp))]
        [else
         (push! char-list char)
         (get-char fp)]))))


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
      [else
       (let ([apair (assv c escape-codes)])
         (if apair
             (cdr apair)
             (error #f "token error: unspecific escape")))])))

(define (take-unicode fp n)
  (unless (or (= n 4)
              (= n 8))
    (error #f "token error: wrong unicode number"))
  (integer->char (string->number (take-chars fp n) 16)))
  
      

(define (next-token fp)
  (let ([char-list '()]
        [token-exit #f]
        [token-type 'n)
    (do ([char (lookahead-char fp) (lookahead-char fp)])
        ((or token-exit (eof-object? char))
         (cons token-type
               (list->string (reverse char-list))))
      (get-char)



(let ([fp (open-input-file "src/example.toml")]
      [in-str #f]
      [in-mstr 0]
      [in-lstr #f]
      [in-mlstr 0]
      [in-comment #f]
      [in-escape #f]
      [escape-list '()]
      [token-list '()]
      [char-list '()])

  (define (add-char! c)
    (set! char-list (cons c char-list)))
  
  (define (add-token! vstr)
    (set! token-list
          (cons (cons vstr
                      (list->string (reverse char-list)))
                token-list))
    (set! char-list '()))
  
  (do ([char (get-char fp) (get-char fp)])
      ((eof-object? char) token-list)
    (cond
     [in-lstr (case char
                [(#\newline #\return) (error #f "token error in literal string" char char-list)]
                [#\'
                 (set! token-list (cons char-list token-list))
                 (set! char-list '())
                 (set! in-lstr #f)]
                [else (set! char-list (cons char char-list))])]
     [in-str (case char
               [(#\newline #\return) (error #f "token error in basic string" char char-list)]
               [#\\
                (set! in-escape #t)]
               [#\"
                (set! token-list (cons char-list token-list))
                (set! char-list '())
                (set! in-str #f)]
               [else (set! char-list (cons char char-list))])]
     [in-comment (case char
                   [#\newline (set! in-comment #f)]     
                   )))
    
    



    
