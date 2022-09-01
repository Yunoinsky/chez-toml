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
  (and (char? c) (char=? c #\")))

(define (char-quote? c)
  (and (char? c) (char=? c #\')))

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
  (if (or (char=? (get-char fp) #\newline)
          (char=? (get-char fp) #\newline))
      'newline
      (error #f "Token Error: invalid newline")))

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
      [(#\tab #\space #\newline #\return) (consume-spaces&newlines fp) #f]
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
