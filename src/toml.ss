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
        (set! i0 1)
        (when (char=? #\0 char)
          (set! i0 2)
          (case (string-ref str 1)
            [#\b (set! char-integer? char-bin?)]
            [#\o (set! char-integer? char-oct?)]
            [#\x (set! char-integer? char-hex?)]
            [else (set! i0 1)])))
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
  (if (char=? (string-ref str 0) #\0)
      (let* ([str-e (string-extract str #\_)]
             [str-n (substring str-e 2 (string-length str-e))])
        (case (string-ref str 1)
          [#\x (string->number str-n 16)]
          [#\o (string->number str-n 8)]
          [#\b (string->number str-n 2)]
          [else (string->number str-n)]))
      (string->number str-n)))


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

(char-literal? #\")
(take-literal (open-input-string "This = '32'"))

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

(define (tokenizer fp)
  (lambda ()
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
                  (error #f "Token Error: Unmatched token character" char))]))))


(define fp (open-input-file "./src/example.toml"))
(define tk (tokenizer fp))

(tk)


