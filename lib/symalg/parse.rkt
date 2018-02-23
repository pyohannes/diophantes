#lang racket/base

;; Parsing s-expressions and math strings.

(provide parse-sexpr
         parse-infix)

;; ---------------------------------
;; Import and implementation section

(require racket/match
         parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "private/data.rkt")

;; -----------
;; parse-sexpr
;; -----------

(module+ test
  (require rackunit)

  (check-equal? (parse-sexpr 3)
                (make-num 3))
  (check-equal? (parse-sexpr 'x)
                (make-sym 'x))
  (check-equal? (parse-sexpr '(+ x 3))
                (make-add (make-sym 'x) (make-num 3)))
  (check-equal? (parse-sexpr '(+ x y 3))
                (make-add (make-sym 'x) (make-sym 'y) (make-num 3)))
  (check-equal? (parse-sexpr '(- x 3))
                (make-add (make-sym 'x) (make-mul (make-num -1) (make-num 3))))
  (check-equal? (parse-sexpr '(- x y 3))
                (make-add (make-sym 'x) 
                          (make-mul (make-num -1) (make-sym 'y))
                          (make-mul (make-num -1) (make-num 3))))
  (check-equal? (parse-sexpr '(* x 3))
                (make-mul (make-sym 'x) (make-num 3)))
  (check-equal? (parse-sexpr '(* x y 3))
                (make-mul (make-sym 'x) (make-sym 'y) (make-num 3)))
  (check-equal? (parse-sexpr '(/ x 3))
                (make-mul (make-sym 'x) 
                          (make-power (make-num 3) (make-num -1))))
  (check-equal? (parse-sexpr '(expt x y))
                (make-power (make-sym 'x) (make-sym 'y)))
  (check-equal? (parse-sexpr '(ln x))
                (make-logn (make-sym 'x) (make-num (exp 1))))
  (check-equal? (parse-sexpr '(logn x 2))
                (make-logn (make-sym 'x) (make-num 2)))
  (check-equal? (parse-sexpr '(+ (* 3 x) (* 4 y)))
                (make-add (make-mul (make-num 3) (make-sym 'x))
                          (make-mul (make-num 4) (make-sym 'y))))
  (check-exn 
    exn:fail?
    (lambda () (parse-sexpr #t)))
  )

(define (parse-sexpr s)
  (match s
    [(? number? n)
     (make-num n)]
    [(? symbol? s)
     (make-sym s)]
    [(list 'ln first)
     (parse-sexpr (list 'logn first (exp 1)))]
    [(list '/ first second)
     (parse-sexpr `(* ,first (expt ,second -1)))]
    [(list op args ...)
     (apply (match op
              ['+ make-add]
              ['- make-sub]
              ['* make-mul]
              ['expt make-power]
              ['logn make-logn])
            (map parse-sexpr args))]
    [s
      (error "cannot parse expression" s)]))

; --------
; make-sub
; --------

(define (make-sub first . rest)
  (apply make-add 
         (cons first
               (for/list ([r rest])
                 (make-mul (make-num -1) r)))))

;; -----------
;; parse-infix
;; -----------

(module+ test
  (require rackunit)

  (check-equal? (parse-infix "3")
                (make-num 3))
  (check-equal? (parse-infix "x")
                (make-sym 'x))
  (check-equal? (parse-infix "x + 3")
                (make-add (make-sym 'x) (make-num 3)))
  (check-equal? (parse-infix "x + y + 3")
                (make-add (make-add (make-sym 'x) (make-sym 'y)) 
                          (make-num 3)))
  (check-equal? (parse-infix "x - 3")
                (make-add (make-sym 'x) (make-mul (make-num -1) (make-num 3))))
  (check-equal? (parse-infix "x - y - 3")
                (make-add (make-add (make-sym 'x) 
                                    (make-mul (make-num -1) (make-sym 'y)))
                          (make-mul (make-num -1) (make-num 3))))
  (check-equal? (parse-infix "x * 3")
                (make-mul (make-sym 'x) (make-num 3)))
  (check-equal? (parse-infix "x * y * 3")
                (make-mul (make-mul (make-sym 'x) (make-sym 'y)) 
                          (make-num 3)))
  (check-equal? (parse-infix "x / 3")
                (make-mul (make-sym 'x) 
                          (make-power (make-num 3) (make-num -1))))
  (check-equal? (parse-infix "x^y")
                (make-power (make-sym 'x) (make-sym 'y)))
  (check-equal? (parse-infix "ln(x)")
                (make-logn (make-sym 'x) (make-num (exp 1))))
  (check-equal? (parse-infix "3 * x + 4 * y")
                (make-add (make-mul (make-num 3) (make-sym 'x))
                          (make-mul (make-num 4) (make-sym 'y))))
  (check-exn 
    exn:fail?
    (lambda () (parse-infix #t)))
  )

(define (parse-infix s)
  (define input (open-input-string s))
  (parse-sexpr
    (infix-parser
      (lambda ()
        (infix-lexer input)))))

(define infix-parser
  (parser
    (start exp)
    (end EOF)
    (error void)
    (tokens value-tokens op-tokens)
    (precs (left - +)
           (left * /)
           (right ^))
    (grammar
      (exp [(NUM) $1]
           [(SYM) $1]
           [(FUN LPAR exp RPAR) (list $1 $3)]
           [(LPAR exp RPAR) $2]
           [(exp + exp) (list '+ $1 $3)]
           [(exp - exp) (list '- $1 $3)]
           [(exp * exp) (list '* $1 $3)]
           [(exp / exp) (list '/ $1 $3)]
           [(exp ^ exp) (list 'expt $1 $3)]
           ))))

(define-tokens value-tokens
  (NUM SYM FUN))

(define-empty-tokens op-tokens
  (+ - * / ^ EOF LPAR RPAR))

(define infix-lexer
  (lexer 
    [(:or "ln")
     (token-FUN (string->symbol lexeme))]
    [(:: alphabetic (:? "_" (:+ (:or alphabetic numeric))))
     (token-SYM (string->symbol lexeme))]
    [(:: (:? #\-) (:+ (char-range #\0 #\9)))
     (token-NUM (string->number lexeme))]
    [(:or "+" "-" "*" "/" "^")
     (string->symbol lexeme)]
    ["("
     'LPAR]
    [")"
     'RPAR]
    [whitespace
     (infix-lexer input-port)]
    [(eof)
     'EOF]))