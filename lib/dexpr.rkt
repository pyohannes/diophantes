#lang racket/base

;; Basic data types representing d-expressions as well as parsing routines.

(require racket/contract
         racket/generic
         )

(provide
  sexpr->dexpr
  dexpr? dexpr->sexpr dexpr-children dexpr->latex
  (struct-out dexpr-num)
  (struct-out dexpr-sym)
  (struct-out dexpr-add)
  (struct-out dexpr-mul)
  (struct-out dexpr-expt)
  )


;; ---------------------------------
;; Import and implementation section

(require racket/match
         racket/string
         racket/list
         "dexprgen.rkt"
         "util.rkt")

(module+ test
  (require "testutil.rkt")
  )


; ------------
; sexpr->dexpr
; ------------

(module+ test
  (check-equal? (sexpr->dexpr 2)
                (dexpr-num 2))
  (check-equal? (sexpr->dexpr 'a)
                (dexpr-sym 'a))
  (check-equal? (sexpr->dexpr '(+ a 1))
                (dexpr-add (dexpr-sym 'a) (dexpr-num 1)))
  (check-equal? (sexpr->dexpr '(* a 1))
                (dexpr-mul (dexpr-sym 'a) (dexpr-num 1)))
  (check-equal? (sexpr->dexpr '(+ a b 1))
                (dexpr-add (dexpr-sym 'a) 
                           (dexpr-add (dexpr-sym 'b) (dexpr-num 1))))
  (check-equal? (sexpr->dexpr '(expt a 2))
                (dexpr-expt (dexpr-sym 'a) (dexpr-num 2)))
  (check-exn 
    exn:fail?
    (lambda () (sexpr->dexpr #t)))
  )

(define (sexpr->dexpr sexpr)
  (match sexpr
    [(list op first second rest ..1)
     (sexpr->dexpr (list op first (append (list op second) rest)))]
    [(list op args ...)
     (apply (match op
              ['+ dexpr-add]
              ['* dexpr-mul]
              ['expt dexpr-expt])
            (map sexpr->dexpr args))]
    [(? number? n) 
     (dexpr-num n)]
    [(? symbol? s) 
     (dexpr-sym s)]
    [s
     (error "Error parsing expression" s)]))

; ---------
; dexpr-num
; ---------

(module+ test
  (for [(i '(2))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))
  (check-equal? (dexpr-children (dexpr-num 3)) '())

  (check-equal? (dexpr->latex (dexpr-num 3))
                "3")
  )

(struct dexpr-num (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-num-val e))
   (define (dexpr->latex dexpr)
     (number->string (dexpr-num-val dexpr)))
   ])

; ---------
; dexpr-sym
; ---------
 
(module+ test
  (for [(i '(a))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))
  (check-equal? (dexpr-children (dexpr-sym 'a)) '())

  (check-equal? (dexpr->latex (dexpr-sym 'a))
                "a")
  )

(struct dexpr-sym (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-sym-val e))
   (define (dexpr->latex dexpr)
     (symbol->string (dexpr-sym-val dexpr)))
   ])

; ---------
; dexpr-add
; ---------
 
(module+ test
  (check-equal? (dexpr-children (dexpr-add (dexpr-num 1) (dexpr-num 2)))
                (list (dexpr-num 1) (dexpr-num 2)))
  (for [(i '((+ a 1) 
             (+ a b 1)
             (+ a 1 a 1)))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))

  (check-equal? (dexpr->latex (sexpr->dexpr '(+ a 3)))
                "a + 3")
  (check-equal? (dexpr->latex (sexpr->dexpr '(+ a b c 3)))
                "a + b + c + 3")
  )

(struct dexpr-add (add aug)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr->latex@super dexpr->latex)
   (define (dexpr-children e)
     (list (dexpr-add-add e) (dexpr-add-aug e)))
   (define (dexpr->sexpr e)
     (cons '+
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-add? e))))
   (define (dexpr->latex dexpr)
     (string-join 
       (map dexpr->latex@super (dexpr-flatten/pred dexpr-add? dexpr))
       " + "))
   ])

; ---------
; dexpr-mul
; ---------

(module+ test
  (check-equal? (dexpr-children (dexpr-mul (dexpr-num 1) (dexpr-num 2)))
                (list (dexpr-num 1) (dexpr-num 2)))
  (for [(i '((* a 1) 
             (* a b 1)
             (+ a 2 (* b 4) c 9)))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))

  (check-equal? (dexpr->latex (sexpr->dexpr '(* 3 a)))
                "3a")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* 3 a b c)))
                "3abc")
  )

(struct dexpr-mul (mpr mpd)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr->latex@super dexpr->latex)
   (define (dexpr-children e)
     (list (dexpr-mul-mpr e) (dexpr-mul-mpd e)))
   (define (dexpr->sexpr e)
     (cons '*
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-mul? e))))
   (define (dexpr->latex dexpr)
     (string-join (map dexpr->latex@super 
                       (dexpr-flatten/pred dexpr-mul? dexpr))
                  ""))
   ])

; ----------
; dexpr-expt
; ----------

(module+ test
  (check-equal? (dexpr-children (sexpr->dexpr '(expt a 2)))
                (list (dexpr-sym 'a) (dexpr-num 2)))
  (for [(i '((expt a 2)
             (expt 2 (+ a b))))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))

  (check-equal? (dexpr->latex (sexpr->dexpr '(expt a 3)))
                "a^{3}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt (+ x 1) (+ x 1))))
                "(x + 1)^{x + 1}")
  )

(struct dexpr-expt (base power)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr->latex@super dexpr->latex)
   (define (dexpr-children e)
     (list (dexpr-expt-base e) (dexpr-expt-power e)))
   (define (dexpr->sexpr e)
     (list 'expt (dexpr->sexpr@super (dexpr-expt-base e))
                 (dexpr->sexpr@super (dexpr-expt-power e))))
   (define (dexpr->latex dexpr)
     (string-append (dexpr->latex/paren (dexpr-expt-base dexpr))
                    "^{"
                    (dexpr->latex@super (dexpr-expt-power dexpr))
                    "}"))
   ])

