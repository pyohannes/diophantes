#lang racket/base

;; Basic data types representing d-expressions as well as parsing routines.

(require racket/contract
         racket/generic
         )

(provide
  sexpr->dexpr
  dexpr? dexpr->sexpr dexpr-children dexpr-negative?
  (struct-out dexpr-num)
  (struct-out dexpr-sym)
  (struct-out dexpr-add)
  (struct-out dexpr-mul)
  (struct-out dexpr-expt)
  (struct-out dexpr-log)
  )


;; ---------------------------------
;; Import and implementation section

(require racket/match
         racket/string
         racket/list
         racket/bool
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
  (check-equal? (sexpr->dexpr '(- a b))
                (dexpr-add (dexpr-sym 'a) 
                           (dexpr-mul (dexpr-num -1) (dexpr-sym 'b))))
  (check-equal? (sexpr->dexpr '(- a b c))
                (dexpr-add (dexpr-sym 'a) 
                           (dexpr-mul (dexpr-num -1) 
                                      (dexpr-add (dexpr-sym 'b)
                                                 (dexpr-sym 'c)))))
  (check-equal? (sexpr->dexpr '(- a))
                (dexpr-mul (dexpr-num -1) (dexpr-sym 'a)))
  (check-equal? (sexpr->dexpr '(ln x))
                (dexpr-log (dexpr-num (exp 1)) (dexpr-sym 'x)))
  (check-equal? (sexpr->dexpr '(/ x y))
                (dexpr-mul (dexpr-sym 'x)
                           (dexpr-expt (dexpr-sym 'y) (dexpr-num -1))))
  )

(define (sexpr->dexpr sexpr)
  (match sexpr
    [(list '- first second rest ..1)
     (sexpr->dexpr (list '- first (append (list '+ second) rest)))]
    [(list op first second rest ..1)
     (sexpr->dexpr (list op first (append (list op second) rest)))]
    [(list '- first)
     (sexpr->dexpr (list '* -1 first))]
    [(list '/ first second)
     (sexpr->dexpr `(* ,first (expt ,second -1)))]
    [(list 'ln first)
     (sexpr->dexpr (list 'logn (exp 1) first))]
    [(list op args ...)
     (apply (match op
              ['+ dexpr-add]
              ['* dexpr-mul]
              ['- dexpr-sub]
              ['expt dexpr-expt]
              ['logn dexpr-log])
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

  (check-equal? (dexpr-differentiate (dexpr-sym 'x) (dexpr-num 3))
                (dexpr-num 0))
  (check-true  (dexpr-negative? (dexpr-num -1)))
  (check-false (dexpr-negative? (dexpr-num 1)))
  )

(struct dexpr-num (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-num-val e))
   (define (dexpr-differentiate s dexpr)
     (dexpr-num 0))
   (define (dexpr-negative? dexpr)
     (< (dexpr-num-val dexpr) 0))
   ])

; ---------
; dexpr-sym
; ---------
 
(module+ test
  (for [(i '(a))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))
  (check-equal? (dexpr-children (dexpr-sym 'a)) '())

  (check-equal? (dexpr-differentiate (dexpr-sym 'x) (dexpr-sym 'x))
                (dexpr-num 1))
  (check-equal? (dexpr-differentiate (dexpr-sym 'x) (dexpr-sym 'y))
                (dexpr-num 0))
  (check-true  (dexpr-negative? (dexpr-mul (dexpr-num -1) (dexpr-sym 'a))))
  (check-false (dexpr-negative? (dexpr-sym 'a)))
  )

(struct dexpr-sym (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-sym-val e))
   (define (dexpr-differentiate s dexpr)
     (if (equal? s dexpr)
         (dexpr-num 1)
         (dexpr-num 0)))
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

  (check-equal? (dexpr-differentiate (dexpr-sym 'x) 
                                     (sexpr->dexpr '(+ x 3)))
                (sexpr->dexpr '(+ 1 0)))
  (check-false (dexpr-negative? (dexpr-add (dexpr-sym 'a) (dexpr-sym 'b))))
  )

(struct dexpr-add (add aug)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-differentiate@super dexpr-differentiate)
   (define (dexpr-children e)
     (list (dexpr-add-add e) (dexpr-add-aug e)))
   (define (dexpr->sexpr e)
     (cons '+
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-add? e))))
   (define (dexpr-differentiate s dexpr)
     (dexpr-add (dexpr-differentiate@super s (dexpr-add-add dexpr))
                (dexpr-differentiate@super s (dexpr-add-aug dexpr))))
   ])

; ---------
; dexpr-sub
; ---------
;
(module+ test
  (check-equal? (dexpr-sub (dexpr-num 3) (dexpr-num 2))
                (sexpr->dexpr '(+ 3 (* -1 2))))
  (check-equal? (dexpr-sub (dexpr-num 3) (dexpr-sym 'a))
                (sexpr->dexpr '(+ 3 (* -1 a))))
  )

(define (dexpr-sub str sth)
  (dexpr-add str
             (dexpr-mul (dexpr-num -1) 
                        sth)))

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

  (check-equal? (dexpr-differentiate (dexpr-sym 'x) 
                                     (sexpr->dexpr '(* a x)))
                (sexpr->dexpr '(+ (* 0 x)
                                  (* a 1))))
  (check-true  (dexpr-negative? (dexpr-mul (dexpr-num -1) (dexpr-sym 'a))))
  (check-false (dexpr-negative? (dexpr-expt (dexpr-num 2) (dexpr-sym 'a))))
  )

(struct dexpr-mul (mpr mpd)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-differentiate@super dexpr-differentiate)
   (define/generic dexpr-negative?@super dexpr-negative?)
   (define (dexpr-children e)
     (list (dexpr-mul-mpr e) (dexpr-mul-mpd e)))
   (define (dexpr->sexpr e)
     (cons '*
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-mul? e))))
   (define (dexpr-differentiate s dexpr)
     (define mpr (dexpr-mul-mpr dexpr))
     (define mpd (dexpr-mul-mpd dexpr))
     (define mpr/deriv (dexpr-differentiate@super s mpr))
     (define mpd/deriv (dexpr-differentiate@super s mpd))
     (dexpr-add (dexpr-mul mpr/deriv mpd)
                (dexpr-mul mpr mpd/deriv)))
   (define (dexpr-negative? dexpr)
     (xor (dexpr-negative?@super (dexpr-mul-mpr dexpr))
          (dexpr-negative?@super (dexpr-mul-mpd dexpr))))
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
  (check-true  (dexpr-negative? (dexpr-expt (dexpr-num -1) (dexpr-sym 'a))))
  (check-false (dexpr-negative? (dexpr-expt (dexpr-num 2) (dexpr-sym 'a))))
  )

(struct dexpr-expt (base power)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-negative?@super dexpr-negative?)
   (define/generic dexpr-differentiate@super dexpr-differentiate)
    (define (dexpr-children e)
     (list (dexpr-expt-base e) (dexpr-expt-power e)))
   (define (dexpr->sexpr e)
     (list 'expt (dexpr->sexpr@super (dexpr-expt-base e))
                 (dexpr->sexpr@super (dexpr-expt-power e))))
   (define (dexpr-differentiate s dexpr)
     (define base (dexpr-expt-base dexpr))
     (define power (dexpr-expt-power dexpr))
     (define base/d (dexpr-differentiate@super s base))
     (define power/d (dexpr-differentiate@super s power))
     (dexpr-mul dexpr
                (dexpr-add (dexpr-mul base/d
                                      (dexpr-mul power
                                                 (dexpr-expt base
                                                             (dexpr-num -1))))
                           (dexpr-mul power/d
                                      (dexpr-log (dexpr-num (exp 1))
                                                 base)))))
   (define (dexpr-negative? dexpr)
     (dexpr-negative?@super (dexpr-expt-base dexpr)))
   ])

; ---------
; dexpr-log
; ---------

(module+ test
  (check-equal? (dexpr-children (dexpr-log (dexpr-num (exp 1))
                                           (dexpr-sym 'x)))
                (list (dexpr-num (exp 1)) (dexpr-sym 'x)))
  (check-equal? (dexpr->sexpr (dexpr-log (dexpr-num (exp 1))
                                         (dexpr-sym 'x)))
                '(ln x))
  (check-equal? (dexpr-differentiate (dexpr-sym 'x) 
                                     (dexpr-log (dexpr-num (exp 1))
                                                (dexpr-sym 'x)))
                (dexpr-mul (dexpr-num 1)
                           (dexpr-expt (dexpr-sym 'x)
                                       (dexpr-num -1))))
  (check-true  (dexpr-negative? (dexpr-log (dexpr-sym 'a) (dexpr-num -2))))
  (check-false (dexpr-negative? (dexpr-log (dexpr-sym 'a) (dexpr-num 2))))
  )

(struct dexpr-log (base n)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-differentiate@super dexpr-differentiate)
   (define/generic dexpr-negative?@super dexpr-negative?)
   (define (dexpr-children e)
     (list (dexpr-log-base e) (dexpr-log-n e)))
   (define (dexpr->sexpr e)
     (define sexpr-n (dexpr->sexpr@super (dexpr-log-n e)))
     (define base (dexpr-log-base e))
     (cond ((equal? base (dexpr-num (exp 1)))
            (list 'ln sexpr-n))
           (else
             (list 'logn (dexpr->sexpr@super base) sexpr-n))))
   (define (dexpr-differentiate s dexpr)
     (define base (dexpr-log-base dexpr))
     (define n (dexpr-log-n dexpr))
     (cond ((equal? base (dexpr-num (exp 1)))
            (dexpr-mul (dexpr-differentiate@super s n)
                       (dexpr-expt n
                                   (dexpr-num -1))))
           (else
             (error "Not implemented"))))
   (define (dexpr-negative? dexpr)
     (dexpr-negative?@super (dexpr-log-n dexpr)))
   ])
