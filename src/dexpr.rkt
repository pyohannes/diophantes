#lang racket/base

;; Basic data types representing d-expressions as well as parsing routines.

(require racket/contract
         racket/generic
         )

(provide
  sexpr->dexpr
  dexpr? dexpr->sexpr dexpr-children dexpr-<
  (struct-out dexpr-num)
  (struct-out dexpr-sym)
  (struct-out dexpr-add)
  (struct-out dexpr-mul)
  (struct-out dexpr-expt)
  )


;; ---------------------------------
;; Import and implementation section

(require racket/match
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
  (check-true  (dexpr-< (dexpr-num 1) (dexpr-num 2)))
  (check-false (dexpr-< (dexpr-num 2) (dexpr-num 1)))
  (check-true  (dexpr-< (dexpr-num 1) (dexpr-sym 'a)))
  )

(struct dexpr-num (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-num-val e))
   (define (dexpr-< dexpr d)
     (if (dexpr-num? d)
         (< (dexpr-num-val dexpr) (dexpr-num-val d))
         #t))
   ])

; ---------
; dexpr-sym
; ---------
 
(module+ test
  (for [(i '(a))]
    (check-equal? (dexpr->sexpr (sexpr->dexpr i))
                  i))
  (check-equal? (dexpr-children (dexpr-sym 'a)) '())

  (check-true  (dexpr-< (dexpr-sym 'a) (dexpr-sym 'b)))
  (check-false (dexpr-< (dexpr-sym 'b) (dexpr-sym 'a)))
  )

(struct dexpr-sym (val) 
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->sexpr e)
     (dexpr-sym-val e))
   (define (dexpr-< dexpr d)
     (if (dexpr-sym? d)
         (symbol<? (dexpr-sym-val dexpr) (dexpr-sym-val d))
         #f))
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
  )

(struct dexpr-add (add aug)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define (dexpr-children e)
     (list (dexpr-add-add e) (dexpr-add-aug e)))
   (define (dexpr->sexpr e)
     (cons '+
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-add? e))))
   (define (dexpr-< dexpr d)
     #f)
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
  (check-true  (dexpr-< (sexpr->dexpr '(* 2 a))
                        (sexpr->dexpr 'b)))
  (check-false (dexpr-< (sexpr->dexpr 'b)
                        (sexpr->dexpr '(* 2 a))))
  (check-true  (dexpr-< (sexpr->dexpr '(* 2 a))
                        (sexpr->dexpr '(+ b 1))))
  (check-false (dexpr-< (sexpr->dexpr '(+ b 1))
                        (sexpr->dexpr '(* 2 a))))
  (check-true  (dexpr-< (sexpr->dexpr '(* 2 a y))
                        (sexpr->dexpr '(* 2 b x))))
  (check-false (dexpr-< (sexpr->dexpr '(* 2 b x))
                        (sexpr->dexpr '(* 2 a y))))
  (check-true  (dexpr-< (sexpr->dexpr 1)
                        (sexpr->dexpr '(* 2 a y))))
  (check-false (dexpr-< (sexpr->dexpr '(* 2 a y))
                        (sexpr->dexpr 1)))
  (check-true  (dexpr-< (sexpr->dexpr '(* 2 2))
                        (sexpr->dexpr 'a)))
  )

(struct dexpr-mul (mpr mpd)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-<@super dexpr-<)
   (define (dexpr-children e)
     (list (dexpr-mul-mpr e) (dexpr-mul-mpd e)))
   (define (dexpr->sexpr e)
     (cons '*
           (map dexpr->sexpr@super
                (dexpr-flatten/pred dexpr-mul? e))))
   (define (dexpr-< dexpr d)
     (define c1 (for/first ([c (dexpr-flatten dexpr)]
                            #:when (dexpr-sym? c))
                  c))
     (define c2 (for/first ([c (dexpr-flatten d)]
                            #:when (dexpr-sym? c))
                  c))
     (cond ((not c1) #t)
           ((not c2) #f)
           ((dexpr-add? d) #t)
           (else
             (dexpr-<@super c1 c2))))
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
  (check-true  (dexpr-< (sexpr->dexpr 2)
                        (sexpr->dexpr '(expt a 2))))
  (check-false (dexpr-< (sexpr->dexpr '(expt a 2))
                        (sexpr->dexpr 2)))
  (check-true  (dexpr-< (sexpr->dexpr '(expt a 2))
                        (sexpr->dexpr 'b)))
  (check-false (dexpr-< (sexpr->dexpr 'b)
                        (sexpr->dexpr '(expt a 2))))
  (check-true  (dexpr-< (sexpr->dexpr '(expt a 3))
                        (sexpr->dexpr '(expt b 2))))
  (check-false (dexpr-< (sexpr->dexpr '(expt b 2))
                        (sexpr->dexpr '(expt a 3))))
  )

(struct dexpr-expt (base power)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->sexpr@super dexpr->sexpr)
   (define/generic dexpr-<@super dexpr-<)
   (define (dexpr-children e)
     (list (dexpr-expt-base e) (dexpr-expt-power e)))
   (define (dexpr->sexpr e)
     (list 'expt (dexpr->sexpr@super (dexpr-expt-base e))
                 (dexpr->sexpr@super (dexpr-expt-power e))))
   (define (dexpr-< dexpr d)
     (cond ((dexpr-expt? d)
            (dexpr-<@super (dexpr-expt-power d)
                           (dexpr-expt-power dexpr)))
           (else
             (not (dexpr-num? d)))))
   ])
