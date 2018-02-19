#lang racket/base

;; Constant fractions.

(require racket/contract)

(provide
  (all-from-out "private/expr.rkt")
  (contract-out
    [make-frac               (-> integer? integer? frac?)]
    [frac?                   (-> any/c boolean?)]
    [frac-num                (-> frac? integer?)]
    [frac-denom              (-> frac? integer?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require "private/expr.rkt"
         "private/util.rkt"
         "num.rkt")

;; ---------
;; make-frac
;; ---------

(module+ test
  (require rackunit)

  (check-equal? (make-frac 3 4)
                (frac 3 4))
  )

(define (make-frac n d)
  (frac n d))

;; ----
;; frac
;; ----

(struct frac (num denom)
  #:transparent
  #:methods gen:algexpr [
  (define (evaluate f)
    (frac-evaluate f))
  (define (sexpr f)
    (frac-sexpr f))
  (define (latex f)
    (frac-latex f))
  (define (differentiate f s)
    (frac-differentiate f s))
  (define (zero? f)
    (frac-zero? f))
  (define (simplify f)
    (frac-simplify f))
  ])

;; ----------
;; frac-zero?
;; ----------

(module+ test
  (check-true  (frac-zero? (make-frac 0 3)))
  (check-false (frac-zero? (make-frac 1 3)))
  )

(define (frac-zero? f)
  (= (frac-num f) 0))

;; -------------
;; frac-evaluate
;; -------------

(module+ test
  (check-equal? ((evaluate (make-frac 3 3))
                 9)
                1)
  (check-equal? ((evaluate (make-frac 3 4))
                 3)
                (/ 3 4))
  )

(define (frac-evaluate f)
  (define (_ . rest)
    (/ (frac-num f)
       (frac-denom f)))
  _)

;; ----------
;; frac-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-frac 3 4))
                '(/ 3 4))
  )

(define (frac-sexpr f)
  (list '/
        (frac-num f)
        (frac-denom f)))

;; ----------
;; frac-latex
;; ----------

(module+ test
  (check-equal? (latex (make-frac 3 4))
                "\\frac{3}{4}")
  )

(define (frac-latex f)
  (format "\\frac{~a}{~a}" (frac-num f) (frac-denom f)))

;; ------------------
;; frac-differentiate
;; ------------------

(module+ test
  (check-equal? (differentiate (make-frac 3 4) 'x)
                (make-num 0))
  )

(define (frac-differentiate f s)
  (make-num 0))

;; -------------
;; frac-simplify
;; -------------

(module+ test

  ;; ASAE-2: u is a fraction in standard form.
  (check-equal? (simplify (make-frac 3 4))
                (make-frac 3 4))
  (check-equal? (simplify (make-frac 4 1))
                (make-num 4))
  (check-equal? (simplify (make-frac 8 4))
                (make-num 2))
  (check-equal? (simplify (make-frac 4 8))
                (make-frac 1 2))
  )

(define (frac-simplify f)
  (apply-simplify f
                  frac?
                  (list frac-simplify/standard
                        frac-simplify/num)))

(define (frac-simplify/standard f)
  (define num (frac-num f))
  (define denom (frac-denom f))
  (define g (gcd num denom))
  (if (> g 1)
      (make-frac (/ num g)
                 (/ denom g))
      f))

(define (frac-simplify/num f)
  (if (= (frac-denom f) 1)
      (make-num (frac-num f))
      f))
