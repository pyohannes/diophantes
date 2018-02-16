#lang racket/base

;; Numerical expressions.

(require racket/contract)

(provide
  (all-from-out "private/expr.rkt")
  (contract-out
    [make-num                (-> integer?  num?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require "private/expr.rkt")

;; --------
;; make-num
;; --------

(module+ test
  (require rackunit)

  (check-equal? (make-num 3)
                (num 3))
  )

(define (make-num n)
  (num n))

;; ---
;; num
;; ---

(struct num (val)
  #:transparent
  #:methods gen:algexpr [
  (define (evaluate n)
    (num-evaluate n))
  (define (sexpr n)
    (num-sexpr n))
  (define (latex n)
    (num-latex n))
  (define (differentiate n s)
    (num-differentiate n s))
  ])

;; ------------
;; num-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-num 3))
                 9)
                3)
  (check-equal? ((evaluate (make-num 0))
                 3)
                0)
  )

(define (num-evaluate n)
  (define (_ . rest)
    (num-val n))
  _)

;; ---------
;; num-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-num 3))
                '3)
  (check-equal? (sexpr (make-num 0))
                '0)
  )

(define (num-sexpr n)
  (num-val n))

;; ---------
;; num-latex
;; ---------

(module+ test
  (check-equal? (latex (make-num 3))
                "3")
  (check-equal? (latex (make-num 0))
                "0")
  )

(define (num-latex n)
  (number->string (num-val n)))

;; -----------------
;; num-differentiate
;; -----------------

(module+ test
  (check-equal? (differentiate (make-num 3) 'x)
                (make-num 0))
  (check-equal? (differentiate (make-num 0) 'y)
                (make-num 0))
  )

(define (num-differentiate p s)
  (make-num 0))
