#lang racket/base

;; Numerical expressions.

(provide
  (all-from-out "private/expr.rkt")
  make-num num? num-val
  )
  
;; ---------------------------------
;; Import and implementation section

(require multimethod
         "private/expr.rkt")

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

(struct num (val))

;; ---------
;; num-zero?
;; ---------

(module+ test
  (check-true  (zero? (make-num 0)))
  (check-false (zero? (make-num 9)))
  )

(define-instance ((zero? num) n)
  (= (num-val n) 0))

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

(define-instance ((evaluate num) n)
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

(define-instance ((sexpr num) n)
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

(define-instance ((latex num) n)
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

(define-instance ((differentiate num) n s)
  (make-num 0))

;; ------------
;; num-simplify
;; ------------

(module+ test

  ;; ASAE-1: u is an integer.
  (check-equal? (simplify (make-num 3))
                (make-num 3))
  )

(define-instance ((simplify num) n)
  n)
