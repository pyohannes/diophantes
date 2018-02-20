#lang racket/base

;; The interface (generic) for symbolic algebraic expressions.

(provide
  sexpr
  differentiate
  latex
  evaluate
  simplify
  zero?
  )

;; ---------------------------------
;; Import and implementation section

(require multimethod)

(define-generic (sexpr e))
(define-generic (differentiate e _))
(define-generic (latex e))
(define-generic (evaluate e))
(define-generic (simplify e))
(define-generic (zero? e))
