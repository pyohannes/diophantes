#lang racket/base

;; The interface (generic) for d-expressions.

(require racket/contract)

(provide
  gen:dexpr
  (contract-out
    [dexpr?              (-> any/c boolean?)]
    [dexpr->sexpr        (-> dexpr? any/c)]
    [dexpr-children      (-> dexpr? (listof dexpr?))]
    [dexpr-differentiate (-> dexpr? dexpr? dexpr?)]
    [dexpr-negative?     (-> dexpr? boolean?)]
    ))

;; ---------------------------------
;; Import and implementation section

(require racket/generic)

(define-generics dexpr
  (dexpr->sexpr dexpr)
  (dexpr-children dexpr)
  (dexpr-differentiate s dexpr)
  (dexpr-negative? dexpr)
  #:fallbacks
  [(define (dexpr-children dexpr) '())
   (define (dexpr-negative? dexpr) #f)
  ])
