#lang racket/base

;; The interface (generic) for symbolic algebraic expressions.

(require racket/contract)

(provide
  zero?
  gen:algexpr
  (contract-out
    [algexpr?           (-> any/c boolean?)]
    [sexpr              (-> algexpr? any/c)]
    [differentiate      (-> algexpr? symbol? algexpr?)]
    [latex              (-> algexpr? string?)]
    [evaluate           (-> algexpr? (->* () #:rest (listof number?) number?))]
    [simplify           (-> algexpr? algexpr?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require racket/generic)

(define-generics algexpr
  (sexpr algexpr)
  (children algexpr)
  (differentiate algexpr s)
  (latex algexpr)
  (evaluate algexpr)
  (simplify algexpr)
  (zero? algexpr)
  #:fallbacks
  [(define (children algexpr) '())
   (define (simplify algexpr) algexpr)
   (define (zero? algexpr) #f)
  ])
