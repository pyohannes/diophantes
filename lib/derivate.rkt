#lang racket/base

;; Derivatives of d-expressions

(require racket/contract)

(provide
  (contract-out
    [dexpr-deriv/auto            (-> dexpr? dexpr-deriv?)]
    [dexpr-deriv->latex          (-> dexpr-deriv? string?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require "dexpr.rkt"
         "util.rkt"
         "sort.rkt"
         "dexprgen.rkt"
         "simplify.rkt")

(struct dexpr-deriv (respect deriv)
  #:transparent)

; ----------------
; dexpr-deriv/auto
; ----------------

(module+ test
  (require rackunit)

  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(+ x 3)))
                (dexpr-deriv (dexpr-sym 'x)
                             (dexpr-num 1)))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(+ (expt x 3) (* 2 x))))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(+ (* 3 (expt x 2)) 2))))
  )

(define (dexpr-deriv/auto dexpr)
  (define respect (dexpr-smallest-sym dexpr))
  (dexpr-deriv respect
               (dexpr-simplify (dexpr-differentiate respect dexpr))))

; ------------------
; dexpr-deriv->latex
; ------------------

(module+ test
  (check-equal? (dexpr-deriv->latex
                  (dexpr-deriv/auto (sexpr->dexpr '(+ (expt x 3) (* 2 x)))))
                "\\frac{\\delta}{\\delta x} = 3x^{2} + 2")
  )

(define (dexpr-deriv->latex deriv)
  (string-append 
    "\\frac{\\delta}{\\delta "
    (dexpr->latex (dexpr-deriv-respect deriv))
    "} = "
    (dexpr->latex (dexpr-deriv-deriv deriv))))

; ------------------
; dexpr-smallest-sym
; ------------------

(module+ test
  (check-equal? (dexpr-smallest-sym (sexpr->dexpr '(* (+ x 3) (expt y 4) z)))
                (dexpr-sym 'x))
  )

(define (dexpr-smallest-sym dexpr)
  (car
    (sort
      (for/list ([c (dexpr-flatten dexpr)]
                 #:when (dexpr-sym? c))
        c)
      dexpr-<)))
