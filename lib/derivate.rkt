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
         "latex.rkt"
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
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(expt (+ x 1 ) 2)))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* 2 (+ x 1)))))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(* 2 (expt x 2))))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* 4 x))))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(* z (expt x 2))))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* 2 x z))))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(expt x y)))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* (expt x (+ y -1)) y))))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(expt (+ x 2) z)))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* (expt (+ x 2) (+ z -1)) z))))
  (check-equal? (dexpr-deriv/auto (sexpr->dexpr '(* a (expt x 2))))
                (dexpr-deriv (dexpr-sym 'x)
                             (sexpr->dexpr '(* 2 a x))))
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
  (check-equal? (dexpr-smallest-sym (sexpr->dexpr '(* a x z)))
                (dexpr-sym 'x))
  (check-equal? (dexpr-smallest-sym (sexpr->dexpr '(* a z)))
                (dexpr-sym 'a))
  )

(define (dexpr-smallest-sym dexpr)
  (define syms
    (for/list ([c (dexpr-flatten dexpr)]
               #:when (dexpr-sym? c))
      c))
  (if (member (dexpr-sym 'x) syms)
      (dexpr-sym 'x)
      (car (sort syms dexpr-<))))
