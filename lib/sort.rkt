#lang racket/base

;; Sorting d-expressions.

(provide dexpr-<)

;; ---------------------------------
;; Import and implementation section

(require "dexpr.rkt"
         "util.rkt"
         racket/match)

; -------
; dexpr-<
; -------

(module+ test
  (require rackunit)

  (define-syntax check-<
    (syntax-rules ()
      [(check-< e1 e2)
       (begin
         (check-true  (dexpr-< e1 e2))
         (check-false (dexpr-< e2 e1)))]))

  (check-< (dexpr-num 2) (dexpr-num 1))
  (check-< (dexpr-sym 'a) (dexpr-num 1))
  (check-< (dexpr-sym 'a) (dexpr-sym 'b))

  (check-< (sexpr->dexpr '(* 2 a))
           (sexpr->dexpr 'b))
  (check-< (sexpr->dexpr '(* 2 a))
           (sexpr->dexpr '(+ b 1)))
  (check-< (sexpr->dexpr '(* 2 a y))
           (sexpr->dexpr '(* 2 b x)))
  (check-< (sexpr->dexpr '(* 2 a y))
           (sexpr->dexpr 1))
  (check-< (sexpr->dexpr 'a)
           (sexpr->dexpr 4))
  (check-< (sexpr->dexpr '(expt a 2))
           (sexpr->dexpr 2))
  (check-< (sexpr->dexpr '(expt a 2))
           (sexpr->dexpr 'b))
  (check-< (sexpr->dexpr '(expt a 3))
           (sexpr->dexpr '(expt b 2)))
  (check-< (sexpr->dexpr '(ln a))
           (sexpr->dexpr '(ln b)))
  (check-< (sexpr->dexpr '(* a b c a b))
           (sexpr->dexpr '(expt d d)))
  (check-< (sexpr->dexpr '(* a b c a b))
           (sexpr->dexpr 'd))
  (check-< (sexpr->dexpr 'a)
           (sexpr->dexpr '(* b c b)))
  )

(define (dexpr-< d1 d2)
  (for/first ([e1 (dexpr-sort-value d1)]
              [e2 (dexpr-sort-value d2)]
              #:when (not (= e1 e2)))
    (< e1 e2)))

; ----------------
; dexpr-sort-value
; ----------------

(module+ test
  (check-equal? (dexpr-sort-value (sexpr->dexpr '(+ a 1)))
                '(4 5 97 6 -1))
  (check-equal? (dexpr-sort-value (sexpr->dexpr '(+ a (expt b 3))))
                '(4 5 97 3 5 98 6 -3))
  )

(define (dexpr-sort-value dexpr)
  (define base/l
    (match dexpr
      [(? dexpr-num? n)  (list 6 (* -1 (dexpr-num-val n)))]
      [(? dexpr-sym? s)  (list 5 (symbol->integer (dexpr-sym-val s)))]
      [(? dexpr-add? a)  (list 4)]
      [(? dexpr-mul? m)  (list 3)]
      [(? dexpr-expt? e) (list 3)]
      [(? dexpr-log? l)  (list 1)]))
  (define children/l
    (apply append 
           (map dexpr-sort-value (dexpr-children dexpr))))
  (append base/l children/l))

