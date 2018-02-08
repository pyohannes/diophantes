#lang racket/base

;; Sorting d-expressions.

(require racket/contract)

(provide
  (contract-out
    [dexpr-<           (-> dexpr? dexpr? boolean?)]))

;; ---------------------------------
;; Import and implementation section

(require "dexpr.rkt"
         "util.rkt"
         racket/list
         racket/match)

; ---------------
; dexpr-value-num
; ---------------

(module+ test
  (require rackunit)

  (check-equal? (dexpr-value-num (dexpr-num 3))
                3)
  (check-equal? (dexpr-value-num (dexpr-sym 'a))
                0))

(define (dexpr-value-num n)
  (if (dexpr-num? n)
      (dexpr-num-val n)
      0))

; ---------------
; dexpr-value-sym
; ---------------

(module+ test
  (check-true (< (dexpr-value-sym (dexpr-sym 'b))
                 (dexpr-value-sym (dexpr-sym 'a))))
  (check-true (< (dexpr-value-sym (dexpr-sym 'a_2))
                 (dexpr-value-sym (dexpr-sym 'a_1))))
  )

(define (dexpr-value-sym expr)
  (define (symbol->integer s)
    (for/fold ([r 0])
              ([char (string->list (symbol->string s))])
      (+ r (char->integer char))))
  (define sym (for/first ([c (dexpr-flatten expr)]
                          #:when (dexpr-sym? c))
                c))
  (if sym
      (+ 1 (/ 1 (symbol->integer (dexpr-sym-val sym))))
      0))

; ----------------
; dexpr-value-expt
; ----------------

(module+ test
  (check-equal? (dexpr-value-expt (sexpr->dexpr '(expt a 3)))
                3)
  )

(define (dexpr-value-expt e)
  (define m (dexpr-max-exponent e))
  (if (dexpr-num? m)
      (dexpr-num-val m)
      (dexpr-value m)))

; -----------
; dexpr-value
; -----------
 
(define (dexpr-value d)
  (list (dexpr-value-expt d) 
        (dexpr-value-sym d) 
        (dexpr-value-num d)))

; -------
; dexpr-<
; -------

(module+ test
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
           (sexpr->dexpr '(* 2 2)))
  (check-< (sexpr->dexpr '(expt a 2))
           (sexpr->dexpr 2))
  (check-< (sexpr->dexpr '(expt a 2))
           (sexpr->dexpr 'b))
  (check-< (sexpr->dexpr '(expt a 3))
           (sexpr->dexpr '(expt b 2)))
  (check-< (sexpr->dexpr '(expt d d))
           (sexpr->dexpr '(* a b c a b)))
  )

(define (dexpr-< d1 d2)
  (define (dexpr-list->number l base)
    (define b (max base 2))
    (for/fold ([n 0])
              ([l l])
      (* (+ n l) b)))
  (define v1 (flatten (dexpr-value d1)))
  (define v2 (flatten (dexpr-value d2)))
  (define base (apply max (append v1 v2)))
  (> (dexpr-list->number v1 base)
     (dexpr-list->number v2 base)))

; ------------------
; dexpr-max-exponent
; ------------------

(module+ test
  (check-equal? (dexpr-max-exponent 
                  (sexpr->dexpr '(* (expt a 3) (expt b 4) 3)))
                (dexpr-num 4))
  (check-equal? (dexpr-max-exponent 
                  (sexpr->dexpr '(+ a b 3)))
                (dexpr-num 1))
  )

(define (dexpr-max-exponent e)
  (cond ((dexpr-expt? e)
         (dexpr-expt-power e))
        (else
          (define exps (cons (dexpr-num 1) 
                             (map dexpr-max-exponent (dexpr-children e))))
          (car (sort exps dexpr-<)))))
