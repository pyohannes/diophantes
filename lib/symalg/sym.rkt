#lang racket/base

;; Expressions representing a variable.

(require racket/contract)

(provide
  (all-from-out "private/expr.rkt")
  make-sym
  )

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "private/expr.rkt"
         "num.rkt")

;; --------
;; make-sym
;; --------

(module+ test
  (require rackunit)

  (check-equal? (make-sym 'x)
                (sym 'x))
  (check-exn
    exn:fail?
    (lambda () (make-sym 'abc)))
  (check-exn
    exn:fail?
    (lambda () (make-sym 1)))
  )

(define (make-sym s)
  (if (regexp-match #rx"^[a-zA-Z](_[a-zA-Z0-9]+)*$" (symbol->string s))
      (sym s)
      (error "Not a valid variable identifier: " s)))

;; ---
;; sym
;; ---

(struct sym (val))

;; ------------
;; sym-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-sym 'x))
                 9)
                9)
  (check-equal? ((evaluate (make-sym 'y))
                 3)
                3)
  )

(define-instance ((evaluate sym) n)
  (define (_ . rest)
    (car rest))
  _)

;; ---------
;; sym-zero?
;; ---------

(module+ test
  (check-false (zero? (make-sym 'x)))
  )

(define-instance ((zero? sym) s)
  #f)

;; ---------
;; sym-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-sym 'x))
                'x)
  )

(define-instance ((sexpr sym) s)
  (sym-val s))

;; ---------
;; sym-latex
;; ---------

(module+ test
  (check-equal? (latex (make-sym 'x))
                "x")
  (check-equal? (latex (make-sym 'y_1))
                "y_1")
  )

(define-instance ((latex sym) s)
  (symbol->string (sym-val s)))

;; -----------------
;; sym-differentiate
;; -----------------

(module+ test
  (check-equal? (differentiate (make-sym 'x) 'x)
                (make-num 1))
  (check-equal? (differentiate (make-sym 'x) 'y)
                (make-sym 'x))
  )

(define-instance ((differentiate sym) sm s)
  (if (equal? (sym-val sm) s)
      (make-num 1)
      sm))

;; ------------
;; sym-simplify
;; ------------

(module+ test

  ;; ASAE-3: u is a symbol.
  (check-equal? (simplify (make-sym 'x))
                (make-sym 'x))
  )

(define-instance ((simplify sym) s)
  s)
