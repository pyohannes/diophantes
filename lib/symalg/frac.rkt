#lang racket/base

;; Constant fractions.

(provide
  (all-from-out "private/expr.rkt")
  make-frac frac? frac-num frac-denom
  )

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "private/expr.rkt"
         "private/util.rkt"
         "num.rkt")

;; ---------
;; make-frac
;; ---------

(module+ test
  (require rackunit)

  (check-equal? (make-frac 3 4)
                (frac 3 4))
  )

(define (make-frac n d)
  (frac n d))

;; ----
;; frac
;; ----

(struct frac (num denom))

;; ----------
;; frac-zero?
;; ----------

(module+ test
  (check-true  (zero? (make-frac 0 3)))
  (check-false (zero? (make-frac 1 3)))
  )

(define-instance ((zero? frac) f)
  (= (frac-num f) 0))

;; -------------
;; frac-evaluate
;; -------------

(module+ test
  (check-equal? ((evaluate (make-frac 3 3))
                 9)
                1)
  (check-equal? ((evaluate (make-frac 3 4))
                 3)
                (/ 3 4))
  )

(define-instance ((evaluate frac) f)
  (define (_ . rest)
    (/ (frac-num f)
       (frac-denom f)))
  _)

;; ----------
;; frac-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-frac 3 4))
                '(/ 3 4))
  )

(define-instance ((sexpr frac) f)
  (list '/
        (frac-num f)
        (frac-denom f)))

;; ----------
;; frac-latex
;; ----------

(module+ test
  (check-equal? (latex (make-frac 3 4))
                "\\frac{3}{4}")
  )

(define-instance ((latex frac) f)
  (format "\\frac{~a}{~a}" (frac-num f) (frac-denom f)))

;; ------------------
;; frac-differentiate
;; ------------------

(module+ test
  (check-equal? (differentiate (make-frac 3 4) 'x)
                (make-num 0))
  )

(define-instance ((differentiate frac) f s)
  (make-num 0))

;; -------------
;; frac-simplify
;; -------------

(module+ test

  ;; ASAE-2: u is a fraction in standard form.
  (check-equal? (simplify (make-frac 3 4))
                (make-frac 3 4))
  (check-equal? (simplify (make-frac 4 1))
                (make-num 4))
  (check-equal? (simplify (make-frac 8 4))
                (make-num 2))
  (check-equal? (simplify (make-frac 4 8))
                (make-frac 1 2))
  )

(define-instance ((simplify frac) f)
  (apply-simplify f
                  frac?
                  (list frac-simplify/standard
                        frac-simplify/num)))

(define (frac-simplify/standard f)
  (define num (frac-num f))
  (define denom (frac-denom f))
  (define g (gcd num denom))
  (if (> g 1)
      (make-frac (/ num g)
                 (/ denom g))
      f))

(define (frac-simplify/num f)
  (if (= (frac-denom f) 1)
      (make-num (frac-num f))
      f))
