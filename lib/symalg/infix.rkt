#lang racket/base

;; Converting symbolic algebraic expressions to infix expressions.

(provide infix)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/string
         "private/data.rkt")

;; -----
;; infix
;; -----

(define-generic (infix e))

;; ---------
;; num-infix
;; ---------

(module+ test
  (require rackunit)

  (check-equal? (infix (make-num 3))
                "3")
  (check-equal? (infix (make-num 0))
                "0")
  )

(define-instance ((infix num) n)
  (number->string (num-val n)))

;; ----------
;; frac-infix
;; ----------

(module+ test
  (check-equal? (infix (make-frac 3 4))
                "3/4")
  )

(define-instance ((infix frac) f)
  (format "~a/~a" (frac-num f) (frac-denom f)))

;; ---------
;; sym-infix
;; ---------

(module+ test
  (check-equal? (infix (make-sym 'x))
                "x")
  )

(define-instance ((infix sym) s)
  (symbol->string (sym-val s)))

;; ---------
;; add-infix
;; ---------

(module+ test
  (check-equal? (infix (make-add (make-num 3) (make-num 4)))
                "3 + 4")
  (check-equal? (infix (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                "3 + x + 4")
  )

(define-instance ((infix add) a)
  (string-join
    (map infix (add-addends a))
    " + "))

;; ---------
;; mul-infix
;; ---------

(module+ test
  (check-equal? (infix (make-mul (make-num 3) (make-num 4)))
                "3 * 4")
  (check-equal? (infix (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                "3 * x * 4")
  )

(define-instance ((infix mul) m)
  (string-join
    (map infix (mul-factors m))
    " * "))

;; -----------
;; power-infix
;; -----------

(module+ test
  (check-equal? (infix (make-power (make-num 3) (make-num 4)))
                "(3)^(4)")
  (check-equal? (infix (make-power (make-num 3) (make-sym 'x)))
                "(3)^(x)")
  )

(define-instance ((infix power) p)
  (format "(~a)^(~a)" (infix (power-base p))
                      (infix (power-exponent p))))

;; ----------
;; logn-infix
;; ----------

(module+ test
  (check-equal? (infix (make-logn (make-num 3) (make-num 4)))
                "logn(3, 4)")
  (check-equal? (infix (make-logn (make-num 3) (make-sym 'x)))
                "logn(3, x)")
  (check-equal? (infix (make-logn (make-num 3) (make-num (exp 1))))
                "ln(3)")
  )

(define-instance ((infix logn) l)
  (define s/n (infix (logn-n l)))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (format "ln(~a)" s/n))
        (else
          (format "logn(~a, ~a)" s/n (infix base)))))
