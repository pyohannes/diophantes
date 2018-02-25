#lang racket/base

;; Converting symbolic algebraic expressions to LaTeX math strings.

(provide latex)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/string
         racket/match
         racket/list
         racket/bool
         "private/data.rkt"
         "private/util.rkt")

;; -----
;; latex
;; -----

(define-generic (latex e))

;; ---------
;; num-latex
;; ---------

(module+ test
  (require rackunit
           "parse.rkt")

  (check-equal? (latex (make-num 3))
                "3")
  (check-equal? (latex (make-num 0))
                "0")
  )

(define-instance ((latex num) n)
  (number->string (num-val n)))

;; ----------
;; frac-latex
;; ----------

(module+ test
  (check-equal? (latex (make-frac 3 4))
                "\\frac{3}{4}")
  (check-equal? (latex (make-frac -3 4))
                "-\\frac{3}{4}")
  (check-equal? (latex (make-frac 3 -4))
                "-\\frac{3}{4}")
  )

(define-instance ((latex frac) f)
  (define num (frac-num f))
  (define denom (frac-denom f))
  (define sign
    (if (xor (> 0 num)
             (> 0 denom))
        "-"
        ""))
  (format "~a\\frac{~a}{~a}" sign (abs num) (abs denom)))

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

;; ---------
;; add-latex
;; ---------

(module+ test
  (check-equal? (latex (make-add (make-num 3) (make-sym 'x)))
                "3 + x")
  (check-equal? (latex (make-add (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2 + x + y")
  (check-equal? (latex (make-add (make-num 3)
                                 (make-mul (make-num -1)
                                           (make-sym 'x))))
                "3 -x")
  (check-equal? (latex (make-add (make-num 3)
                                 (make-mul (make-frac -1 2)
                                           (make-sym 'x))))
                "3 -\\frac{1}{2}x")
  )

(define-instance ((latex add) a)
  (define addends
    (for/list ([addend (add-addends a)])
      (define l (latex addend))
      (cons (if (string-startswith l "-")
                " "
                " + ")
            l)))
  (apply string-append (cdr (flatten addends))))

;; ---------
;; mul-latex
;; ---------

(module+ test
  (check-equal? (latex (make-mul (make-num 3) (make-sym 'x)))
                "3x")
  (check-equal? (latex (make-mul (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2xy")
  (check-equal? (latex (parse-sexpr '(* (+ 2 x) (+ 3 y))))
                "(2 + x)(3 + y)")
  (check-equal? (latex (make-mul (make-num -1) (make-sym 'x)))
                "-x")
  )

(define-instance ((latex mul) m)
  (define (_ factors)
    (apply string-append
      (for/list ([factor factors])
        (parentize factor))))
  (define factors (mul-factors m))
  (if (equal? (car factors) (make-num -1) )
      (string-append "-" 
                     (_ (cdr factors)))
      (_ factors)))

;; -----------
;; power-latex
;; -----------

(module+ test
  (check-equal? (latex (make-power (make-num 3) (make-sym 'x)))
                "3^{x}")
  (check-equal? (latex (parse-sexpr '(expt (+ x 3) 4)))
                "(x + 3)^{4}")
  )

(define-instance ((latex power) p)
  (format "~a^{~a}" 
          (parentize (power-base p))
          (latex (power-exponent p))))

;; ----------
;; logn-latex
;; ----------

(module+ test
  (check-equal? (latex (make-logn (make-num 3) (make-sym 'x)))
                "\\log_{x} 3")
  (check-equal? (latex (make-logn (make-num 3) (make-num (exp 1))))
                "\\ln 3")
  )

(define-instance ((latex logn) l)
  (define s/n (latex (logn-n l)))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (format "\\ln ~a" s/n))
        (else
          (format "\\log_{~a} ~a" (latex base) s/n))))

;; -------------------
;; polynomial/si-latex
;; -------------------

(module+ test
  (check-equal? (latex (make-polynomial/si 'x '(0 1 2)))
                "2x^{2} + x")
  (check-equal? (latex (make-polynomial/si 'x '(3 3 3 3)))
                "3x^{3} + 3x^{2} + 3x + 3")
  (check-equal? (latex (make-polynomial/si 'y '(9 0 0 0 0 1)))
                "y^{5} + 9")
  (check-equal? (latex (make-polynomial/si 'y '()))
                "0")
  (check-equal? (latex (make-polynomial/si 'y '(4)))
                "4")
  (check-equal? (latex (make-polynomial/si 'y '(0 0 1)))
                "y^{2}")
  )

(define-instance ((latex polynomial/si) p)
  (define indet (symbol->string (polynomial/si-indet p)))
  (define parts
    (for/list ([c (polynomial/si-coeffs p)]
               [e (in-naturals)]
               #:when (> c 0))
      (string-append (if (> c 1)
                         (number->string c)
                         "")
                     (if (> e 0)
                         indet
                         "")
                     (if (> e 1)
                         (format "^{~a}" e)
                         ""))))
  (match (length parts)
    [0 "0"]
    [1 (car parts)]
    [_ (string-join (reverse parts) " + ")]))


; -----
; atom? 
; -----

(module+ test
  (check-true  (atom? (make-num 3)))
  (check-true  (atom? (make-sym 'x)))
  (check-true  (atom? (parse-sexpr '(expt x 3))))
  (check-false (atom? (parse-sexpr '(* x 3))))
  (check-false (atom? (parse-sexpr '(+ x 3))))
  (check-false (atom? (parse-sexpr '(ln 3))))
  )

(define (atom? e)
  (or (num? e)
      (sym? e)
      (frac? e)
      (power? e)))

; ---------
; parentize
; ---------

(module+ test
  (check-equal? (parentize (parse-sexpr '(+ x 3)))
                "(x + 3)")
  (check-equal? (parentize (make-num 3))
                "3")
  )

(define (parentize e)
  (if (atom? e)
      (latex e)
      (format "(~a)" (latex e))))
