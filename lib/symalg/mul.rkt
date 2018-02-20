#lang racket/base

;; N-ary multiplication.

(provide
  (all-from-out "private/expr.rkt")
  make-mul
  )

;; ---------------------------------
;; Import and implementation section

(require racket/string
         racket/list
         multimethod
         "private/expr.rkt"
         "private/util.rkt"
         "num.rkt"
         "add.rkt")

;; --------
;; make-mul
;; --------

(module+ test
  (require rackunit)
  (require "sym.rkt")

  (check-equal? (make-mul (make-num 3) (make-num 4))
                (mul (list (make-num 3) (make-num 4))))
  )

(define (make-mul n1 n2 . rest)
  (mul (cons n1 (cons n2 rest))))

;; ---
;; mul
;; ---

(struct mul (factors))

;; ------------
;; mul-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-mul (make-num 3) (make-sym 'x)))
                 9)
                27)
  (check-equal? ((evaluate (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                 9)
                108)
  )

(define-instance ((evaluate mul) m)
  (define (_ . rest)
    (for/product ([factor (mul-factors m)])
      (apply (evaluate factor) rest)))
  _)

;; ---------
;; mul-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-mul (make-num 3) (make-num 4)))
                '(* 3 4))
  (check-equal? (sexpr (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                '(* 3 x 4))
  )

(define-instance ((sexpr mul) m)
  (cons '*
        (for/list ([factor (mul-factors m)])
          (sexpr factor))))

;; ---------
;; mul-latex
;; ---------

(module+ test
  (check-equal? (latex (make-mul (make-num 3) (make-sym 'x)))
                "3x")
  (check-equal? (latex (make-mul (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2xy")
  )

(define-instance ((latex mul) m)
  (apply string-append
    (for/list ([factor (mul-factors m)])
      (latex factor))))

;; -----------------
;; mul-differentiate
;; -----------------

(module+ test
  (check-equal? (differentiate (make-mul (make-sym 'x) (make-num 3)) 'x)
                (make-add (make-mul (make-num 0) (make-sym 'x))
                          (make-mul (make-num 3) (make-num 1))))
  )

(define-instance ((differentiate mul) m s)
  (define (differentiate/2 f1 f2)
    (define d/f1 (differentiate f1 s))
    (define d/f2 (differentiate f2 s))
    (make-add (make-mul d/f2 f1)
              (make-mul f2 d/f1)))
  (define (differentiate/n fs)
    (if (= (length fs) 1)
        (car fs)
        (differentiate/2 (car fs)
                         (differentiate/n (cdr fs)))))
  (differentiate/n (mul-factors m)))

;; ------------
;; mul-simplify
;; ------------
;
;(module+ test
;
;  ;; Resolve nested multiplications
;  (check-equal? (simplify (make-mul (make-num 3) 
;                                    (make-mul (make-sym 'x) (make-sym 'y))))
;                (make-mul (make-sym 'x) (make-sym 'y) (make-num 3)))
;  (check-equal? (simplify (make-mul (make-num 3) 
;                                    (make-mul (make-sym 'x) (make-num 4))))
;                (make-mul (make-sym 'x) (make-num 7)))
;
;  ;; Resolve multiplications of numbers
;  (check-equal? (simplify (make-mul (make-num 3) (make-num 4)))
;                (make-num 7))
;  (check-equal? (simplify (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
;                (make-mul (make-sym 'x) (make-num 7)))
;
;  ;; Remove zeros
;  (check-equal? (simplify (make-mul (make-sym 'x) (make-num 4) (make-num 0)))
;                (make-mul (make-sym 'x) (make-num 4)))
;
;  ;; Resolve unary multiplications
;  (check-equal? (simplify (make-mul (make-num 4) (make-num 0)))
;                (make-num 4))
;  )
;
;(define-instance ((simplify mul) m)
;  (apply-simplify m 
;                  mul?  
;                  (list mul-simplify/nested
;                        mul-simplify/nums
;                        mul-simplify/zero
;                        mul-simplify/unary)))
;
;(define (mul-simplify/nested m)
;  (mul
;    (flatten
;      (for/list ([factor (mul-factors m)])
;        (if (mul? factor)
;            (mul-factors factor)
;            factor)))))
;
;(define (mul-simplify/nums m)
;  (define factors (mul-factors m))
;  (define a/nums (filter num? factors))
;  (cond ((not (null? a/nums))
;         (define a/rest (filter (negate num?) factors))
;         (define a/nums/sum (for/product ([n a/nums]) (num-val n)))
;         (mul (append a/rest
;                      (list (make-num a/nums/sum)))))
;        (else
;          m)))
;
;(define (mul-simplify/zero m)
;  (mul (filter (negate zero?)
;               (mul-factors m))))
;
;(define (mul-simplify/unary m)
;  (define factors (mul-factors m))
;  (if (= 1 (length factors))
;      (car factors)
;      m))
