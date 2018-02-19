#lang racket/base

;; N-ary additions.

(require racket/contract)

(provide
  (all-from-out "private/expr.rkt")
  (contract-out
    [make-add                (->* (algexpr? algexpr?) #:rest (listof algexpr?)
                                  add?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require racket/string
         racket/list
         "private/expr.rkt"
         "private/util.rkt"
         "num.rkt")

;; --------
;; make-add
;; --------

(module+ test
  (require rackunit)
  (require "sym.rkt")

  (check-equal? (make-add (make-num 3) (make-num 4))
                (add (list (make-num 3) (make-num 4))))
  )

(define (make-add n1 n2 . rest)
  (add (cons n1 (cons n2 rest))))

;; ---
;; add
;; ---

(struct add (addends)
  #:transparent
  #:methods gen:algexpr [
  (define (evaluate a)
    (add-evaluate a))
  (define (sexpr a)
    (add-sexpr a))
  (define (latex a)
    (add-latex a))
  (define (differentiate a s)
    (add-differentiate a s))
  (define (simplify a)
    (add-simplify a))
  ])

;; ------------
;; add-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-add (make-num 3) (make-sym 'x)))
                 9)
                12)
  (check-equal? ((evaluate (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                 9)
                16)
  )

(define (add-evaluate a)
  (define (_ . rest)
    (for/sum ([addend (add-addends a)])
      (apply (evaluate addend) rest)))
  _)

;; ---------
;; add-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-add (make-num 3) (make-num 4)))
                '(+ 3 4))
  (check-equal? (sexpr (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                '(+ 3 x 4))
  )

(define (add-sexpr a)
  (cons '+
        (for/list ([addend (add-addends a)])
          (sexpr addend))))

;; ---------
;; add-latex
;; ---------

(module+ test
  (check-equal? (latex (make-add (make-num 3) (make-sym 'x)))
                "3 + x")
  (check-equal? (latex (make-add (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2 + x + y")
  )

(define (add-latex a)
  (string-join 
    (for/list ([addend (add-addends a)])
      (latex addend))
    " + "))

;; -----------------
;; add-differentiate
;; -----------------

(module+ test
  (check-equal? (differentiate (make-add (make-sym 'x) (make-num 3)) 'x)
                (make-add (make-num 1) (make-num 0)))
  )

(define (add-differentiate a s)
  (add (for/list ([addend (add-addends a)])
         (differentiate addend s))))

;; ------------
;; add-simplify
;; ------------

(module+ test

  ;; Resolve nested additions
  (check-equal? (simplify (make-add (make-num 3) 
                                    (make-add (make-sym 'x) (make-sym 'y))))
                (make-add (make-sym 'x) (make-sym 'y) (make-num 3)))
  (check-equal? (simplify (make-add (make-num 3) 
                                    (make-add (make-sym 'x) (make-num 4))))
                (make-add (make-sym 'x) (make-num 7)))

  ;; Resolve additions of numbers
  (check-equal? (simplify (make-add (make-num 3) (make-num 4)))
                (make-num 7))
  (check-equal? (simplify (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                (make-add (make-sym 'x) (make-num 7)))

  ;; Remove zeros
  (check-equal? (simplify (make-add (make-sym 'x) (make-num 4) (make-num 0)))
                (make-add (make-sym 'x) (make-num 4)))

  ;; Resolve unary additions
  (check-equal? (simplify (make-add (make-num 4) (make-num 0)))
                (make-num 4))
  )

(define (add-simplify a)
  (for/fold ([a a])
            ([op (list add-simplify/nested
                       add-simplify/nums
                       add-simplify/zero
                       add-simplify/unary)])
            #:break (not (add? a))
    (op a)))

(define (add-simplify/nested a)
  (add
    (flatten
      (for/list ([addend (add-addends a)])
        (if (add? addend)
            (add-addends addend)
            addend)))))

(define (add-simplify/nums a)
  (define addends (add-addends a))
  (define a/nums (filter num? addends))
  (cond ((not (null? a/nums))
         (define a/rest (filter (negate num?) addends))
         (define a/nums/sum (for/sum ([n a/nums]) (num-val n)))
         (add (append a/rest
                      (list (make-num a/nums/sum)))))
        (else
          a)))

(define (add-simplify/zero a)
  (add (filter (negate num-zero?)
               (add-addends a))))

(define (add-simplify/unary a)
  (define addends (add-addends a))
  (if (= 1 (length addends))
      (car addends)
      a))