#lang racket/base

;; Simplifying symbolic algebraic expressions.

(provide simplify)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/list
         "private/data.rkt"
         "private/zero-expr.rkt"
         "private/util.rkt")
         

;; --------
;; simplify
;; --------

(define-generic (simplify e))

;; ------------
;; num-simplify
;; ------------

(module+ test
  (require rackunit)

  ;; ASAE-1: u is an integer.
  (check-equal? (simplify (make-num 3))
                (make-num 3))
  )

(define-instance ((simplify num) n)
  n)

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

(define-instance ((simplify add) a)
  (apply-simplify a 
                  add?  
                  (list add-simplify/nested
                        add-simplify/nums
                        add-simplify/zero
                        add-simplify/unary)))

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
  (add (filter (negate zero-expr?)
               (add-addends a))))

(define (add-simplify/unary a)
  (define addends (add-addends a))
  (if (= 1 (length addends))
      (car addends)
      a))

;; ----------------------
;; polynomial/si-simplify
;; ----------------------

(module+ test
  (check-equal? (simplify (make-polynomial/si 'x '(0 1 2)))
                (make-polynomial/si 'x '(0 1 2)))
  )

(define-instance ((simplify polynomial/si) p)
  p)

; --------------
; apply-simplify
; --------------

(module+ test
  (check-equal? (apply-simplify
                  3
                  number?
                  (list number->string symbol->string))
                "3"))


(define (apply-simplify expr pred? ops)
  (for/fold ([a expr])
            ([op ops])
            #:break (not (pred? a))
    (op a)))
