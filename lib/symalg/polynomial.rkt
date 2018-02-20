#lang racket/base

;; Polynomials with a single indeterminante.

(provide
  (all-from-out "private/expr.rkt")
  make-polynomial/si
  )

;; ---------------------------------
;; Import and implementation section

(require racket/match
         racket/string
         multimethod
         "private/expr.rkt"
         "private/util.rkt")

;; ------------------
;; make-polynomial/si
;; ------------------

(module+ test
  (require rackunit)

  (check-equal? (make-polynomial/si 'x '(9 0 1))
                (polynomial/si 'x '(9 0 1)))
  (check-equal? (make-polynomial/si 'x '(9 0 1 0))
                (polynomial/si 'x '(9 0 1)))
  )

(define (make-polynomial/si indet coeffs)
  (polynomial/si indet 
                 (list-trim-right coeffs 0)))

;; -------------
;; polynomial/si
;; -------------

(struct polynomial/si (indet coeffs))

;; --------------------
;; polynomial/si-degree
;; --------------------

(module+ test
  (check-equal? (polynomial/si-degree (make-polynomial/si 'x '(0 1 2)))
                2)
  (check-equal? (polynomial/si-degree (make-polynomial/si 'x '(0)))
                0)
  (check-equal? (polynomial/si-degree (make-polynomial/si 'x '()))
                0)
  )

(define (polynomial/si-degree p)
  (max 0 
       (- (length (polynomial/si-coeffs p))
          1)))

;; ----------------------
;; polynomial/si-evaluate
;; ----------------------

(module+ test
  (check-equal? ((evaluate (make-polynomial/si 'x '(0 1 2)))
                 9)
                171)
  (check-equal? ((evaluate (make-polynomial/si 'x '(3 3 3 3)))
                 3)
                120)
  (check-equal? ((evaluate (make-polynomial/si 'x '()))
                 3)
                0)
  )

(define-instance ((evaluate polynomial/si) p)
  (define (_ . rest)
    (define x (car rest))
    (for/sum ([c (polynomial/si-coeffs p)]
              [e (in-naturals)])
      (* c (expt x e))))
  _)

;; -------------------
;; polynomial/si-sexpr
;; -------------------

(module+ test
  (check-equal? (sexpr (make-polynomial/si 'x '(0 1 2)))
                '(+ (* 2 (expt x 2)) x))
  (check-equal? (sexpr (make-polynomial/si 'x '(3 3 3 3)))
                '(+ (* 3 (expt x 3)) (* 3 (expt x 2)) (* 3 x) 3))
  (check-equal? (sexpr (make-polynomial/si 'y '(9 0 0 0 0 1)))
                '(+ (expt y 5) 9))
  (check-equal? (sexpr (make-polynomial/si 'y '()))
                0)
  (check-equal? (sexpr (make-polynomial/si 'y '(4)))
                4)
  (check-equal? (sexpr (make-polynomial/si 'y '(0 0 1)))
                '(expt y 2))
  )

(define-instance ((sexpr polynomial/si) p)
  (define indet (polynomial/si-indet p))
  (define parts
    (for/list ([c (polynomial/si-coeffs p)]
               [e (in-naturals)]
               #:when (> c 0))
      (cond ((and (= c 1) (> e 1))
             (list 'expt indet e))
            ((and (= c 1) (= e 1))
             indet)
            ((= e 1)
             (list '* c indet))
            ((= e 0)
             c)
            (else
              `(* ,c (expt ,indet ,e))))))
  (match (length parts)
    [0 0]
    [1 (car parts)]
    [_ (cons '+ (reverse parts))]))

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

;; ---------------------------
;; polynomial/si-differentiate
;; ---------------------------

(module+ test
  (check-equal? (differentiate (make-polynomial/si 'x '(0 1 2)) 'x)
                (make-polynomial/si 'x '(1 4)))
  (check-equal? (differentiate (make-polynomial/si 'x '(3 3 3 3)) 'x)
                (make-polynomial/si 'x '(3 6 9)))
  (check-equal? (differentiate (make-polynomial/si 'y '(9 0 0 0 0 1)) 'y)
                (make-polynomial/si 'y '(0 0 0 0 5)))
  (check-equal? (differentiate (make-polynomial/si 'y '()) 'y)
                (make-polynomial/si 'y '()))
  (check-equal? (differentiate (make-polynomial/si 'y '(4)) 'y)
                (make-polynomial/si 'y '()))
  (check-equal? (differentiate (make-polynomial/si 'y '(0 0 1)) 'y)
                (make-polynomial/si 'y '(0 2)))
  (check-equal? (differentiate (make-polynomial/si 'y '(0 0 1)) 'x)
                (make-polynomial/si 'y '(0 0 1)))
  )

(define-instance ((differentiate polynomial/si) p s)
  (define indet (polynomial/si-indet p))
  (define coeffs (polynomial/si-coeffs p))
  (cond ((and (> (length coeffs) 0)
              (equal? s indet))
         (polynomial/si 
           indet 
           (for/list ([c (cdr coeffs)]
                      [e (in-naturals 1)])
             (* c e))))
        (else
          p)))

;; ----------------------
;; polynomial/si-simplify
;; ----------------------

(module+ test
  (check-equal? (simplify (make-polynomial/si 'x '(0 1 2)))
                (make-polynomial/si 'x '(0 1 2)))
  )

(define-instance ((simplify polynomial/si) p)
  p)
