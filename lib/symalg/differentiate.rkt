#lang racket/base

;; Differentiating symbolic algebraic expressions.

(provide differentiate)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "private/data.rkt")

;; -------------
;; differentiate
;; -------------

(define-generic (differentiate e))

;; -----------------
;; num-differentiate
;; -----------------

(module+ test
  (require rackunit
           "parse.rkt")

  (check-equal? (differentiate (make-num 3) 'x)
                (make-num 0))
  (check-equal? (differentiate (make-num 0) 'y)
                (make-num 0))
  )

(define-instance ((differentiate num) n s)
  (make-num 0))

;; ------------------
;; frac-differentiate
;; ------------------

(module+ test
  (check-equal? (differentiate (make-frac 3 4) 'x)
                (make-num 0))
  )

(define-instance ((differentiate frac) f s)
  (make-num 0))

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

;; -----------------
;; add-differentiate
;; -----------------

(module+ test
  (check-equal? (differentiate (make-add (make-sym 'x) (make-num 3)) 'x)
                (make-add (make-num 1) (make-num 0)))
  )

(define-instance ((differentiate add) a s)
  (add (for/list ([addend (add-addends a)])
         (differentiate addend s))))

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

;; -------------------
;; power-differentiate
;; -------------------

(module+ test
  (check-equal? (differentiate (parse-sexpr '(expt x 3)) 'x)
                (parse-sexpr '(* (expt x 3)
                                 (+ (* 1 3 (expt x -1))
                                    (* 0 (ln x))))))
  )

(define-instance ((differentiate power) p s)
  (define base (power-base p))
  (define d/base (differentiate base s))
  (define exponent (power-exponent p))
  (define d/exponent (differentiate exponent s))
  (make-mul p 
            (make-add (make-mul d/base
                                exponent
                                (make-power base (make-num -1)))
                      (make-mul d/exponent
                                (make-logn base 
                                           (make-num (exp 1)))))))

;; ------------------
;; logn-differentiate
;; ------------------

(module+ test
  (check-equal? (differentiate (make-logn (make-sym 'x) (make-num (exp 1))) 'x)
                (make-mul (make-num 1)
                          (make-power (make-sym 'x) (make-num -1))))
  )

(define-instance ((differentiate logn) l s)
  (define n (logn-n l))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (make-mul (differentiate n s)
                   (make-power n (make-num -1))))
        (else
          (error "Not implemented"))))

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
