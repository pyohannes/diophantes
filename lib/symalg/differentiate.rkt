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

(module+ test
  (require rackunit
           "parse.rkt")

  (check-equal? (differentiate (make-sym 'x))
                (make-num 1))
  (check-equal? (differentiate (make-sym 'y))
                (make-sym 'y))
  )

(define (differentiate e [s 'x])
  (diff/i e s))

;; ------
;; diff/i
;; ------

(define-generic (diff/i e))

;; ----------
;; num-diff/i
;; ----------

(module+ test
  (require rackunit
           "parse.rkt")

  (check-equal? (diff/i (make-num 3) 'x)
                (make-num 0))
  (check-equal? (diff/i (make-num 0) 'y)
                (make-num 0))
  )

(define-instance ((diff/i num) n s)
  (make-num 0))

;; -----------
;; frac-diff/i
;; -----------

(module+ test
  (check-equal? (diff/i (make-frac 3 4) 'x)
                (make-num 0))
  )

(define-instance ((diff/i frac) f s)
  (make-num 0))

;; ----------
;; sym-diff/i
;; ----------

(module+ test
  (check-equal? (diff/i (make-sym 'x) 'x)
                (make-num 1))
  (check-equal? (diff/i (make-sym 'x) 'y)
                (make-sym 'x))
  )

(define-instance ((diff/i sym) sm s)
  (if (equal? (sym-val sm) s)
      (make-num 1)
      sm))

;; ----------
;; add-diff/i
;; ----------

(module+ test
  (check-equal? (diff/i (make-add (make-sym 'x) (make-num 3)) 'x)
                (make-add (make-num 1) (make-num 0)))
  )

(define-instance ((diff/i add) a s)
  (add (for/list ([addend (add-addends a)])
         (diff/i addend s))))

;; ----------
;; mul-diff/i
;; ----------

(module+ test
  (check-equal? (diff/i (make-mul (make-sym 'x) (make-num 3)) 'x)
                (make-add (make-mul (make-num 0) (make-sym 'x))
                          (make-mul (make-num 3) (make-num 1))))
  )

(define-instance ((diff/i mul) m s)
  (define (diff/i/2 f1 f2)
    (define d/f1 (diff/i f1 s))
    (define d/f2 (diff/i f2 s))
    (make-add (make-mul d/f2 f1)
              (make-mul f2 d/f1)))
  (define (diff/i/n fs)
    (if (= (length fs) 1)
        (car fs)
        (diff/i/2 (car fs)
                         (diff/i/n (cdr fs)))))
  (diff/i/n (mul-factors m)))

;; ------------
;; power-diff/i
;; ------------

(module+ test
  (check-equal? (diff/i (parse-sexpr '(expt x 3)) 'x)
                (parse-sexpr '(* (expt x 3)
                                 (+ (* 1 3 (expt x -1))
                                    (* 0 (ln x))))))
  )

(define-instance ((diff/i power) p s)
  (define base (power-base p))
  (define d/base (diff/i base s))
  (define exponent (power-exponent p))
  (define d/exponent (diff/i exponent s))
  (make-mul p 
            (make-add (make-mul d/base
                                exponent
                                (make-power base (make-num -1)))
                      (make-mul d/exponent
                                (make-logn base 
                                           (make-num (exp 1)))))))

;; -----------
;; logn-diff/i
;; -----------

(module+ test
  (check-equal? (diff/i (make-logn (make-sym 'x) (make-num (exp 1))) 'x)
                (make-mul (make-num 1)
                          (make-power (make-sym 'x) (make-num -1))))
  )

(define-instance ((diff/i logn) l s)
  (define n (logn-n l))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (make-mul (diff/i n s)
                   (make-power n (make-num -1))))
        (else
          (error "Not implemented"))))

;; --------------------
;; polynomial/si-diff/i
;; --------------------

(module+ test
  (check-equal? (diff/i (make-polynomial/si 'x '(0 1 2)) 'x)
                (make-polynomial/si 'x '(1 4)))
  (check-equal? (diff/i (make-polynomial/si 'x '(3 3 3 3)) 'x)
                (make-polynomial/si 'x '(3 6 9)))
  (check-equal? (diff/i (make-polynomial/si 'y '(9 0 0 0 0 1)) 'y)
                (make-polynomial/si 'y '(0 0 0 0 5)))
  (check-equal? (diff/i (make-polynomial/si 'y '()) 'y)
                (make-polynomial/si 'y '()))
  (check-equal? (diff/i (make-polynomial/si 'y '(4)) 'y)
                (make-polynomial/si 'y '()))
  (check-equal? (diff/i (make-polynomial/si 'y '(0 0 1)) 'y)
                (make-polynomial/si 'y '(0 2)))
  (check-equal? (diff/i (make-polynomial/si 'y '(0 0 1)) 'x)
                (make-polynomial/si 'y '(0 0 1)))
  )

(define-instance ((diff/i polynomial/si) p s)
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
