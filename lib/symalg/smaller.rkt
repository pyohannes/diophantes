#lang racket/base

;; Specifying the order relation of symbolic algebraic expressions.

(provide smaller?)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/match
         "private/data.rkt")
         

;; --------
;; smaller?
;; --------

(define-generic (smaller? e1 e2))

;; ---
;; num
;; ---

(module+ test
  (require rackunit)

  ;; check if all possible combinations are implemented
  (define terms 
    (list
      (make-num 1)
      (make-frac 1 2)
      (make-sym 'x)
      (make-constant 'pi)
      (make-add (make-sym 'x) (make-num 4))
      (make-mul (make-sym 'x) (make-num 4))
      (make-power (make-sym 'x) (make-num 3))
      (make-logn (make-sym 'x) (make-num 2))
    ))

  (for ([e1 terms]
        #:when #t
        [e2 terms])
    (smaller? e1 e2))

  (define-syntax check-smaller
    (syntax-rules ()
      [(check-smaller e1 e2)
       (begin
         (check-true  (smaller? e1 e2))
         (check-false (smaller? e2 e1)))]))
       
  ;; O-1: Numbers and fractions
  (check-smaller (make-num 3) (make-num 4))
  (check-smaller (make-frac 3 4) (make-frac 4 3))
  (check-smaller (make-frac 3 4) (make-num 1))

  ;; O-7: num < any/c
  (check-smaller (make-num 3) (make-constant 'pi))
  (check-smaller (make-num 3) (make-sym 'a))
  (check-smaller (make-num 3) (make-mul (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-num 3) (make-add (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-num 3) (make-logn (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-num 3) (make-power (make-sym 'a) (make-sym 'b)))
  )

(define-instance ((smaller? num num) n1 n2)
  (< (num-val n1) (num-val n2)))

(define-instance ((smaller? frac num) f n)
  (< (constant->number f) (constant->number n)))

(define-instance ((smaller? num frac) n f)
  (< (constant->number n) (constant->number f)))

(define-instance ((smaller? num constant) n c)
  #t)

(define-instance ((smaller? num sym) n s)
  #t)

(define-instance ((smaller? num add) n a)
  #t)

(define-instance ((smaller? num mul) n m)
  #t)

(define-instance ((smaller? num logn) n l)
  #t)

(define-instance ((smaller? num power) n p)
  #t)

;; ----
;; frac
;; ----

(module+ test
  ;; O-1: Numbers and fractions
  (check-smaller (make-frac 3 4) (make-frac 4 3))

  ;; O-7: frac < any/c
  (check-smaller (make-frac 3 4) (make-constant 'pi))
  (check-smaller (make-frac 3 4) (make-sym 'a))
  (check-smaller (make-frac 3 4) (make-mul (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-frac 3 4) (make-add (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-frac 3 4) (make-logn (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-frac 3 4) (make-power (make-sym 'a) (make-sym 'b)))
  )

(define-instance ((smaller? frac frac) f1 f2)
  (< (constant->number f1) (constant->number f2)))

(define-instance ((smaller? frac constant) n c)
  #t)

(define-instance ((smaller? frac sym) n s)
  #t)

(define-instance ((smaller? frac add) n a)
  #t)

(define-instance ((smaller? frac mul) n m)
  #t)

(define-instance ((smaller? frac logn) n l)
  #t)

(define-instance ((smaller? frac power) n p)
  #t)

;; --------
;; constant
;; --------

(module+ test
  ;; O-1: Numbers and constanttions
  (check-smaller (make-constant 'e) (make-constant 'pi))

  ;; O-7: constant < any/c
  (check-smaller (make-num 3) (make-constant 'pi))
  (check-smaller (make-frac 3 4) (make-constant 'pi))
  (check-smaller (make-constant 'pi) (make-sym 'a))
  (check-smaller (make-constant 'pi) (make-mul (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-constant 'pi) (make-add (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-constant 'pi) (make-logn (make-sym 'a) (make-sym 'b)))
  (check-smaller (make-constant 'pi) (make-power (make-sym 'a) (make-sym 'b)))
  )

(define-instance ((smaller? constant num) c n)
  #f)

(define-instance ((smaller? constant frac) c f)
  #f)

(define-instance ((smaller? constant constant) c1 c2)
  (symbol<? (constant-name c1)
            (constant-name c2)))

(define-instance ((smaller? constant sym) c s)
  #t)

(define-instance ((smaller? constant add) c a)
  #t)

(define-instance ((smaller? constant mul) c m)
  #t)

(define-instance ((smaller? constant logn) c l)
  #t)

(define-instance ((smaller? constant power) c p)
  #t)

;; ---
;; sym
;; ---

(module+ test
  (check-smaller (make-constant 'pi) (make-sym 'y))

  ;; O-2: Symbols
  (check-smaller (make-sym 'x) (make-sym 'y))

  ;; O-12: function < sym
  (check-smaller (make-sym 'x) 
                 (make-logn (make-sym 'y) (make-num 2)))
  )

(define-instance ((smaller? sym sym) s1 s2)
  (symbol<? (sym-val s1) (sym-val s2)))

(define-instance ((smaller? sym num) s n)
  #f)

(define-instance ((smaller? sym frac) s f)
  #f)

(define-instance ((smaller? sym constant) s c)
  #f)

(define-instance ((smaller? sym add) s a)
  (smaller? (make-add (make-num 0) s) a))

(define-instance ((smaller? sym mul) s m)
  (smaller? (make-mul (make-num 1) s) m))

(define-instance ((smaller? sym power) s p)
  (smaller? (make-power s (make-num 1)) p))

(define-instance ((smaller? sym logn) s l)
  #t)

;; ---
;; add
;; ---

(module+ test
  ;; O-3 Addition and multiplication
  (check-smaller (make-add (make-sym 'a) (make-sym 'b))
                 (make-add (make-sym 'a) (make-sym 'c)))
  (check-smaller (make-add (make-sym 'a) (make-sym 'c) (make-sym 'd))
                 (make-add (make-sym 'b) (make-sym 'c) (make-sym 'd)))
  (check-smaller (make-add (make-sym 'c) (make-sym 'd))
                 (make-add (make-sym 'b) (make-sym 'c) (make-sym 'd)))

  ;; O-10: sum < function, symbol
  (check-smaller (make-add (make-num 1) (make-sym 'x))
                 (make-sym 'y))
  (check-smaller (make-add (make-num 1) (make-logn (make-sym 'x) (make-num 2)))
                 (make-logn (make-sym 'x) (make-num 10)))
  )

(define-instance ((smaller? add add) a1 a2)
  (smaller-op-list (add-addends a1) (add-addends a2)))

(define-instance ((smaller? add num) a n)
  #f)

(define-instance ((smaller? add frac) a f)
  #f)

(define-instance ((smaller? add constant) a c)
  #f)

(define-instance ((smaller? add sym) a s)
  (smaller? a (make-add (make-num 0) s)))

(define-instance ((smaller? add mul) a m)
  (smaller? (make-mul (make-num 1) a) m))

(define-instance ((smaller? add power) a p)
  (smaller? (make-power a (make-num 1)) p))

(define-instance ((smaller? add logn) a l)
  (smaller? a (make-add (make-num 0) l)))

;; ---
;; mul
;; ---

(module+ test
  ;; O-3 Addition and multiplication
  (check-smaller (make-mul (make-sym 'a) (make-sym 'b))
                 (make-mul (make-sym 'a) (make-sym 'c)))
  (check-smaller (make-mul (make-sym 'a) (make-sym 'c) (make-sym 'd))
                 (make-mul (make-sym 'b) (make-sym 'c) (make-sym 'd)))
  (check-smaller (make-mul (make-sym 'c) (make-sym 'd))
                 (make-mul (make-sym 'b) (make-sym 'c) (make-sym 'd)))

  ;; O-8 mul < power, sum, function
  (check-smaller (make-mul (make-sym 'a) 
                           (make-power (make-sym 'x) (make-num 2)))
                 (make-power (make-sym 'x) (make-num 3)))
  (check-smaller (make-mul (make-sym 'a) 
                           (make-num 2))
                 (make-add (make-num 1)
                           (make-sym 'x) (make-num 3)))
  (check-smaller (make-mul (make-sym 'a) 
                           (make-num 2))
                 (make-logn (make-num 3) (make-num 10)))
  (check-smaller (make-mul (make-sym 'a) 
                           (make-num 2))
                 (make-sym 'x))
  )

(define-instance ((smaller? mul mul) m1 m2)
  (smaller-op-list (mul-factors m1) (mul-factors m2)))

(define-instance ((smaller? mul num) m n)
  #f)

(define-instance ((smaller? mul constant) m c)
  #f)

(define-instance ((smaller? mul sym) m s)
  (smaller? m (make-mul (make-num 1) s)))

(define-instance ((smaller? mul frac) m f)
  #f)

(define-instance ((smaller? mul add) m a)
  (smaller? m (make-mul (make-num 1) a)))

(define-instance ((smaller? mul power) m p)
  (smaller? m (make-mul (make-num 1) p)))

(define-instance ((smaller? mul logn) m l)
  (smaller? m (make-mul (make-num 1) l)))

;; -----
;; power
;; -----

(module+ test
  ;; O-4.1: Powers with different bases
  (check-smaller (make-power (make-sym 'x) (make-num 10))
                 (make-power (make-sym 'y) (make-num 9)))
  ;; O-4.2: Powers with equal bases
  (check-smaller (make-power (make-sym 'x) (make-num 9))
                 (make-power (make-sym 'x) (make-num 10)))
  ;; O-9: power < sum, function, symbol
  (check-smaller (make-power (make-sym 'x) (make-num 4))
                 (make-add (make-num 3) (make-sym 'y)))
  (check-smaller (make-power (make-sym 'x) (make-num 4))
                 (make-logn (make-sym 'y) (make-num 3)))
  (check-smaller (make-sym 'x)
                 (make-power (make-sym 'x) (make-num 4)))
  )

(define-instance ((smaller? power power) p1 p2)
  (define b1 (power-base p1))
  (define b2 (power-base p2))
  (define e1 (power-exponent p1))
  (define e2 (power-exponent p2))
  (cond ((equal? b1 b2)
         (smaller? e1 e2))
        (else
          (smaller? b1 b2))))

(define-instance ((smaller? power num) p n)
  #f)

(define-instance ((smaller? power frac) p f)
  #f)

(define-instance ((smaller? power constant) p c)
  #f)

(define-instance ((smaller? power sym) p s)
  (smaller? p (make-power s (make-num 1))))

(define-instance ((smaller? power add) p a)
  (smaller? p (make-power a (make-num 1))))

(define-instance ((smaller? power mul) p m)
  (smaller? (make-mul (make-num 1) p) m))

(define-instance ((smaller? power logn) p l)
  (smaller? p (make-power l (make-num 1))))

;; ---
;; log
;; ---

(module+ test
  ;; O-6: Functions of the same type
  (check-smaller (make-logn (make-sym 'x) (make-num 2))
                 (make-logn (make-sym 'x) (make-num 10)))
  (check-smaller (make-logn (make-sym 'x) (make-num 10))
                 (make-logn (make-sym 'y) (make-num 10)))
  (check-smaller (make-logn (make-sym 'x) (make-num 2))
                 (make-logn (make-sym 'a) (make-num 10)))
  )

(define-instance ((smaller? logn logn) l1 l2)
  (smaller-op-list (list (logn-n l1) (logn-base l1))
                   (list (logn-n l2) (logn-base l2))))

(define-instance ((smaller? logn num) l n)
  #f)

(define-instance ((smaller? logn frac) l f)
  #f)

(define-instance ((smaller? logn constant) l c)
  #f)

(define-instance ((smaller? logn add) l a)
  (smaller? (make-add (make-num 0) l) a))

(define-instance ((smaller? logn mul) l m)
  (smaller? (make-mul (make-num 1) l) m))

(define-instance ((smaller? logn power) l p)
  (smaller? (make-power l (make-num 1)) p))

(define-instance ((smaller? logn sym) l s)
  #f)

; ---------------
; smaller-op-list
; ---------------

(module+ test
  ;; O-3: Operands of multiplications and additions
  (check-true  (smaller-op-list (list (make-sym 'a) (make-sym 'b))
                                (list (make-sym 'a) (make-sym 'c))))
  (check-false (smaller-op-list (list (make-sym 'a) (make-sym 'c)) 
                                (list (make-sym 'a) (make-sym 'b))))
  (check-true  (smaller-op-list 
                 (list (make-sym 'a) (make-sym 'c) (make-sym 'd))
                 (list (make-sym 'b) (make-sym 'c) (make-sym 'd))))
  (check-false (smaller-op-list 
                 (list (make-sym 'b) (make-sym 'c) (make-sym 'd))
                 (list (make-sym 'a) (make-sym 'c) (make-sym 'd))))
  (check-true  (smaller-op-list
                 (list (make-sym 'c) (make-sym 'd))
                 (list (make-sym 'b) (make-sym 'c) (make-sym 'd))))
  (check-false (smaller-op-list
                 (list (make-sym 'b) (make-sym 'c) (make-sym 'd))
                 (list (make-sym 'c) (make-sym 'd))))
  )

(define (smaller-op-list l1 l2)
  (define (smaller-op-list/r l1 l2)
    (cond ((and (null? l1) (null? l2))
           #t)
          ((null? l1)
           #t)
          ((null? l2)
           #f)
          ((equal? (car l1) (car l2))
           (smaller-op-list/r (cdr l1) (cdr l2)))
          (else
            (smaller? (car l1) (car l2)))))
  (smaller-op-list/r (reverse l1) (reverse l2)))


; ----------------
; constant->number
; ----------------

(module+ test
  (check-equal? (constant->number (make-num 3))
                3)
  (check-equal? (constant->number (make-frac 3 4))
                (/ 3 4))
  )

(define (constant->number c)
  (match c
    [(? num? c)
     (num-val c)]
    [(? frac? c)
     (/ (frac-num c) (frac-denom c))]))
