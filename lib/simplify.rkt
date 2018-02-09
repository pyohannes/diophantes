#lang racket/base

;; Reduce d-expressions to minimal normal form.

(require racket/contract)

(provide 
  (contract-out 
    [sexpr->dexpr/s     (-> any/c dexpr?)]
    [dexpr-simplify     (-> dexpr? dexpr?)]))

;; ---------------------------------
;; Import and implementation section

(require "sort.rkt")

(module+ test
  (require rackunit)

  (check-equal? (sexpr->dexpr/s '(+ 3 4))
                (sexpr->dexpr/s '(+ 4 3)))
  )

; --------------
; sexpr->dexpr/s
; --------------

(define (sexpr->dexpr/s sexpr)
  (dexpr-simplify (sexpr->dexpr sexpr)))

; --------------
; dexpr-simplify
; --------------

(module+ test

  (define (check-simplify s:in s:simple)
    (check-equal? (dexpr->sexpr (sexpr->dexpr/s s:in))
                  s:simple))

  ;; 1. Solve solvable arithmetic operations.
  (check-simplify '(+ 1 1) 2)
  (check-simplify '(* 2 2) 4)
  (check-simplify '(expt 2 3) 8)

  ;; 2. In additions order symbols lexicographically, numbers at the end.
  (check-simplify '(+ 1 b a) '(+ a b 1))
  (check-simplify '(+ b (* 2 a)) '(+ (* 2 a) b))

  ;; 3. In multiplications, order symbols lexicographically, numbers at the
  ;; begin.
  (check-simplify '(* a 2) '(* 2 a))
  (check-simplify '(* b a 2) '(* 2 a b))

  ;; 4. Remove superfluous operations.
  (check-simplify '(* a 1) 'a)
  (check-simplify '(* a 0) 0)
  (check-simplify '(+ a 0) 'a)
  (check-simplify '(expt a 0) 1)
  (check-simplify '(expt a 1) 'a)

  ;; 5. Turn additions of same terms into multiplications.
  (check-simplify '(+ a a) '(* 2 a))

  ;; 6. Turn multiplications of same terms into exponentiations.
  (check-simplify '(* a 1 a) '(expt a 2))
  (check-simplify '(* a b a) '(* (expt a 2) b))
  (check-simplify '(* a (expt a 2)) '(expt a 3))

  ;; Combined usage of the rules above.
  (check-simplify '(* (+ 1 1) (+ 1 1)) 4)
  (check-simplify '(+ (* 2 2) (* 2 2)) 8)
  (check-simplify '(+ 1 a 1) '(+ a 2))
  (check-simplify '(* 1 a 1) 'a)
  (check-simplify '(+ a 1 a) '(+ (* 2 a) 1))
  (check-simplify '(+ 1 (* a a)) '(+ (expt a 2) 1))
  (check-simplify '(* 2 (* a a)) '(* 2 (expt a 2)))
  (check-simplify '(expt b (* a a)) '(expt b (expt a 2)))
  (check-simplify '(+ (* a b c a b) (* d d))
                  '(+ (* (expt a 2) (expt b 2) c) (expt d 2)))
  (check-simplify '(* 2 (+ a a))
                  '(* 4 a))
  (check-simplify '(expt (* x y) 3)
                  '(* (expt x 3) (expt y 3)))
  )

(define (dexpr-simplify dexpr)
  (define (_ simple dexpr)
    (if (equal? simple dexpr)
        simple
        (_ (dexpr-simplify-single simple)
           simple)))
  (_ dexpr (not dexpr)))

; ---------------------
; dexpr-simplify-single
; ---------------------
 
(define (dexpr-simplify-single dexpr)
  (cond ((dexpr-add? dexpr)
         (simplify-apply dexpr
           simplify/add-flatten))
        ((dexpr-mul? dexpr)
         (simplify-apply dexpr
           simplify/mul-flatten))
        ((dexpr-expt? dexpr)
         (simplify-apply dexpr
           simplify/expt-children
           simplify/expt-num
           simplify/expt-0-1
           simplify/expt-mul-base))
        (else
          dexpr)))

; --------------
; simplify-apply
; --------------
 
(define (simplify-apply dexpr . fs)
  (cond ((not (null? fs))
         (define dexpr-new ((car fs) dexpr))
         (if (equal? (object-name dexpr-new) 
                     (object-name dexpr))
             (apply simplify-apply (cons dexpr-new (cdr fs)))
             dexpr-new))
        (else
          dexpr)))

; --------------------
; simplify/add-flatten
; --------------------
 
(define (simplify/add-flatten e)
  (define (%simplify es)
    (if (dexpr-add? es)
      (simplify-apply es
        simplify/add-children
        simplify/add-num
        simplify/add-0
        simplify/add-sym)
      es))
  (define (fold-add es)
    (foldr (lambda (x y)
             (%simplify (dexpr-add x y)))
           (dexpr-num 0)
           es))
  (fold-add
    (sort
      (map fold-add
           (%group-by-pred 
             mul-addables
             (map %simplify
                  (dexpr-flatten/pred dexpr-add? e))))
      dexpr-<)))

; ----------------
; simplify/add-num
; ----------------
 
(module+ test
  (check-equal? (simplify/add-num (dexpr-add (dexpr-num 5) (dexpr-num 6)))
                (dexpr-num 11))
  )

(define (simplify/add-num e)
  (define add (dexpr-simplify (dexpr-add-add e)))
  (define aug (dexpr-simplify (dexpr-add-aug e)))
  (if (and (dexpr-num? add) (dexpr-num? aug))
      (dexpr-num (+ (dexpr-num-val add)
                    (dexpr-num-val aug)))
      e))

; ---------------------
; simplify-add-children
; ---------------------

(module+ test
  (check-equal? (simplify/add-children (sexpr->dexpr '(+ 1 (* a a))))
                (sexpr->dexpr '(+ 1 (expt a 2))))
  )

(define (simplify/add-children e)
  (dexpr-add (dexpr-simplify (dexpr-add-add e))
             (dexpr-simplify (dexpr-add-aug e))))

; ----------------
; simplify/add-sym
; ----------------

(module+ test
  (check-equal? (simplify/add-sym (sexpr->dexpr '(+ a a)))
                (sexpr->dexpr '(* 2 a)))
  (check-equal? (simplify/add-sym (sexpr->dexpr '(+ (* 2 a) a)))
                (sexpr->dexpr '(* 3 a)))
  (check-equal? (simplify/add-sym (sexpr->dexpr '(+ (* 2 a b) a)))
                (sexpr->dexpr '(+ (* 2 a b) a)))
  (check-equal? (simplify/add-sym (sexpr->dexpr '(+ (* 2 a b) (* a b))))
                (sexpr->dexpr '(* 3 a b)))
  (check-equal? (simplify/add-sym (sexpr->dexpr '(+ (* a b) (* 2 a b))))
                (sexpr->dexpr '(* 3 a b)))
  )
 
(define (simplify/add-sym e)
  (define add (dexpr-simplify (dexpr-add-add e)))
  (define aug (dexpr-simplify (dexpr-add-aug e)))
  (define addaug (mul-addables add aug))
  (if addaug
      (dexpr-mul (dexpr-num (+ (dexpr-num-val (dexpr-mul-mpr (car addaug)))
                               (dexpr-num-val (dexpr-mul-mpr (cadr addaug)))))
                 (dexpr-mul-mpd (car addaug)))
      e))

; ------------
; mul-addables
; ------------

(module+ test
  (check-equal? (mul-addables (sexpr->dexpr 'a)
                              (sexpr->dexpr 'a))
                (list (sexpr->dexpr '(* 1 a))
                      (sexpr->dexpr '(* 1 a))))
  (check-equal? (mul-addables (sexpr->dexpr '(* 2 a))
                              (sexpr->dexpr 'a))
                (list (sexpr->dexpr '(* 2 a))
                      (sexpr->dexpr '(* 1 a))))
  (check-equal? (mul-addables (sexpr->dexpr 'a)
                              (sexpr->dexpr '(* 2 a)))
                (list (sexpr->dexpr '(* 1 a))
                      (sexpr->dexpr '(* 2 a))))
  )

(define (mul-addables add aug)
  (define elems `( (,add ,aug)
                   (,(dexpr-mul (dexpr-num 1) add) ,aug)
                   (,add ,(dexpr-mul (dexpr-num 1) aug))
                   (,(dexpr-mul (dexpr-num 1) add) 
                    ,(dexpr-mul (dexpr-num 1) aug))))
  (for/first ([e elems]
              #:when (apply dexpr-factor? e))
    e))

; -------------
; dexpr-factor?
; -------------

(module+ test
  (check-true (dexpr-factor? (dexpr-mul (dexpr-num 3) (dexpr-sym 'a))
                             (dexpr-mul (dexpr-num 1) (dexpr-sym 'a))))
  (check-false (dexpr-factor? (dexpr-mul (dexpr-num 3) (dexpr-sym 'a))
                              (dexpr-mul (dexpr-num 1) (dexpr-sym 'b))))
  (check-false (dexpr-factor? (dexpr-mul (dexpr-num 3) (dexpr-sym 'a))
                              (dexpr-sym 'b)))
  )

(define (dexpr-factor? d f)
  (and (dexpr-mul? d) (dexpr-mul? f)
       (dexpr-num? (dexpr-mul-mpr d))
       (dexpr-num? (dexpr-mul-mpr f))
       (equal? (dexpr-mul-mpd d) (dexpr-mul-mpd f))))


; --------------
; simplify/add-0
; --------------

(module+ test
  (check-equal? (simplify/add-0 (dexpr-add (dexpr-sym 'a) (dexpr-num 0)))
                (dexpr-sym 'a))
  (check-equal? (simplify/add-0 (dexpr-add (dexpr-num 0) (dexpr-sym 'a)))
                (dexpr-sym 'a))
  )

(define (simplify/add-0 e)
  (let ((add (dexpr-add-add e))
        (aug (dexpr-add-aug e)))
    (cond ((and (dexpr-num? add) (equal? (dexpr-num-val add) 0))
           aug)
          ((and (dexpr-num? aug) (equal? (dexpr-num-val aug) 0))
           add)
          (else
            e))))

; ----------------
; simplify/mul-0-1
; ----------------
 
(define (simplify/mul-0-1 e)
  (let ((mpr (dexpr-mul-mpr e))
        (mpd (dexpr-mul-mpd e)))
    (cond ((or (and (dexpr-num? mpr) (equal? (dexpr-num-val mpr) 0))
               (and (dexpr-num? mpd) (equal? (dexpr-num-val mpd) 0)))
           (dexpr-num 0))
          ((and (dexpr-num? mpr) (equal? (dexpr-num-val mpr) 1))
           mpd)
          ((and (dexpr-num? mpd) (equal? (dexpr-num-val mpd) 1))
           mpr)
          (else
            e))))

; ---------------
; %group-by-pred?
; ---------------
 
(define (%group-by-pred pred? es)
  (define (g-b-p es groups)
    (if (null? es)
        groups
        (g-b-p (cdr es)
               (let loop ((e (car es))
                          (groups groups))
                 (if (null? groups)
                     (list (list e))
                     (if (pred? e (caar groups))
                         (cons (cons e (car groups))
                               (cdr groups))
                         (cons (car groups)
                               (loop e (cdr groups)))))))))
  (g-b-p es '()))

; ----------------
; simplify/mul-num
; ----------------
 
(define (simplify/mul-num e)
  (let ((mpr (dexpr-simplify (dexpr-mul-mpr e)))
        (mpd (dexpr-simplify (dexpr-mul-mpd e))))
    (if (and (dexpr-num? mpr)
             (dexpr-num? mpd))
        (dexpr-num (* (dexpr-num-val mpr)
                      (dexpr-num-val mpd)))
        e)))

; ---------------------
; simplify-mul-children
; ---------------------

(module+ test
  (check-equal? (simplify/mul-children (sexpr->dexpr '(* 2 (* a a))))
                (sexpr->dexpr '(* 2 (expt a 2))))
  )

(define (simplify/mul-children e)
  (dexpr-mul (dexpr-simplify (dexpr-mul-mpr e))
             (dexpr-simplify (dexpr-mul-mpd e))))

; -----------------
; simplify/mul-expt
; -----------------

(module+ test
  (check-equal? (simplify/mul-expt (sexpr->dexpr '(* a a)))
                (sexpr->dexpr '(expt a 2)))
  (check-equal? (simplify/mul-expt (sexpr->dexpr '(* (expt a 3) a)))
                (sexpr->dexpr '(expt a 4)))
  )

(define (simplify/mul-expt e)
  (define mpr (ensure-expt (dexpr-simplify (dexpr-mul-mpr e))))
  (define mpd (ensure-expt (dexpr-simplify (dexpr-mul-mpd e))))
  (define base (dexpr-simplify (dexpr-expt-base mpr)))
  (cond ((equal? base (dexpr-simplify (dexpr-expt-base mpd)))
         (dexpr-expt base (dexpr-simplify (dexpr-add (dexpr-expt-power mpr)
                                          (dexpr-expt-power mpd)))))
        (else
          e)))

; ----------------
; %mul-resolvable?
; ----------------
 
(define (%mul-resolvable? e1 e2)
  (define base1 (dexpr-expt-base (ensure-expt e1)))
  (define base2 (dexpr-expt-base (ensure-expt e2)))
  (or  (and (dexpr-num? e1) (dexpr-num? e2))
       (equal? base1 base2)))

; -----------
; ensure-expt
; -----------

(module+ test
  (check-equal? (ensure-expt (sexpr->dexpr '(expt a 2)))
                (sexpr->dexpr '(expt a 2)))
  (check-equal? (ensure-expt (sexpr->dexpr 'a))
                (sexpr->dexpr '(expt a 1)))
  (check-equal? (ensure-expt (sexpr->dexpr '(+ a b)))
                (sexpr->dexpr '(expt (+ a b) 1)))
  )

(define (ensure-expt e)
  (if (dexpr-expt? e)
      e
      (dexpr-expt e (dexpr-num 1))))

; --------------------
; simplify/mul-flatten
; --------------------
 
(define (simplify/mul-flatten e)
  (define (%simplify es)
    (if (dexpr-mul? es)
      (simplify-apply es
        simplify/mul-children
        simplify/mul-num
        simplify/mul-0-1
        simplify/mul-expt)
      es))
  (define (fold-mul es)
    (foldr (lambda (x y)
             (%simplify (dexpr-mul x y)))
           (dexpr-num 1)
           es))
  (fold-mul
    (sort
      (map fold-mul
           (%group-by-pred 
             %mul-resolvable?
             (map %simplify
                  (dexpr-flatten/pred dexpr-mul? e))))
      dexpr-mul-<)))

; -----------
; dexpr-mul-<
; -----------

(module+ test
  (check-false (dexpr-mul-< (dexpr-sym 'a) (dexpr-num 1)))
  (check-true  (dexpr-mul-< (dexpr-num 1) (dexpr-sym 'a)))
  (check-true  (dexpr-mul-< (dexpr-sym 'a) (dexpr-sym 'b)))
  (check-false (dexpr-mul-< (dexpr-sym 'b) (dexpr-sym 'a)))
  (check-false (dexpr-mul-< (sexpr->dexpr '(expt 2 a)) (dexpr-num 3)))
  (check-true  (dexpr-mul-< (dexpr-num 3) (sexpr->dexpr '(expt 2 a))))
  (check-true  (dexpr-mul-< (sexpr->dexpr '(expt 2 a)) (dexpr-sym 'b)))
  (check-false (dexpr-mul-< (dexpr-sym 'b) (sexpr->dexpr '(expt 2 a))))
  )

(define (dexpr-mul-< e1 e2)
  (match (list (dexpr-num? e1) (dexpr-num? e2))
    [(list #f #t) #f]
    [(list #t #f) #t]
    [_            (dexpr-< e1 e2)])) 


; -----------------
; simplify/expt-num
; -----------------

(module+ test
  (check-equal? (simplify/expt-num (sexpr->dexpr '(expt 2 3)))
                                   (sexpr->dexpr 8))
  )

(define (simplify/expt-num e)
  (define base (dexpr-expt-base e))
  (define power (dexpr-expt-power e))
  (if (and (dexpr-num? base) (dexpr-num? power))
      (dexpr-num (expt (dexpr-num-val base) 
                       (dexpr-num-val power)))
      e))

; ----------------------
; simplify/expt-children
; ----------------------

(module+ test
  (check-equal? (simplify/expt-children (sexpr->dexpr '(expt 2 (+ a a))))
                (sexpr->dexpr '(expt 2 (* 2 a))))
  )

(define (simplify/expt-children e)
  (dexpr-expt (dexpr-simplify (dexpr-expt-base e))
              (dexpr-simplify (dexpr-expt-power e))))

; -----------------
; simplify/expt-0-1
; -----------------

(module+ test
  (check-equal? (simplify/expt-0-1 (sexpr->dexpr '(expt a 0)))
                                   (sexpr->dexpr 1))
  (check-equal? (simplify/expt-0-1 (sexpr->dexpr '(expt a 1)))
                                   (sexpr->dexpr 'a))
  (check-equal? (simplify/expt-0-1 (sexpr->dexpr '(expt a b)))
                                   (sexpr->dexpr '(expt a b)))
  )

(define (simplify/expt-0-1 e)
  (match (dexpr-expt-power e)
    [(dexpr-num 0) (dexpr-num 1)]
    [(dexpr-num 1) (dexpr-expt-base e)]
    [_             e]))

; ----------------------
; simplify/expt-mul-base
; ----------------------

(module+ test
  (check-equal? (simplify/expt-mul-base (sexpr->dexpr '(expt (* x y) 3)))
                (sexpr->dexpr '(* (expt x 3) (expt y 3))))
  (check-equal? (simplify/expt-mul-base (sexpr->dexpr '(expt x 3)))
                (sexpr->dexpr '(expt x 3)))
  )

(define (simplify/expt-mul-base e)
  (define base (dexpr-expt-base e))
  (define power (dexpr-expt-power e))
  (cond ((dexpr-mul? base)
         (define exprs (dexpr-flatten/pred dexpr-mul? base))
         (for/fold ([expr (dexpr-expt (car exprs) power)])
                   ([b (cdr exprs)])
           (dexpr-mul expr
                      (dexpr-expt b power))))
        (else
          e)))

(require racket/contract
         racket/match
         "dexpr.rkt"
         "util.rkt")
