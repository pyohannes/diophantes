#lang racket/base

;; Convert d-expressions to LaTeX

(require racket/contract)

(provide
  (contract-out
    [dexpr->latex           (-> dexpr? string?)]
  ))

;; ---------------------------------
;; Import and implementation section

(require racket/match
         racket/string
         "dexpr.rkt"
         "simplify.rkt"
         "util.rkt")

; ------------
; dexpr->latex
; ------------

(module+ test
  (require rackunit)

  (check-equal? (dexpr->latex 
                  (dexpr-simplify 
                    (sexpr->dexpr '(expt (expt x 2) -1))))
                "\\frac{1}{x^{2}}")
  (check-equal? (dexpr->latex 
                  (dexpr-simplify 
                    (sexpr->dexpr '(/ -1 (expt x (* -1 -2))))))
                "-\\frac{1}{x^{2}}")
  )

(define (dexpr->latex dexpr)
  (match dexpr
    [(? dexpr-num? d)
     (dexpr-num->latex d)]
    [(? dexpr-sym? d)
     (dexpr-sym->latex d)]
    [(? dexpr-add? d)
     (dexpr-add->latex d)]
    [(? dexpr-mul? d)
     (dexpr-mul->latex d)]
    [(? dexpr-expt? d)
     (dexpr-expt->latex d)]
    [(? dexpr-log? d)
     (dexpr-log->latex d)]
    [_
      (error "Unsupported d-expression: " dexpr)]))

; ----------------
; dexpr-num->latex
; ----------------

(module+ test
  (check-equal? (dexpr->latex (dexpr-num 3))
                "3")
  )

(define (dexpr-num->latex dexpr)
  (number->string (dexpr-num-val dexpr)))

; ----------------
; dexpr-sym->latex
; ----------------

(module+ test
  (check-equal? (dexpr->latex (dexpr-sym 'a))
                "a")
  )

(define (dexpr-sym->latex dexpr)
  (symbol->string (dexpr-sym-val dexpr)))

; ----------------
; dexpr-add->latex
; ----------------

(module+ test
  (check-equal? (dexpr->latex (sexpr->dexpr '(+ a 3)))
                "a + 3")
  (check-equal? (dexpr->latex (sexpr->dexpr '(+ a b c 3)))
                "a + b + c + 3")
  (check-equal? (dexpr->latex (sexpr->dexpr '(+ x -3)))
                "x -3")
  )

(define (dexpr-add->latex dexpr)
  (define latex
    (map dexpr->latex (dexpr-flatten/pred dexpr-add? dexpr)))
  (for/fold ([s (car latex)])
            ([l (cdr latex)])
    (string-append s
                   (if (string-startswith l "-")
                       " "
                       " + ")
                   l)))

; ----------------
; dexpr-mul->latex
; ----------------

(module+ test
  (check-equal? (dexpr->latex (sexpr->dexpr '(* 3 a)))
                "3a")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* 3 a b c)))
                "3abc")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* a (+ b 2))))
                "a(b + 2)")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* (+ a 1) (+ b 2))))
                "(a + 1)(b + 2)")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* (expt a 3) (expt b 2))))
                "a^{3}b^{2}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(/ x y)))
                "\\frac{x}{y}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(/ (* -1 x) y)))
                "-\\frac{x}{y}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* (* -1 x) (* -1 y))))
                "xy")
  (check-equal? (dexpr->latex (sexpr->dexpr '(* z (expt x (- z 1)))))
                "zx^{z -1}")
  )

(define (dexpr-mul->latex dexpr)
  (define exprs (dexpr-flatten/pred dexpr-mul? dexpr))
  (define exprs+ (filter (negate negative-exponent?) exprs))
  (define exprs- (filter negative-exponent? exprs))
  (define exprs-/
    (map (lambda (x)
           (define base (dexpr-expt-base x))
           (define power (dexpr-expt-power x))
           (dexpr-simplify
             (dexpr-expt base
                         (dexpr-mul (dexpr-num -1)
                                    power))))
         exprs-))
  (define latex+
    (map (dexpr->latex/paren/pred dexpr-add?)
         exprs+))
  (define latex-/
    (map (dexpr->latex/paren/pred dexpr-add?)
         exprs-/))
  (define minus 
    (odd? 
      (length 
        (filter (lambda (s) (string-startswith s "-"))
                (append latex+ latex-/)))))
  (define (remove-minus s)
    (cond ((or (string=? s "1")
               (string=? s "-1"))
           "")
          ((string-startswith s "-")
           (substring s 1))
          (else
            s)))
  (define nominator
    (apply string-append (map remove-minus latex+)))
  (define denom
    (apply string-append (map remove-minus latex-/)))
  (string-append (if minus
                     "-"
                     "")
                 (latex/fraction (if (string=? nominator "")
                                     "1"
                                     nominator)
                                 denom)))


; -----------------
; dexpr-expt->latex
; -----------------

(module+ test
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt a 3)))
                "a^{3}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt (+ x 1) (+ x 1))))
                "(x + 1)^{x + 1}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt x -z)))
                "\\frac{1}{x^{z}}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt x (- -z 1))))
                "\\frac{1}{x^{z + 1}}")
  )

(define (dexpr-expt->latex dexpr)
  (define base (dexpr-expt-base dexpr))
  (define power (dexpr-expt-power dexpr))
  (cond ((negative-exponent? dexpr)
         (latex/fraction "1"
                         (dexpr->latex
                           (dexpr-simplify
                             (dexpr-expt base 
                                         (dexpr-mul (dexpr-num -1) 
                                                    power))))))
        (else
              (string-append (dexpr->latex/paren base)
                             "^{"
                             (dexpr->latex power)
                             "}"))))

; -----------------
; dexpr-log->latex
; -----------------

(module+ test
  (check-equal? (dexpr->latex (dexpr-log (dexpr-num (exp 1))
                                         (dexpr-sym 'x)))
                "\\ln x")
  )

(define (dexpr-log->latex dexpr)
  (define base (dexpr-log-base dexpr))
  (define n (dexpr-log-n dexpr))
  (define logop
    (cond ((equal? base (dexpr-num (exp 1)))
           "\\ln")
          (else
            (string-append "\\log_{"
                           (dexpr->latex n)
                           "}"))))
  (string-append logop
                 " "
                 (dexpr->latex/paren n)))

; ------------------
; negative-exponent?
; ------------------

(module+ test
  (check-false (negative-exponent? (sexpr->dexpr '(expt x (- y 1)))))
  (check-true  (negative-exponent? (sexpr->dexpr '(expt x -3))))
  (check-true  (negative-exponent? (sexpr->dexpr '(expt x -y))))
  (check-true  (negative-exponent? (sexpr->dexpr '(expt x (- -z 1)))))
  )

(define (negative-exponent? dexpr)
  (define (negative? dexpr)
    (or (and (dexpr-num? dexpr)
             (<= (dexpr-num-val dexpr) 0))
        (and (dexpr-mul? dexpr)
             (dexpr-num? (dexpr-mul-mpr dexpr))
             (< (dexpr-num-val (dexpr-mul-mpr dexpr)) 0))
        (and (dexpr-add? dexpr)
             (negative? (dexpr-add-add dexpr)))))
  (cond ((not (dexpr-expt? dexpr))
         #f)
        (else
          (define power (dexpr-expt-power dexpr))
          (negative? power))))

; -----------------
; dexpr-latex/paren
; -----------------

(module+ test
  (check-equal? (dexpr->latex/paren (dexpr-num 3))
                "3")
  (check-equal? (dexpr->latex/paren (dexpr-add (dexpr-num 3) (dexpr-num 4)))
                "(3 + 4)")
  )

(define (dexpr->latex/paren e)
  (define estr (dexpr->latex e))
  (if (null? (dexpr-children e))
      estr
      (string-append "(" estr ")")))

; -----------------------
; dexpr->latex/paren/pred
; -----------------------

(module+ test
  (check-equal? (dexpr->latex/paren (dexpr-num 3))
                "3")
  (check-equal? ((dexpr->latex/paren/pred dexpr-num?)
                 (dexpr-add (dexpr-num 3) (dexpr-num 4)))
                "3 + 4")
  (check-equal? ((dexpr->latex/paren/pred dexpr-add?)
                 (dexpr-add (dexpr-num 3) (dexpr-num 4)))
                "(3 + 4)")
  )

(define (dexpr->latex/paren/pred pred?)
  (lambda (e)
    (if (pred? e)
        (dexpr->latex/paren e)
        (dexpr->latex e))))

(define (latex/fraction num denom)
  (if (non-empty-string? denom)
      (string-append "\\frac{" num "}{" denom "}")
      num))
