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
  )

(define (dexpr-add->latex dexpr)
  (string-join
    (map dexpr->latex (dexpr-flatten/pred dexpr-add? dexpr))
    " + "))

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
  )

(define (dexpr-mul->latex dexpr)
  (define exprs (dexpr-flatten/pred dexpr-mul? dexpr))
  (define exprs+ (filter positive-exponent? exprs))
  (define exprs- (filter (negate positive-exponent?) exprs))
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

(define (string-startswith s prefix)
  (string=? (substring s 0 (string-length prefix)) prefix))

; -----------------
; dexpr-expt->latex
; -----------------

(module+ test
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt a 3)))
                "a^{3}")
  (check-equal? (dexpr->latex (sexpr->dexpr '(expt (+ x 1) (+ x 1))))
                "(x + 1)^{x + 1}")
  )

(define (dexpr-expt->latex dexpr)
  (define base (dexpr-expt-base dexpr))
  (define power (dexpr-expt-power dexpr))
  (cond ((and (dexpr-num? power)
              (< (dexpr-num-val power) 0))
         (latex/fraction "1"
                         (dexpr->latex
                           (dexpr-simplify
                             (dexpr-expt base 
                                         (dexpr-num (abs (dexpr-num-val power))))))))
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
; positive-exponent?
; ------------------

(define (positive-exponent? dexpr)
  (cond ((not (dexpr-expt? dexpr))
         #t)
        (else
          (define power (dexpr-expt-power dexpr))
          (and (dexpr-num? power)
               (>= (dexpr-num-val power) 0)))))

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
