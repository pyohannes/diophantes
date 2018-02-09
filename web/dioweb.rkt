#lang racket/base

(require xml
         racket/string
         "../lib/diophantus.rkt")

; ---------------------------------------
; functions returning HTML xexpr entities

(define (<html>)
  (list 'html 
        (<header>) 
        (<body>)))

(define (<header>)
  (list 'head
        (<icon> "diophantus_icon.png")
        `(title ,(make-cdata #f #f "&Delta;iophantus"))
        (<stylesheet> "diophantus.css")
        (<script> "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML")))

(define (<stylesheet> href)
  `(link ((rel "stylesheet")
          (type "text/css")
          (href ,href))))

(define (<icon> href)
  `(link ((rel "icon")
          (href ,href))))

(define (<script> href)
  `(script ((src ,href))))

(define (<body>)
  (define formula (formula-from-command-line))
  (define formulaoutput
    (if (non-empty-string? formula)
        (<formulaoutput> formula)
        '()))
  (list-non-null
    'body
    `(h1 ,(make-cdata #f #f "&Delta;iophantus"))
    (<formulainput> formula)
    formulaoutput))

(define (<formulainput> formula)
  (define js/submit "
    formula = document.getElementById('formula').value;
    formula = encodeURIComponent(formula);
    location.href = location.pathname + '?' + formula;
    ")
  `(div
     (input ((type "text")
             (id "formula")
             (value ,formula)))
     (input ((type "submit")
             (onclick ,js/submit)
             (value ">")))))

(define (<formulaoutput> formula)
  (define f/dexpr (sexpr->dexpr (read (open-input-string formula))))
  (define f/dexpr-simple (dexpr-simplify f/dexpr))
  (list-non-null
    'table
    (<caption/tablerow> "Input")
    (<formula/tablerow> formula)
    (<caption/tablerow> "Formula")
    (<formula/tablerow> (format-math (dexpr->latex f/dexpr)))
    (when (not (equal? f/dexpr f/dexpr-simple))
        (<caption/tablerow> "Simplified formula"))
    (when (not (equal? f/dexpr f/dexpr-simple))
        (<formula/tablerow> (format-math (dexpr->latex f/dexpr-simple))))
    (<caption/tablerow> "Derivative")
    (<formula/tablerow> (format-math (dexpr-deriv->latex
                                     (dexpr-deriv/auto f/dexpr-simple))))))

(define (<caption/tablerow> text)
   `(tr ((class "caption"))
      (td ,text)
      (td)))

(define (<formula/tablerow> text)
   `(tr ((class "formula"))
      (td)
      (td ,text)))

; -----------------
; Utility functions

(define (formula-from-command-line)
  (decode-uri-arg
    (string-join
      (vector->list (current-command-line-arguments))
      "&")))

(define (decode-uri-arg s)
  (for/fold ([s s])
            ([char '("(" ")" "*")])
    (string-replace s (string-append "\\" char) char)))

(define (list-non-null . args)
  (define (non-null? l)
    (not (or (null? l) (void? l))))
  (apply list
         (filter non-null? args)))

(define (format-math f)
  (string-append "$$" f "$$"))

; -----------------
; main invocation

(define (main)
  (printf "Content-Type: text/html\n\n")
  (printf (xexpr->string (<html>))))

(main)
