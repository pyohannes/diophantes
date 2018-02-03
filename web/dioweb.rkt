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
        (<stylesheet> "diophantus.css")
        (<script> "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML")))

(define (<stylesheet> href)
  `(link ((rel "stylesheet")
          (type "text/css")
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
    '(h1 'Delta "iophantus")
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
    (<f/tablerow> "Input" formula)
    (<f/tablerow> "Formula" (format-math (dexpr->latex f/dexpr)))
    (if (not (equal? f/dexpr f/dexpr-simple))
        (<f/tablerow> "Simplified formula" 
                      (format-math (dexpr->latex f/dexpr-simple)))
        '())))

(define (<f/tablerow> caption text)
  `(tr
     (td ((class "caption")) ,caption)
     (td ((class "formula")) ,text)))

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
    (not (null? l)))
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
