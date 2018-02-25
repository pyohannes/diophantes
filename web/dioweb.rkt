#lang racket/base

(require xml
         racket/string
         racket/format
         "../lib/symalg/main.rkt")

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
        `(script ((type "text/javascript"))
           ,(make-cdata #f #f "
              function diowebOpen(formula) {
                  formula = encodeURIComponent(formula);
                  location.href = location.pathname + '?' + formula;
              }"))
        (<stylesheet> "diophantus.css")
        (<script> "http://cdnjs.cloudflare.com/ajax/libs/d3/3.5.5/d3.min.js")
        (<script> "https://wzrd.in/standalone/function-plot@1.14.0")
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
  (define js/submit "diowebOpen(document.getElementById('formula').value);")
  `(div
     (input ((type "text")
             (id "formula")
             (value ,formula)))
     (input ((type "submit")
             (onclick ,js/submit)
             (value ">")))))

(define (<formulaoutput> formula)
  (define f/dexpr (parse-infix formula))
  (define f/dexpr-simple (simplify f/dexpr))
  (define f/dexpr-deriv (simplify (differentiate f/dexpr-simple)))
  (list-non-null
    'table
    (<caption/tablerow> "Input")
    (<formula/tablerow> formula)
    (<caption/tablerow> "Formula")
    (<formula/tablerow> (format-math (latex f/dexpr-simple)
                                     f/dexpr-simple))
    (<caption/tablerow> "Plot")
    (<formula/tablerow> (<plot> f/dexpr-simple))
    (<caption/tablerow> "Derivative")
    (<formula/tablerow> (format-math (latex f/dexpr-deriv)
                                     f/dexpr-deriv))))

(define (<caption/tablerow> text)
   `(tr ((class "caption"))
      (td ,text)
      (td)))

(define (<formula/tablerow> text)
   `(tr ((class "formula"))
      (td)
      (td ,text)))

(define (<plot> f)
  `(div
     (span ((id "plot")))
     (script ((type "text/javascript"))
       ,(make-cdata #f #f (format "
           functionPlot({
               target: '#plot',
               data: [{
                   fn: '~a',
                   graphType: 'polyline'
               }]
           })" (infix f))))))

(define (<img/png> data)
  (sleep 100)
  `(img ((src ,(string-append "data:image/png;base64,"
                              data)))))

; -----------------
; Utility functions

(define (formula-from-command-line)
  (decode-uri-arg
    (string-join
      (vector->list (current-command-line-arguments))
      "&")))

(define (decode-uri-arg s)
  (for/fold ([s s])
            ([char '("(" ")" "*" "^" )])
    (string-replace s (string-append "\\" char) char)))

(define (list-non-null . args)
  (define (non-null? l)
    (not (or (null? l) (void? l))))
  (apply list
         (filter non-null? args)))

(define (format-math latex dexpr)
  (define formula 
    (string-append "$$" latex "$$"))
  (define href
    (string-append "javascript:diowebOpen('"
                   (~a (sexpr dexpr))
                   "');"))
  `(a ((href ,href)
       (class "formula"))
      ,formula))

; -----------------
; main invocation

(define (main)
  (printf "Content-Type: text/html\n\n")
  (printf (xexpr->string (<html>))))

(main)
