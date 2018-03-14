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
        `(title "Diophantus")
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
    (with-handlers ([exn:fail? <errormessage>])
      (cond ((equal? formula "about")
             (<about>))
            ((equal? formula "help")
             (<help>))
            ((non-empty-string? formula)
             (<formulaoutput> formula))
            (else
              '()))))
  (list-non-null
    'body
    `(span ((class "header"))
       (a ((class "base navbar")
           (href "javascript:diowebOpen('about');"))
          "About")
       (a ((class "base navbar")
           (href "javascript:diowebOpen('help');"))
          "Help"))
    `(h1 ,(make-cdata #f #f "&Delta;iophantus"))
    (<formulainput> formula)
    formulaoutput))

(define (<about>)
  (define a-racket  '(a ((class "base")
                         (href "https://www.racket-lang.org")) "Racket"))
  (define a-mathjax '(a ((class "base")
                         (href "https://www.mathjax.org")) "MathJax"))
  (define a-function-plot
                    '(a ((class "base")
                         (href "https://mauriciopoppe.github.io/function-plot")) 
                       "function-plot"))
  (define a-roboto  '(a ((class "base")
                         (href "https://github.com/google/roboto")) "Roboto"))
  `(div ((class "textflow"))
    (h2 "About")
    (p
      "Diophantus is a CGI executable written in " ,a-racket ". It serves as "
      "a web frontend for the racket-symalg library.")
    (p 
      "racket-symalg is a Racket library which I developed as a pet project "
      "of mine. It allows one to express and manipulate algebraic expressions "
      "in Racket.")
    (p
      "Diophantus makes use of:"
      (ul
        (li ,a-mathjax " to display formulas,")
        (li ,a-function-plot " to plot functions of one variable and")
        (li "the " ,a-roboto " font.")))
    (p
      "All source code can be found on github:"
      (ul
        (li (a ((class "base")
                (href "https://github.com/pyohannes/racket-symalg"))
               "racket-symalg"))
        (li (a ((class "base")
                (href "https://github.com/pyohannes/diophantus"))
               "diophantus"))))
    (p
      "You can contact the author at "
      (a ((class "base")
          (href "mailto:johannes@johannes.tax"))
         "johannes@johannes.tax")
      ".")))

(define (<help>)
  `(div ((class "textflow"))
    (h2 "Help")
    (p
      "Diophantus is supposed to give derivatives and plots for arbitrary "
      "elementary functions. Currently supported operations are:"
      (ul
        (li "arithmetic operations \\(+ - \\times \\div\\)")
        (li "exponentials")
        (li "logarithms")
        (li "the constants \\(\\pi\\) and \\( e \\)")
        (li "the trigonometric functions \\(\\sin\\), \\(\\cos\\) and "
            "\\(\\tan\\)")))
    (p
      "A function can be entered in the input field as infix or as "
      "s-expression, on confirmation the function is simplified, plotted "
      "and the derivative is calculated.")
    (p
      "The following table shows what operations can be entered. Infix and "
      "s-expressions must not be mixed.")
    (table ((class "base help"))
      (tr ((class "header"))
        (td "Operation")
        (td "infix")
        (td "sexpr"))
      (tr
        (td "\\(x + 1\\)")
        (td "x + 1")
        (td "(+ x 1)"))
      (tr
        (td "\\(x - 1\\)")
        (td "x - 1")
        (td "(- x 1)"))
      (tr
        (td "\\(x \\times 1\\)")
        (td "x * 1")
        (td "(* x 1)"))
      (tr
        (td "\\(\\frac{1}{x}\\)")
        (td "1/x")
        (td "(/ 1 x)"))
      (tr
        (td "\\(x^2\\)")
        (td "x^2")
        (td "(expt x 2)"))
      (tr
        (td "\\( \\ln(x) \\)")
        (td "ln(x)")
        (td "(ln x)"))
      (tr
        (td "\\( \\log(x) \\)")
        (td "log(x)")
        (td "(log x)"))
      (tr
        (td "\\( \\pi \\)")
        (td "pi")
        (td "pi"))
      (tr
        (td "\\( e \\)")
        (td "e")
        (td "e"))
      (tr
        (td "\\( \\sin(x) \\)")
        (td "sin(x)")
        (td "(sin x)"))
      (tr
        (td "\\( \\cos(x) \\)")
        (td "cos(x)")
        (td "(cos x)"))
      (tr
        (td "\\( \\tan(x) \\)")
        (td "tan(x)")
        (td "(tan x)")))
  (p
    "In the expressions above \\( x \\) can be replaced by any other "
    "expression. For example, entering ")
  (p ((class "formula"))
    "(e^tan(x))/(1 - x^2)")
  (p
    "yields the function")
  (p "$$\\frac{e^{tan(x)}}{1 - x^2}$$")
  (p
    "This software is still work in progress and may give incorrect and "
    "suboptimal results in some cases. In case you have any problems, "
    "questions or suggestions, please contact "
    (a ((class "base")
        (href "mailto:johannes@johannes.tax"))
       "johannes@johannes.tax")
    ".")
  ))

(define (<errormessage> e)
  `(p ((class "error"))
      ,(exn-message e)))

(define (<formulainput> formula)
  (define js/submit "javascript:diowebOpen(document.getElementById('formula').value);")
  `(form ((action ,js/submit))
     (input ((type "text")
             (id "formula")
             (value ,formula)))
     (input ((type "submit")
             (value ">")))))

(define (<formulaoutput> formula)
  (define f/dexpr (parse formula))
  (define f/dexpr-simple (simplify f/dexpr))
  (define f/dexpr-deriv (simplify (differentiate f/dexpr-simple)))
  (list-non-null
    'table '((class "base main"))
    (<caption/tablerow> "Input")
    (<formula/tablerow> formula)
    (<caption/tablerow> "Formula")
    (<formula/tablerow> (format-math (latex f/dexpr-simple)
                                     f/dexpr-simple))
    (when (linear? f/dexpr-simple)
          (<caption/tablerow> "Plot"))
    (when (linear? f/dexpr-simple)
          (<formula/tablerow> (<plot> f/dexpr-simple)))
    (<caption/tablerow> "Derivative")
    (<formula/tablerow> (format-math (string-append
                                       "\\frac{\\delta}{\\delta x} = "
                                       (latex f/dexpr-deriv))
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
               width: 550,
               height: 350,
               target: '#plot',
               data: [{
                   fn: '~a',
                   graphType: 'polyline',
                   color: 'red'
               }]
           })" (infix f #t))))))

(define (<img> url)
  `(img ((src ,url))))

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
                   (~a (infix dexpr))
                   "');"))
  `(a ((href ,href))
      ,formula))

(define (parse s)
  (with-handlers ([exn:fail? (lambda (e)
                               (parse-infix s))])
    (parse-sexpr (string->sexpr s))))

(define (string->sexpr s)
  (define port (open-input-string s))
  (define sexpr (read port))
  (if (eof-object? (read port))
      sexpr
      (error "Not a valid s-expressions: " s)))

; -----------------
; main invocation

(define (main)
  (printf "Content-Type: text/html\n\n")
  (printf (xexpr->string (<html>))))

(main)
