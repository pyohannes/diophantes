#lang racket/base

(provide dexpr-plot-png)

(require plot/no-gui
         file/convertible
         net/base64
         racket/math
         "dexpr.rkt")

(module+ test
  (require rackunit)

  (dexpr-plot-png (sexpr->dexpr '(+ x 9)))
  )

(define (dexpr-plot-png dexpr)
  (define f (function (dexpr->lambda dexpr) (- pi) pi))
  (define a (axes #:x-labels? #t #:y-labels? #t))
  (parameterize ([plot-decorations? #f])
    (bytes->string/utf-8
      (base64-encode 
        (convert (plot-bitmap (list f a))
                 'png-bytes)
        ""))))
