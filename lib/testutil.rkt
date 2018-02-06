#lang racket/base

(provide (struct-out mock/num)
         (struct-out mock/add)
         (struct-out mock/mul)
         (all-from-out rackunit))

(require "dexprgen.rkt"
         rackunit)

; --------------
; implementation

(require racket/generic)

(struct mock/num (val)
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr->latex dexpr)
     (number->string (mock/num-val dexpr)))
   ])

(struct mock/add (add aug)
  #:transparent
  #:methods gen:dexpr
  [(define/generic dexpr->latex@super dexpr->latex)
   (define (dexpr-children e)
     (list (mock/add-add e) (mock/add-aug e)))
   (define (dexpr->latex dexpr)
     (string-append (dexpr->latex@super (mock/add-add dexpr))
                    " + "
                    (dexpr->latex@super (mock/add-aug dexpr))))
   ])

(struct mock/mul (mpr mpd)
  #:transparent
  #:methods gen:dexpr
  [(define (dexpr-children e)
     (list (mock/mul-mpr e) (mock/mul-mpd e)))])

