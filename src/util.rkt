#lang racket/base

;; Internally used utility functions.

(provide
  dexpr-flatten
  dexpr-flatten/pred
  )

;; ---------------------------------
;; Import and implementation section

(require "dexprgen.rkt")

; -------------
; dexpr-flatten
; -------------

(module+ test
  (require "testutil.rkt")
  (check-equal? (dexpr-flatten (mock/add (mock/mul (mock/num 3) (mock/num 4))
                                         (mock/add (mock/num 5) (mock/num 6))))
                (map mock/num '(3 4 5 6)))
  )

(define (dexpr-flatten e)
  (define children (dexpr-children e))
  (if (null? children)
      (list e)
      (foldr append '() (map dexpr-flatten children))))

; ------------------
; dexpr-flatten/pred
; ------------------

(module+ test
  (check-equal? (dexpr-flatten/pred mock/add?
                  (mock/add (mock/add (mock/num 3) (mock/num 4))
                            (mock/num 5)))
                (map mock/num '(3 4 5)))
  (check-equal? (dexpr-flatten/pred mock/add?
                  (mock/add (mock/mul (mock/num 3) (mock/num 4))
                            (mock/add (mock/num 5) (mock/num 6))))
                (list (mock/mul (mock/num 3) (mock/num 4))
                      (mock/num 5)
                      (mock/num 6)))
  )

(define (dexpr-flatten/pred pred? e)
  (if (pred? e)
      (apply append
        (map (lambda (c)
               (dexpr-flatten/pred pred? c))
             (dexpr-children e)))
      (list e)))

