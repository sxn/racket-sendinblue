#lang racket/base

(provide remove-false-params)

(define (remove-false-params params)
  (for/fold ([params (hasheq)])
            ([(name value) params]
             #:when (and value (not (null? value))))
    (hash-set params name value)))
