#lang racket/base

(require json
         rackunit
         rackunit/text-ui
         "common.rkt")

(define sendblue-tests
  (test-suite
   "sendinblue-send-email"
   #:before start-server
   #:after stop-server))

(module+ test (run-tests sendblue-tests))
