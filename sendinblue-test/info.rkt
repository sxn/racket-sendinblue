#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "rackunit-lib"
                     "sendinblue-lib"
                     "web-server-lib"))

(define update-implies '("sendinblue-lib"))
