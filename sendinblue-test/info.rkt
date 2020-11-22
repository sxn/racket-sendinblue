#lang info

(define collection "tests")

(define deps '("base" "sendinblue"))
(define build-deps '("rackunit-lib"
                     "web-server-lib"))

(define update-implies '("sendinblue"))
