#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"sek-lib\"")

(define pkg-authors '(samth))

(define build-deps '(["sek-lib" #:version "0.1"]
                     "rackunit-lib"))

(define update-implies '("sek-lib"))

(define license
  '(Apache-2.0 OR MIT))
