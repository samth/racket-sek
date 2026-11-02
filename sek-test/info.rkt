#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"sek-lib\"")

(define pkg-authors '(samth))

;; No module requires sek-doc, but test-docs-complete.rkt needs its
;; documentation installed.
(define build-deps '(["sek-lib" #:version "1.0"]
                     "sek-doc"
                     "racket-index"
                     "rackunit-lib"))

(define update-implies '("sek-lib"))

(define license
  '(Apache-2.0 OR MIT))
