#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"sek\"")

(define pkg-authors '(samth))

(define build-deps '(["sek-lib" #:version "0.1"]
                     "racket-doc"
                     "scribble-lib"
                     "data-doc"
                     "data-lib"))

(define update-implies '("sek-lib"))

(define license
  '(Apache-2.0 OR MIT))
