#lang info

(define collection "sek")
(define version "0.1")
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "data-lib"))
(define scribblings '(("scribblings/sek.scrbl" ())))
(define pkg-desc "Catenable, splittable, transient sequences (Charguéraud & Pottier, ICFP 2026)")
(define pkg-authors '(samth))
(define test-omit-paths '("bench.rkt"))
