#lang racket/base
;; Every exported name must be documented.  This is what keeps the reference
;; honest as the library grows: an export added without a `defproc` fails here
;; rather than being noticed years later.
(require rackunit/docs-complete)

(check-docs (quote sek))
