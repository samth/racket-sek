#lang racket/base
;; Every exported name must be documented.  This is what keeps the reference
;; honest as the library grows: an export added without a `defproc` fails here
;; rather than being noticed years later.
;;
;; The body is in a `test` submodule like every other test here.  At top level
;; it was silently skipped by `raco test -x`, which is `--no-run-if-absent`:
;; with no submodule to run, the file was required only if `-x` was absent, so
;; the check never ran under the flag CI passes.
;;
;; sek-doc has to be installed for this to mean anything -- `check-docs`
;; reports every export as undocumented when it cannot find the documentation
;; at all -- which is why sek-test build-depends on it.
(require rackunit/docs-complete)

(provide run-docs-complete-tests)

(define (run-docs-complete-tests)
  (check-docs (quote sek)))

(module+ test
  (run-docs-complete-tests))
