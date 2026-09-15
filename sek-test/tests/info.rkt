#lang info

;; The benchmarks and the conformance harness live here because they belong to
;; the same body of work as the tests, but neither is a test: the benchmarks
;; report timings rather than pass or fail, and the conformance driver needs
;; the reference OCaml library built first.  See the README in each directory.
(define test-omit-paths
  '("sek/bench"
    "sek/conformance"))

;; The benchmarks compare against `gvector-append!`, which is not in a released
;; data-lib -- it exists only on the gvector branch they were written to
;; measure.  So they cannot be compiled on a stock installation, and are
;; shipped as source.  Run them with a data-lib that has it; see
;; sek/bench/README.md.
(define compile-omit-paths
  '("sek/bench"))
