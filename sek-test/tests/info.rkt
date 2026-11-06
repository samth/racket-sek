#lang info

;; The benchmarks, the conformance harness and the race fuzzer live here
;; because they belong to the same body of work as the tests, but none is a
;; test: the benchmarks report timings rather than pass or fail, the
;; conformance driver needs the reference OCaml library built first, and the
;; fuzzer runs for as long as it is asked to.  See the README in each
;; directory, and the fuzzer's header.
(define test-omit-paths
  '("sek/bench"
    "sek/conformance"
    "sek/stress"))

;; The benchmarks compare against `gvector-append!`, which is not in a released
;; data-lib -- it exists only on the gvector branch they were written to
;; measure.  So they cannot be compiled on a stock installation, and are
;; shipped as source.  Run them with a data-lib that has it; see
;; sek/bench/README.md.
(define compile-omit-paths
  '("sek/bench"))
