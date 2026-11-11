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
