#lang racket/base
;; A runtime validation function -- Charguéraud & Pottier, ICFP 2026,
;; Appendix A, Figure 19.  These checks document the invariants of the data
;; structure and are what the randomized tests lean on to catch a bug at the
;; step that introduces it rather than many operations later.

(require "config.rkt"
         "chunk.rkt"
         "ptree.rkt"
         "persistent.rkt"
         "ephemeral.rkt")

(provide check-tree
         check-chunk
         check-pseq
         check-eseq
         sek-check-failure)

(define (sek-check-failure fmt . args)
  (raise (exn:fail:sek (string-append "sek invariant violated: " (apply format fmt args))
                       (current-continuation-marks))))

(define-syntax-rule (want e fmt arg ...)
  (unless e
    (sek-check-failure fmt arg ...)))

;; c holds items of depth d; owner is the id of the ephemeral sequence that
;; owns this structure, or #f for a persistent one.
(define (check-chunk c d owner)
  (want (chunk-well-formed? c) "malformed chunk view/support at depth ~a" d)
  (want (= (chunk-capacity c) (capacity-at d))
        "chunk at depth ~a has capacity ~a, expected ~a"
        d
        (chunk-capacity c)
        (capacity-at d))
  (when (chunk-owned? c owner)
    (want (chunk-aligned? c) "owned chunk at depth ~a is not aligned" d))
  (define mw (measure-at d))
  (define w (for/sum ([i (in-range (chunk-length c))]) (mw (chunk-ref c i))))
  (want (= w (chunk-weight c))
        "chunk at depth ~a records weight ~a but holds ~a"
        d
        (chunk-weight c)
        w))

;; t is a tree at depth d.
(define (check-tree t d owner)
  (when t
    (define f (lvl-front t))
    (define m (lvl-middle t))
    (define b (lvl-back t))
    (want (> (lvl-weight t) 0) "level at depth ~a has weight 0" d)
    ;; invariant 1
    (when (or (chunk-empty? f) (chunk-empty? b))
      (want (not m) "invariant 1 broken at depth ~a: empty side, nonempty middle" d))
    (check-chunk f d owner)
    (check-chunk b d owner)
    (define wm (check-middle m d owner))
    (want (= (lvl-weight t) (+ (chunk-weight f) wm (chunk-weight b)))
          "level at depth ~a records weight ~a but holds ~a"
          d
          (lvl-weight t)
          (+ (chunk-weight f) wm (chunk-weight b)))))

;; Check the middle sequence of a level of depth d and return its weight.  Its
;; items are chunks of depth d items, so this is where invariants 2 and 3 live.
(define (check-middle m d owner)
  (check-tree m (add1 d) owner)
  (define k (capacity-at d))
  (define total 0)
  (define previous k)
  (pt-items-for-each m
                     (add1 d)
                     (lambda (c)
                       (check-chunk c d owner)
                       ;; invariant 2
                       (want (> (chunk-length c) 0) "invariant 2 broken at depth ~a: empty chunk" d)
                       ;; invariant 3
                       (want (> (+ previous (chunk-length c)) k)
                             "invariant 3 broken at depth ~a: adjacent chunks of size ~a and ~a"
                             d
                             previous
                             (chunk-length c))
                       (set! previous (chunk-length c))
                       (set! total (+ total (chunk-weight c)))))
  total)

(define (check-pseq s)
  (define r (pseq-rep s))
  (cond
    [(not r) (void)]
    [(vector? r)
     (want (<= 1 (vector-length r) (max 1 (short-threshold)))
           "compact sequence of length ~a exceeds the threshold ~a"
           (vector-length r)
           (short-threshold))]
    [else
     (check-tree r 0 no-owner)
     (want (> (lvl-weight r) (short-threshold))
           "sequence of length ~a should use the compact representation"
           (lvl-weight r))])
  s)

;; The corresponding validator for an ephemeral sequence (§3.6).  Its middle
;; is a persistent tree, so most of the work is the same; what is specific is
;; that each inner chunk must be either empty or full, and that a chunk
;; carrying this sequence's id really is aligned with its support.
(define (check-eseq e)
  (define id (eseq-id e))
  ;; a side that has never been pushed to carries the shared stand-in
  (define f (eseq-front e))
  (define b (eseq-back e))
  (unless (eq? f empty-chunk) (check-chunk f 0 id))
  (unless (eq? b empty-chunk) (check-chunk b 0 id))
  (for ([c (in-list (list (eseq-ifront e) (eseq-iback e)))]
        [name (in-list '(ifront iback))])
    (unless (eq? c empty-chunk)
      (check-chunk c 0 id)
      (want (chunk-full? c) "~a is neither empty nor full: ~a of ~a"
            name (chunk-length c) (chunk-capacity c))))
  (check-middle (eseq-middle e) 0 id)
  e)
