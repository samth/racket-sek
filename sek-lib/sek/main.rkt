#lang racket/base
;; Sek: catenable, splittable, transient sequences for Racket.
;;
;; An implementation of Arthur Charguéraud and François Pottier, "A Catenable,
;; Splittable, Transient Sequence Data Structure", Proc. ACM Program. Lang. 10,
;; ICFP, Article 308 (August 2026).  https://doi.org/10.1145/3828706
;;
;; The library offers two views of one data structure:
;;
;;   pseq -- a persistent sequence: every operation returns a new version and
;;           leaves the old one intact, so taking a snapshot is free
;;   eseq -- an ephemeral sequence: operations update it in place
;;
;; `eseq-snapshot` and `pseq-edit` convert between them in constant time, and
;; the two flavours share their internal representation.
;;
;; On top of the core there are first-class iterators, segments -- the runs of
;; contiguous storage an iterator can hand out in one piece -- and a set of
;; derived operations that work on either flavour, returning results of the
;; same flavour as their argument.

(require "config.rkt"
         ;; generic.rkt provides faster for-clause versions of these two
         (except-in "persistent.rkt" in-pseq)
         (except-in "ephemeral.rkt" in-eseq)
         "iterator.rkt"
         "segment.rkt"
         "generic.rkt"
         "check.rkt")

;; ---- configuration (§4.1)
(provide sek-configure!

         ;; ---- persistent sequences (§3.1-3.5)
         pseq?
         pseq
         empty-pseq
         pseq-empty?
         pseq-length
         pseq-push-front
         pseq-push-back
         pseq-pop-front
         pseq-pop-back
         pseq-first
         pseq-last
         pseq-ref
         pseq-set
         pseq-append
         pseq-split
         pseq-take
         pseq-drop
         pseq->list
         list->pseq
         pseq->vector
         vector->pseq
         pseq-for-each
         pseq-map
         in-pseq

         ;; ---- ephemeral sequences (§3.6)
         eseq?
         eseq
         make-eseq
         eseq-empty?
         eseq-length
         eseq-push-front!
         eseq-push-back!
         eseq-pop-front!
         eseq-pop-back!
         eseq-first
         eseq-last
         eseq-ref
         eseq-set!
         eseq-append!
         eseq-concat!
         eseq-split!
         eseq-carve!
         eseq-take!
         eseq-drop!
         eseq-clear!
         eseq-assign!
         eseq->list
         list->eseq
         eseq->vector
         eseq-for-each
         in-eseq

         ;; ---- conversions between the two flavours (§2.1, §3.6)
         eseq-snapshot
         eseq-snapshot-and-clear!
         pseq-edit
         eseq-copy

         ;; ---- iterators (§5.4)
         sek-iterator
         sek-iterator-at-sentinel
         sek-iter?
         sek-iter-copy
         sek-iter-reset!
         sek-iter-sequence
         sek-iter-length
         sek-iter-index
         sek-iter-finished?
         sek-iter-valid?
         sek-iter-get
         sek-iter-get*
         sek-iter-move!
         sek-iter-get-and-move!
         sek-iter-get-and-move*!
         sek-iter-jump!
         sek-iter-reach!
         sek-iter-segment
         sek-iter-segment*
         sek-iter-segment-and-jump!
         sek-iter-segment-and-jump*!
         sek-iter-set!
         sek-iter-set-and-move!
         sek-iter-writable-segment
         sek-iter-writable-segment*
         sek-iter-writable-segment-and-jump!
         sek-iter-writable-segment-and-jump*!
         sek-iter-check

         ;; ---- segments
         segment
         segment?
         segment-vector
         segment-start
         segment-length
         segment-valid?
         segment-empty?
         segment-ref
         segment-set!
         segment-for-each
         segment-for-each2
         in-segment
         segment->list
         segment->vector

         ;; ---- operations that work on either flavour
         sek?
         sek-length
         sek-empty?
         sek-ref
         sek-first
         sek-last
         in-sek
         sek-segments-for-each
         sek-for-each
         sek-for-each/index
         sek-fold-left
         sek-fold-right
         sek-find
         sek-find-index
         sek-find-map
         sek-for-all?
         sek-exists?
         sek-member?
         sek-memq?
         sek->list
         sek->vector
         sek-equal?
         sek-compare
         sek-segments-for-each2
         sek-for-each2
         sek-fold-left2
         sek-fold-right2
         sek-for-all2?
         sek-exists2?
         sek-map
         sek-map/index
         sek-map2
         sek-filter
         sek-filter-map
         sek-partition
         sek-reverse
         sek-zip
         sek-unzip
         sek-append*
         sek-append-map
         sek-sort
         sek-uniq
         sek-merge
         sek-sub
         sek-take
         sek-drop
         sek-copy
         sek-fill!
         sek-blit!
         make-pseq
         build-pseq
         build-eseq
         sequence->pseq
         sequence->eseq
         for/eseq
         for*/eseq
         for/pseq
         for*/pseq

         ;; ---- runtime validation (Appendix A)
         (rename-out [check-pseq sek-validate-pseq] [check-eseq sek-validate-eseq]))
