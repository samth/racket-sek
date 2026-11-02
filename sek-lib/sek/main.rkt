#lang racket/base
;; Sek: catenable, splittable, transient sequences for Racket.
;;
;; An implementation of Arthur Charguéraud and François Pottier, "A Catenable,
;; Splittable, Transient Sequence Data Structure", Proc. ACM Program. Lang. 10,
;; ICFP, Article 308 (August 2026).  https://doi.org/10.1145/3828706
;;
;; The library offers two views of one data structure:
;;
;;   psek -- a persistent sequence: every operation returns a new version and
;;           leaves the old one intact, so taking a snapshot is free
;;   esek -- an ephemeral sequence: operations update it in place
;;
;; `esek-snapshot` and `psek-edit` convert between them in constant time, and
;; the two flavors share their internal representation.
;;
;; On top of the core there are first-class iterators, segments -- the runs of
;; contiguous storage an iterator can hand out in one piece -- and a set of
;; derived operations that work on either flavor, returning results of the
;; same flavor as their argument.

(require "config.rkt"
         ;; generic.rkt provides faster for-clause versions of these two
         (except-in "persistent.rkt" in-psek)
         (except-in "ephemeral.rkt" in-esek)
         "iterator.rkt"
         "segment.rkt"
         "generic.rkt"
         "check.rkt")

;; ---- configuration (§4.1)
(provide sek-configure!

         ;; ---- persistent sequences (§3.1-3.5)
         psek?
         psek
         empty-psek
         psek-empty?
         psek-length
         psek-cons
         psek-add
         psek-push-front
         psek-push-back
         psek-pop-front
         psek-pop-back
         psek-first
         psek-last
         psek-ref
         psek-set
         psek-append
         psek-split
         psek-take
         psek-drop
         psek->list
         list->psek
         psek->vector
         vector->psek
         psek-for-each
         psek-map
         in-psek

         ;; ---- ephemeral sequences (§3.6)
         esek?
         esek
         make-esek
         esek-empty?
         esek-length
         esek-cons!
         esek-add!
         esek-push-front!
         esek-push-back!
         esek-pop-front!
         esek-pop-back!
         esek-first
         esek-last
         esek-ref
         esek-set!
         esek-append!
         esek-concat!
         esek-split!
         esek-carve!
         esek-take!
         esek-drop!
         esek-clear!
         esek-assign!
         esek->list
         list->esek
         esek->vector
         esek-for-each
         in-esek

         ;; ---- conversions between the two flavors (§2.1, §3.6)
         esek-snapshot
         esek-snapshot-and-clear!
         psek-edit
         esek-copy

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

         ;; ---- operations that work on either flavor
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
         sek-take-right
         sek-drop-right
         sek-insert
         sek-delete
         sek-index-of
         sek-copy
         esek-fill!
         esek-copy!
         make-psek
         build-psek
         build-esek
         sequence->psek
         sequence->esek
         for/esek
         for*/esek
         for/psek
         for*/psek

         ;; ---- runtime validation (Appendix A)
         (rename-out [check-psek sek-validate-psek] [check-esek sek-validate-esek]))
