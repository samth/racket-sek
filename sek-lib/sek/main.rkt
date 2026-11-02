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
;; On top of the core there are first-class iterators, and a set of derived
;; operations that work on either flavor, returning results of the same flavor
;; as their argument.  Internally the iterators also hand out segments -- the
;; runs of contiguous storage the derived operations traverse in one piece --
;; but segments are not part of the public interface.

(require "config.rkt"
         ;; generic.rkt provides faster for-clause versions of these two
         (except-in "persistent.rkt" in-psek)
         (except-in "ephemeral.rkt" in-esek)
         "iterator.rkt"
         "segment.rkt"
         "generic.rkt"
         "check.rkt")

;; ---- persistent sequences (§3.1-3.5)
(provide psek?
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

         ;; ---- iterators (§5.4); the library itself uses the full set in
         ;; iterator.rkt, segments included
         (rename-out [sek-iterator-at sek-iterator])
         sek-iter?
         sek-iter-index
         sek-iter-finished?
         sek-iter-get
         sek-iter-move!
         sek-iter-reach!
         sek-iter-set!

         ;; ---- operations that work on either flavor
         sek?
         sek-length
         sek-empty?
         sek-ref
         sek-first
         sek-last
         in-sek
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

         ;; ---- what a program that breaks the threading rules can see
         (struct-out exn:fail:sek))

;; Not part of the public interface, and not documented; the tests use them.
;;   (require (submod sek config))    the tunable settings (§4.1)
;;   (require (submod sek validate))  runtime validation (Appendix A)
(module+ config
  (provide sek-configure!))
(module+ validate
  (provide (rename-out [check-psek sek-validate-psek] [check-esek sek-validate-esek])
           sek-iter-check))
