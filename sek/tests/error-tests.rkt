#lang racket/base
;; Every module of the library is compiled with `(#%declare #:unsafe)`, so a
;; struct accessor or a vector reference no longer raises when handed the wrong
;; kind of value -- it reads whatever is at that offset.  Everything a caller
;; outside the library can reach therefore has to check its arguments with an
;; explicit `unless`, and this file is what holds that line: it calls the public
;; surface with wrong types and out-of-range indices and insists on an
;; exception.  Without the checks these are not failures, they are reads of
;; arbitrary memory.
(require rackunit
         racket/list
         "../main.rkt")

(define-syntax-rule (raises expr)
  (check-exn exn:fail? (lambda () expr) (format "~s should raise" 'expr)))

(define p (list->pseq '(1 2 3)))
(define e (list->eseq '(1 2 3)))
(define pa (vector->parray (vector 1 2 3)))
(define ea (vector->earray (vector 1 2 3)))

;; ------------------------------------------------- wrong type where a sequence
;; is expected.  7, a list and a vector are the three shapes most likely to be
;; passed by mistake, and none of them is a sequence of ours.
(test-case "persistent operations reject a non-pseq"
  (for ([bad (in-list (list 7 '(1 2 3) (vector 1 2 3) e))])
    (raises (pseq-length bad))
    (raises (pseq-empty? bad))
    (raises (pseq-ref bad 0))
    (raises (pseq-set bad 0 'x))
    (raises (pseq-push-front bad 'x))
    (raises (pseq-push-back bad 'x))
    (raises (pseq-pop-front bad))
    (raises (pseq-pop-back bad))
    (raises (pseq-first bad))
    (raises (pseq-last bad))
    (raises (pseq-append bad p))
    (raises (pseq-append p bad))
    (raises (pseq-split bad 1))
    (raises (pseq-take bad 1))
    (raises (pseq-drop bad 1))
    (raises (pseq->list bad))
    (raises (pseq->vector bad))
    (raises (pseq-edit bad))))

(test-case "ephemeral operations reject a non-eseq"
  (for ([bad (in-list (list 7 '(1 2 3) (vector 1 2 3) p))])
    (raises (eseq-length bad))
    (raises (eseq-empty? bad))
    (raises (eseq-ref bad 0))
    (raises (eseq-set! bad 0 'x))
    (raises (eseq-push-front! bad 'x))
    (raises (eseq-push-back! bad 'x))
    (raises (eseq-pop-front! bad))
    (raises (eseq-pop-back! bad))
    (raises (eseq-first bad))
    (raises (eseq-last bad))
    (raises (eseq-clear! bad))
    (raises (eseq-copy bad))
    (raises (eseq-snapshot bad))
    (raises (eseq-snapshot-and-clear! bad))
    (raises (eseq-split! bad 1))
    (raises (eseq->list bad))
    (raises (eseq->vector bad))))

(test-case "the generic surface rejects a non-sequence"
  (for ([bad (in-list (list 7 '(1 2 3) (vector 1 2 3)))])
    (raises (sek-length bad))
    (raises (sek-empty? bad))
    (raises (sek-ref bad 0))
    (raises (sek-first bad))
    (raises (sek-last bad))
    (raises (sek-for-each bad void))
    (raises (sek-fold-left bad + 0))
    (raises (sek-map bad add1))
    (raises (sek-filter bad odd?))
    (raises (sek->list bad))
    (raises (sek->vector bad))
    (raises (sek-take bad 1))
    (raises (sek-drop bad 1))
    (raises (sek-sub bad 0 1))
    (raises (sek-copy bad))
    (raises (sek-reverse bad))
    (raises (sek-equal? bad p))
    (raises (sek-equal? p bad))
    (raises (sek-iterator bad 'forward))))

(test-case "transient arrays reject the wrong flavour"
  (for ([bad (in-list (list 7 (vector 1 2 3) ea))])
    (raises (parray-length bad))
    (raises (parray-ref bad 0))
    (raises (parray-set bad 0 'x))
    (raises (parray-edit bad))
    (raises (parray->vector bad))
    (raises (parray->list bad)))
  (for ([bad (in-list (list 7 (vector 1 2 3) pa))])
    (raises (earray-length bad))
    (raises (earray-ref bad 0))
    (raises (earray-set! bad 0 'x))
    (raises (earray-snapshot bad))
    (raises (earray->vector bad))))

(test-case "segments reject a non-segment"
  (for ([bad (in-list (list 7 '(1 2) (vector 1 2)))])
    (raises (segment-length bad))
    (raises (segment-empty? bad))
    (raises (segment-ref bad 0))
    (raises (segment->list bad))
    (raises (segment->vector bad))
    (raises (segment-for-each bad void))))

(test-case "iterators reject a non-iterator"
  (for ([bad (in-list (list 7 p e))])
    (raises (sek-iter-get bad))
    (raises (sek-iter-valid? bad))
    (raises (sek-iter-sequence bad))
    (raises (sek-iter-finished? bad))
    (raises (sek-iter-move! bad 'forward))
    (raises (sek-iter-reach! bad 0))))

;; ------------------------------------------------------------ index range
(test-case "indices outside the sequence are rejected"
  (for ([i (in-list (list -1 3 4 100 'x 1.5 (expt 2 70)))])
    (raises (pseq-ref p i))
    (raises (eseq-ref e i))
    (raises (sek-ref p i))
    (raises (parray-ref pa i))
    (raises (earray-ref ea i)))
  ;; split, take and drop accept the length itself but nothing beyond it
  (check-equal? (pseq->list (pseq-take p 3)) '(1 2 3))
  (check-equal? (pseq->list (pseq-drop p 3)) '())
  (for ([i (in-list (list -1 4 100 'x))])
    (raises (pseq-split p i))
    (raises (pseq-take p i))
    (raises (pseq-drop p i))))

(test-case "an empty sequence has no first or last"
  (raises (pseq-first empty-pseq))
  (raises (pseq-last empty-pseq))
  (raises (pseq-pop-front empty-pseq))
  (raises (pseq-pop-back empty-pseq))
  (raises (eseq-first (make-eseq)))
  (raises (eseq-last (make-eseq)))
  (raises (eseq-pop-front! (make-eseq)))
  (raises (eseq-pop-back! (make-eseq))))

(test-case "sek-sub rejects a range outside the sequence"
  (for ([se (in-list (list (list 0 4) (list 2 2) (list -1 1) (list 1 -1) (list 4 0)))])
    (raises (sek-sub p (first se) (second se)))))

(test-case "an invalidated iterator raises rather than reading stale storage"
  (define q (list->eseq '(1 2 3 4)))
  (define it (sek-iterator q 'forward))
  (eseq-push-back! q 5)
  (raises (sek-iter-get it))
  (raises (sek-iter-move! it 'forward)))

(module+ test
  (void))
