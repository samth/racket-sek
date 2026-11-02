#lang racket/base
;; The accessors of the library's own structures do not check their argument
;; (see sek/private/unchecked), so one handed the wrong kind of value does not
;; raise -- it reads whatever is at that offset.  Everything a caller outside
;; the library can reach therefore has to check its arguments with an
;; explicit `unless`, and this file is what holds that line: it calls the public
;; surface with wrong types and out-of-range indices and insists on an
;; exception.  Without the checks these are not failures, they are reads of
;; arbitrary memory.
(require rackunit
         racket/list
         ;; the iterator and segment operations the library uses internally
         ;; are checked too, though `sek` exports only some of them
         (except-in sek sek-iterator)
         sek/private/iterator
         sek/private/segment)

(define-syntax-rule (raises expr)
  (check-exn exn:fail? (lambda () expr) (format "~s should raise" 'expr)))

(define p (list->psek '(1 2 3)))
(define e (list->esek '(1 2 3)))

;; ------------------------------------------------- wrong type where a sequence
;; is expected.  7, a list and a vector are the three shapes most likely to be
;; passed by mistake, and none of them is a sequence of ours.
(test-case "persistent operations reject a non-psek"
  (for ([bad (in-list (list 7 '(1 2 3) (vector 1 2 3) e))])
    (raises (psek-length bad))
    (raises (psek-empty? bad))
    (raises (psek-ref bad 0))
    (raises (psek-set bad 0 'x))
    (raises (psek-push-front bad 'x))
    (raises (psek-push-back bad 'x))
    (raises (psek-pop-front bad))
    (raises (psek-pop-back bad))
    (raises (psek-first bad))
    (raises (psek-last bad))
    (raises (psek-append bad p))
    (raises (psek-append p bad))
    (raises (psek-split bad 1))
    (raises (psek-take bad 1))
    (raises (psek-drop bad 1))
    (raises (psek->list bad))
    (raises (psek->vector bad))
    (raises (psek-edit bad))))

(test-case "ephemeral operations reject a non-esek"
  (for ([bad (in-list (list 7 '(1 2 3) (vector 1 2 3) p))])
    (raises (esek-length bad))
    (raises (esek-empty? bad))
    (raises (esek-ref bad 0))
    (raises (esek-set! bad 0 'x))
    (raises (esek-push-front! bad 'x))
    (raises (esek-push-back! bad 'x))
    (raises (esek-pop-front! bad))
    (raises (esek-pop-back! bad))
    (raises (esek-first bad))
    (raises (esek-last bad))
    (raises (esek-clear! bad))
    (raises (esek-copy bad))
    (raises (esek-snapshot bad))
    (raises (esek-snapshot-and-clear! bad))
    (raises (esek-split! bad 1))
    (raises (esek->list bad))
    (raises (esek->vector bad))))

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

(test-case "segments reject a non-segment"
  (for ([bad (in-list (list 7 '(1 2) (vector 1 2)))])
    (raises (segment-length bad))
    (raises (segment-empty? bad))
    (raises (segment-ref bad 0))
    (raises (segment->list bad))
    (raises (segment->vector bad))
    (raises (segment-for-each bad void))))

;; segment-for-each reads the run unchecked, so the constructor has to refuse a
;; run that does not fit, or a vector it could not read that way
(test-case "segment rejects a run outside its vector"
  (raises (segment (vector 1 2) 0 5))
  (raises (segment (vector 1 2) 3 0))
  (raises (segment (vector 1 2) -1 1))
  (raises (segment "ab" 0 1))
  (raises (segment (chaperone-vector (vector 1 2) (lambda (v i x) x) (lambda (v i x) x)) 0 1))
  (check-equal? (segment->list (segment (vector 1 2 3) 1 2)) '(2 3)))

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
    (raises (psek-ref p i))
    (raises (esek-ref e i))
    (raises (sek-ref p i)))
  ;; split, take and drop accept the length itself but nothing beyond it
  (check-equal? (psek->list (psek-take p 3)) '(1 2 3))
  (check-equal? (psek->list (psek-drop p 3)) '())
  (for ([i (in-list (list -1 4 100 'x))])
    (raises (psek-split p i))
    (raises (psek-take p i))
    (raises (psek-drop p i))))

(test-case "an empty sequence has no first or last"
  (raises (psek-first empty-psek))
  (raises (psek-last empty-psek))
  (raises (psek-pop-front empty-psek))
  (raises (psek-pop-back empty-psek))
  (raises (esek-first (make-esek)))
  (raises (esek-last (make-esek)))
  (raises (esek-pop-front! (make-esek)))
  (raises (esek-pop-back! (make-esek))))

(test-case "sek-sub rejects a range outside the sequence"
  (for ([se (in-list (list (list 0 4) (list 2 2) (list -1 1) (list 1 -1) (list 4 0)))])
    (raises (sek-sub p (first se) (second se)))))

(test-case "an invalidated iterator raises rather than reading stale storage"
  (define q (list->esek '(1 2 3 4)))
  (define it (sek-iterator q 'forward))
  (esek-push-back! q 5)
  (raises (sek-iter-get it))
  (raises (sek-iter-move! it 'forward)))

(module+ test
  (void))
