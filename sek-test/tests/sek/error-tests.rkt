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

;; ------------------------------------------- the other arguments, and blame
;; The tests above pass the wrong sequence.  These pass the wrong everything
;; else -- an index, a side, a direction, a callback, a count, a mode -- and
;; insist that the error names the operation the caller called.
(define-syntax-rule (raises/who who expr)
  (check-exn (lambda (x)
               (and (exn:fail:contract? x)
                    (regexp-match? (string-append "^" (regexp-quote (symbol->string who)) ":")
                                   (exn-message x))))
             (lambda () expr)
             (format "~s should raise, naming ~a" 'expr who)))

;; Each of these empties its argument before it builds the result, so it has
;; to check everything first.
(test-case "a destructive operation with a bad argument leaves its input alone"
  (for* ([op (in-list (list esek-split! esek-carve! esek-take! esek-drop!))]
         [i (in-list (list 4 -1 'x 1.5 (expt 2 70)))])
    (define q (esek 'a 'b 'c))
    (raises/who (object-name op) (op q i))
    (check-equal? (esek->list q) '(a b c) (format "~a ~s" (object-name op) i)))
  (for ([op (in-list (list esek-carve! esek-take! esek-drop!))])
    (define q (esek 'a 'b 'c))
    (raises/who (object-name op) (op q 1 'sideways))
    (check-equal? (esek->list q) '(a b c)))
  (define q (esek 'a 'b))
  (define r (esek 'c))
  (raises/who 'esek-append! (esek-append! q r 'sideways))
  (check-equal? (esek->list q) '(a b))
  (check-equal? (esek->list r) '(c)))

(test-case "a callback is checked even when the sequence is empty"
  (for ([s (in-list (list empty-psek (make-esek)))])
    (raises/who 'sek-for-each (sek-for-each s 'wrong))
    (raises/who 'sek-for-each/index (sek-for-each/index s car))
    (raises/who 'sek-fold-left (sek-fold-left s car 0))
    (raises/who 'sek-fold-right (sek-fold-right s car 0))
    (raises/who 'sek-find (sek-find s cons))
    (raises/who 'sek-find-index (sek-find-index s cons))
    (raises/who 'sek-find-map (sek-find-map s cons))
    (raises/who 'sek-for-all? (sek-for-all? s cons))
    (raises/who 'sek-exists? (sek-exists? s cons))
    (raises/who 'sek-member? (sek-member? s 1 car))
    (raises/who 'sek-index-of (sek-index-of s 1 car))
    (raises/who 'sek-map (sek-map s 'wrong))
    (raises/who 'sek-map/index (sek-map/index s car))
    (raises/who 'sek-filter (sek-filter s cons))
    (raises/who 'sek-filter-map (sek-filter-map s cons))
    (raises/who 'sek-partition (sek-partition s cons))
    (raises/who 'sek-append-map (sek-append-map s cons))
    (raises/who 'sek-sort (sek-sort s car))
    (raises/who 'sek-uniq (sek-uniq s car))
    (raises/who 'sek-for-each2 (sek-for-each2 s s car))
    (raises/who 'sek-fold-left2 (sek-fold-left2 s s cons 0))
    (raises/who 'sek-fold-right2 (sek-fold-right2 s s cons 0))
    (raises/who 'sek-for-all2? (sek-for-all2? s s car))
    (raises/who 'sek-exists2? (sek-exists2? s s car))
    (raises/who 'sek-map2 (sek-map2 s s car))
    (raises/who 'sek-merge (sek-merge s s car))
    (raises/who 'sek-equal? (sek-equal? s s car))
    (raises/who 'sek<? (sek<? s s car)))
  (raises/who 'psek-for-each (psek-for-each empty-psek 'wrong))
  (raises/who 'psek-map (psek-map empty-psek 'wrong))
  (raises/who 'esek-for-each (esek-for-each (make-esek) 'wrong))
  (raises/who 'esek-map! (esek-map! (make-esek) 'wrong))
  (raises/who 'esek-map! (esek-map! (make-esek) cons))
  (raises/who 'esek-map/index! (esek-map/index! (make-esek) car))
  (raises/who 'esek-map! (esek-map! (psek 1 2) add1))
  (raises/who 'esek-map/index! (esek-map/index! (psek 1 2) +))
  (raises/who 'build-psek (build-psek 0 cons))
  (raises/who 'build-esek (build-esek 0 cons)))

(test-case "a callback's results are checked where the operation uses them"
  (raises/who 'sek-append-map (sek-append-map (psek 1) (lambda (x) (list x))))
  (raises/who 'sek-unzip (sek-unzip (psek (cons 1 2) 3))))

(test-case "directions, sides and modes are checked"
  (for ([s (in-list (list p e))])
    (raises/who 'sek-for-each (sek-for-each s void 'sideways))
    (raises/who 'sek-for-each/index (sek-for-each/index s void 'sideways))
    (raises/who 'sek-find (sek-find s odd? 'sideways))
    (raises/who 'sek-find-index (sek-find-index s odd? 'sideways))
    (raises/who 'sek-find-map (sek-find-map s odd? 'sideways))
    (raises/who 'sek->list (sek->list s 'sideways))
    (raises/who 'sek-for-each2 (sek-for-each2 s s void 'sideways))
    (raises/who 'in-sek (for/list ([x (in-sek s 'sideways)]) x))
    (raises/who 'in-sek (in-sek s 'sideways))
    (raises/who 'sek-copy (sek-copy s #:mode 'deep)))
  (raises/who 'in-psek (for/list ([x (in-psek p 'sideways)]) x))
  (raises/who 'in-psek (in-psek p 'sideways))
  (raises/who 'in-esek (for/list ([x (in-esek e 'sideways)]) x))
  (raises/who 'in-esek (in-esek e 'sideways))
  (raises/who 'sek-iter-move! (sek-iter-move! (sek-iterator p) 'sideways)))

(test-case "in-psek and in-esek take only their own flavor, in a for clause too"
  (raises/who 'in-psek (for/list ([x (in-psek e)]) x))
  (raises/who 'in-psek (in-psek e))
  (raises/who 'in-esek (for/list ([x (in-esek p)]) x))
  (raises/who 'in-esek (in-esek p)))

(test-case "counts and sources are checked"
  (for ([n (in-list (list -1 3/2 1.0 'x))])
    (raises/who 'make-esek (make-esek n))
    (raises/who 'make-psek (make-psek n))
    (raises/who 'build-psek (build-psek n values))
    (raises/who 'build-esek (build-esek n values))
    (raises/who 'sequence->psek (sequence->psek '(a b) n))
    (raises/who 'sequence->esek (sequence->esek '(a b) n)))
  (raises/who 'sequence->psek (sequence->psek (box 1)))
  (raises/who 'sequence->esek (sequence->esek (box 1))))

(test-case "the generic operations name themselves in errors"
  (for ([s (in-list (list p e))])
    (raises/who 'sek-ref (sek-ref s 4))
    (raises/who 'sek-ref (sek-ref s 'x))
    (raises/who 'sek-take (sek-take s 4))
    (raises/who 'sek-drop (sek-drop s -1)))
  (for ([s (in-list (list empty-psek (make-esek)))])
    (raises/who 'sek-first (sek-first s))
    (raises/who 'sek-last (sek-last s)))
  (check-exn #rx"expected: sek-iter[?]" (lambda () (sek-iter-get 'wrong))))

;; sek-append* empties ephemeral arguments as it goes, so a bad element must
;; be found before the first of them is touched.
(test-case "sek-append* checks every element before consuming any"
  (define inner (esek 1 2))
  (define outer (esek inner 7))
  (raises/who 'sek-append* (sek-append* outer))
  (check-equal? (esek->list inner) '(1 2))
  (check-equal? (esek-length outer) 2)
  (define self (esek))
  (esek-add! self self)
  (raises/who 'sek-append* (sek-append* self))
  (check-equal? (esek-length self) 1))

(module+ test
  (void))
