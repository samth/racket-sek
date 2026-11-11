#lang racket/base
;; Behavior of the public interface that the other files, written mostly
;; against the internal modules, do not reach: the sequence forms as values as
;; well as in for clauses, binary folds over sequences of different lengths,
;; what a comparator's zero means, and traversals that race an update.

(require rackunit
         racket/list
         racket/sequence
         sek)

(define (both xs)
  (list (list->psek xs) (list->esek xs)))

(test-case "in-psek, in-esek and in-sek go either way, as values and as clauses"
  (for ([s (in-list (both '(1 2 3)))])
    (define in-flavor (if (psek? s) in-psek in-esek))
    (check-equal? (sequence->list (in-flavor s)) '(1 2 3))
    (check-equal? (sequence->list (in-flavor s 'forward)) '(1 2 3))
    (check-equal? (sequence->list (in-flavor s 'backward)) '(3 2 1))
    (check-equal? (sequence->list (in-sek s 'backward)) '(3 2 1))
    (check-equal? (if (psek? s)
                      (for/list ([x (in-psek s 'backward)])
                        x)
                      (for/list ([x (in-esek s 'backward)])
                        x))
                  '(3 2 1))
    (check-equal? (for/list ([x (in-sek s 'backward)])
                    x)
                  '(3 2 1))))

;; The reference defines fold_right2 as iter2 backward, so the two sequences
;; are paired from their last elements.
(test-case "sek-fold-right2 pairs the sequences from their ends"
  (define (pairs a b)
    (sek-fold-right2 a b (lambda (x y acc) (cons (list x y) acc)) '()))
  (for* ([a (in-list (both '(1 2 3)))]
         [b (in-list (both '(10 20)))])
    (check-equal? (pairs a b) '((2 10) (3 20)))
    (check-equal? (pairs b a) '((10 2) (20 3)))
    (check-equal? (pairs a (list->psek '())) '())
    (check-equal? (pairs (list->esek '()) b) '()))
  ;; and agrees with a backward binary traversal at every pair of lengths
  (for* ([n (in-range 0 40 7)]
         [m (in-range 0 300 37)])
    (define a (list->psek (range n)))
    (define b (list->esek (range 1000 (+ 1000 m))))
    (define backward '())
    (sek-for-each2 a b (lambda (x y) (set! backward (cons (list x y) backward))) 'backward)
    (check-equal? (pairs a b) backward)))

(test-case "sek-fold-left2 pairs the sequences from their starts"
  (check-equal?
   (sek-fold-left2 (psek 1 2 3) (esek 10 20) (lambda (acc x y) (cons (list x y) acc)) '())
   '((2 20) (1 10))))

;; As in `sort`, elements are ordered by a less-than procedure, and two
;; elements neither of which is less than the other are equal.
(test-case "sek<? orders sequences lexicographically by a less-than procedure"
  (check-true (sek<? (psek 1 2) (psek 1 3) <))
  (check-false (sek<? (psek 1 3) (esek 1 2) <))
  (check-false (sek<? (psek 1 2) (esek 1 2) <))
  (check-true (sek<? (psek 1 2) (psek 1 2 0) <))
  (check-false (sek<? (psek 1 2 0) (psek 1 2) <))
  (check-false (sek<? (psek) (esek) <))
  (check-true (sek<? (psek) (psek 0) <))
  (check-false (sek<? (psek 0) (psek) <))
  (check-false (sek<? (psek "b" "A") (psek "B" "a") string-ci<?))
  (check-false (sek<? (psek "B" "a") (psek "b" "A") string-ci<?))
  (check-true (sek<? (psek 1.0 2) (psek 1 3) <))
  ;; strict, so it can be sort's less-than?, and the sort stays stable
  (check-equal? (map sek->list
                     (sort (list (psek 2) (psek 1 1) (psek 1) (esek 1 1))
                           (lambda (a b) (sek<? a b <))))
                '((1) (1 1) (1 1) (2))))

(test-case "sequence->psek takes at most n elements"
  (check-equal? (sek->list (sequence->psek '(a b) 5)) '(a b))
  (check-equal? (sek->list (sequence->esek '(a b c) 2)) '(a b))
  (check-equal? (sek->list (sequence->psek (in-naturals) 4)) '(0 1 2 3)))

;; An update to an ephemeral sequence during a traversal is an error, which
;; the traversal reports before it hands over another element.  A traversal
;; that read ahead and did not check would show the update, and after a pop
;; could hand over the private value left in the emptied slot.
(define-syntax-rule (traversals (e see!) [who expr] ...)
  (list (list 'who (lambda (e see!) expr)) ...))

(define all-traversals
  (traversals
   (e see!)
   [in-esek (for ([x (in-esek e)]) (see! x))]
   [in-esek (for ([x (in-esek e 'backward)]) (see! x))]
   [in-sek (for ([x (in-sek e)]) (see! x))]
   [in-esek (let ([s (in-esek e)]) (for ([x s]) (see! x)))]
   [in-sek (let ([s (in-sek e)]) (for ([x s]) (see! x)))]
   [in-esek (for ([x e]) (see! x))]
   [esek-for-each (esek-for-each e see!)]
   [sek-for-each (sek-for-each e see!)]
   [sek-for-each (sek-for-each e see! 'backward)]
   [sek-for-each/index (sek-for-each/index e (lambda (i x) (see! x)))]
   [sek-fold-left (sek-fold-left e (lambda (a x) (see! x) a) 0)]
   [sek-fold-right (sek-fold-right e (lambda (x a) (see! x) a) 0)]
   [sek-map (sek-map e (lambda (x) (see! x) x))]
   [sek-map/index (sek-map/index e (lambda (i x) (see! x) x))]
   [sek-filter (sek-filter e (lambda (x) (see! x) #t))]
   [sek-filter-map (sek-filter-map e (lambda (x) (see! x) x))]
   [sek-partition (sek-partition e (lambda (x) (see! x) #t))]
   [sek-append-map (sek-append-map e (lambda (x) (see! x) (psek x)))]
   [sek-uniq (sek-uniq e (lambda (a b) (see! b) #f))]
   [sek-find (sek-find e (lambda (x) (see! x) #f))]
   [sek-find-index (sek-find-index e (lambda (x) (see! x) #f))]
   [sek-find-map (sek-find-map e (lambda (x) (see! x) #f))]
   [sek-for-all? (sek-for-all? e (lambda (x) (see! x) #t))]
   [sek-exists? (sek-exists? e (lambda (x) (see! x) #f))]
   [sek-member? (sek-member? e 'z (lambda (v x) (see! x) #f))]
   [sek-index-of (sek-index-of e 'z (lambda (v x) (see! x) #f))]
   [sek-for-each2 (sek-for-each2 e (list->esek (range 300)) (lambda (x y) (see! x)))]
   [sek-fold-left2 (sek-fold-left2 e (list->psek (range 300)) (lambda (a x y) (see! x) a) 0)]
   [sek-fold-right2 (sek-fold-right2 e (list->psek (range 300)) (lambda (x y a) (see! x) a) 0)]
   [sek-for-all2? (sek-for-all2? e (list->psek (range 300)) (lambda (x y) (see! x) #t))]
   [sek-exists2? (sek-exists2? e (list->psek (range 300)) (lambda (x y) (see! x) #f))]
   [sek-equal? (sek-equal? e (list->psek (range 300)) (lambda (x y) (see! x) #t))]
   [sek-map2 (sek-map2 e (list->psek (range 300)) (lambda (x y) (see! x) x))]
   ;; less-than? is called twice for each pair of equal elements, once each
   ;; way, so it sees each element of e on the first of the two calls
   [sek<? (let ([first-call? #f])
            (sek<? e (list->psek (range 300))
                   (lambda (x y)
                     (set! first-call? (not first-call?))
                     (when first-call? (see! x))
                     #f)))]
   [sek-merge (sek-merge e (list->psek (range 300)) (lambda (x y) (see! x) #f))]
   [esek-map! (esek-map! e (lambda (x) (see! x) x))]
   [esek-map/index! (esek-map/index! e (lambda (i x) (see! x) x))]))

(test-case "a traversal reports an update before it hands over another element"
  (for* ([t (in-list all-traversals)]
         [update (in-list (list (lambda (e) (esek-pop-back! e))
                                (lambda (e) (esek-pop-front! e))
                                (lambda (e) (esek-set! e 150 'changed))
                                (lambda (e) (esek-add! e 'added))
                                (lambda (e) (esek-clear! e))))])
    (define who (car t))
    (define e (list->esek (range 300)))
    (define seen '())
    (define (see! x)
      (when (null? seen) (update e))
      (set! seen (cons x seen)))
    (check-exn (lambda (x)
                 (and (exn:fail:contract? x)
                      (regexp-match? (string-append "^" (regexp-quote (symbol->string who)) ":")
                                     (exn-message x))))
               (lambda () ((cadr t) e see!))
               (format "~a" who))
    (check-equal? (length seen) 1 (format "~a" who))))

(test-case "reading is not an update"
  (define e (list->esek (range 1000)))
  (check-equal? (for/sum ([x e]) x) (for/sum ([x (in-range 1000)]) x))
  (check-equal? (for/sum ([x (in-esek e)] [y e]) (- x y)) 0)
  (check-equal? (sek-fold-left e (lambda (a x) (esek-snapshot e) (+ a x)) 0) 499500)
  (check-equal? (sek-map e (lambda (x) (esek-ref e x))) e))

(test-case "esek-map! and esek-map/index! update in place"
  (for ([n (in-list '(0 1 10 300 100000))])
    (define e (list->esek (range n)))
    (define it (sek-iterator e))
    (esek-map! e (lambda (x) (* 2 x)))
    (check-equal? (esek->list e) (for/list ([i (in-range n)]) (* 2 i)))
    (esek-map/index! e (lambda (i x) (+ x i)))
    (check-equal? (esek->list e) (for/list ([i (in-range n)]) (* 3 i)))
    ;; an update, so it invalidates other iterators
    (unless (zero? n)
      (check-exn exn:fail:contract? (lambda () (sek-iter-get it))))))

;; The transient pattern: edit a persistent sequence, update it in place,
;; take a snapshot.  The original is never touched.
(test-case "esek-map! on an edited sequence leaves the original alone"
  (define s (for/psek ([i (in-range 1000)]) i))
  (define e (psek-edit s))
  (esek-map/index! e (lambda (i x) (+ x i)))
  (check-equal? (psek->list (esek-snapshot e)) (for/list ([i (in-range 1000)]) (* 2 i)))
  (check-equal? (psek->list s) (range 1000)))

;; A snapshot taken by proc is a read, but it takes ownership of e's chunks
;; away: the rest of the map must copy them rather than write into the
;; snapshot.
(test-case "a snapshot taken during esek-map! keeps its contents"
  (define e (list->esek (range 1000)))
  (define snap #f)
  (esek-map! e (lambda (x)
                 (when (= x 150) (set! snap (esek-snapshot e)))
                 (* 2 x)))
  (check-equal? (esek->list e) (for/list ([i (in-range 1000)]) (* 2 i)))
  (check-equal? (psek->list snap)
                (append (for/list ([i (in-range 150)]) (* 2 i)) (range 150 1000))))

;; Lengths on either side of the short-sequence threshold (32) and of one
;; chunk (128), where psek-map and vector->psek take different paths.
(define conversion-lengths '(0 1 31 32 33 127 128 129 1000 100000))

(test-case "psek-map calls its procedure on each element, first to last"
  (for ([n (in-list conversion-lengths)])
    (define calls '())
    (define r (psek-map (list->psek (range n))
                        (lambda (x) (set! calls (cons x calls)) (* 2 x))))
    (check-equal? (psek->list r) (for/list ([i (in-range n)]) (* 2 i)))
    (check-equal? (reverse calls) (range n))))

(test-case "vector->psek copies the vector"
  (for ([n (in-list conversion-lengths)])
    (define v (build-vector n values))
    (define p (vector->psek v))
    (unless (zero? n) (vector-set! v 0 'changed))
    (check-equal? (psek->list p) (range n))))

;; The searches and the indexed traversals walk a run of storage at a time and
;; count indices as they go, in either direction and across chunk boundaries.
(test-case "indices and searches agree with a list, in both directions"
  (for* ([n (in-list conversion-lengths)]
         [s (in-list (both (range n)))])
    (define forward '())
    (sek-for-each/index s (lambda (i x) (set! forward (cons (cons i x) forward))))
    (check-equal? (reverse forward) (for/list ([i (in-range n)]) (cons i i)))
    (define backward '())
    (sek-for-each/index s (lambda (i x) (set! backward (cons (cons i x) backward))) 'backward)
    (check-equal? backward (for/list ([i (in-range n)]) (cons i i)))
    (check-equal? (sek->list (sek-map/index s (lambda (i x) (- (* 2 i) x)))) (range n))
    (define mid (quotient n 2))
    (define (near? x) (<= (sub1 mid) x (add1 mid)))
    (unless (zero? n)
      (check-equal? (sek-find-index s near?) (max 0 (sub1 mid)))
      (check-equal? (sek-find-index s near? 'backward) (min (sub1 n) (add1 mid)))
      (check-equal? (sek-find s near? 'backward) (min (sub1 n) (add1 mid)))
      (check-equal? (sek-find-map s (lambda (x) (and (= x mid) (list x)))) (list mid))
      (check-equal? (sek-index-of s (sub1 n)) (sub1 n)))
    (check-equal? (sek-find-index s negative?) #f)
    (check-equal? (sek-for-all? s (lambda (x) (< x mid))) (zero? n))
    (check-equal? (sek-exists? s (lambda (x) (= x (sub1 n)))) (not (zero? n)))
    (check-equal? (sek-member? s -1) #f)))

(test-case "the builders give the same sequence at every length"
  (for ([n (in-list conversion-lengths)])
    (define xs (range n))
    (check-equal? (psek->list (list->psek xs)) xs)
    (check-equal? (psek->list (for/psek ([x (in-list xs)]) x)) xs)
    (check-equal? (psek->list (for*/psek ([x (in-list xs)] [y '(0)]) (+ x y))) xs)
    (check-true (psek? (for/psek ([x (in-list xs)]) x)))
    (for ([s (in-list (both xs))])
      (define-values (evens odds) (sek-partition s even?))
      (check-equal? (sek->list evens) (filter even? xs))
      (check-equal? (sek->list odds) (filter odd? xs))
      (check-equal? (psek? evens) (psek? s))
      (check-equal? (sek->list (sek-uniq (sek-map s (lambda (x) (quotient x 3))))) 
                    (remove-duplicates (map (lambda (x) (quotient x 3)) xs)))
      (check-equal? (sek->list (sek-merge s s <)) (sort (append xs xs) <))
      (check-equal? (sek->list (sek-sub s (quotient n 4) (quotient n 2)))
                    (take (drop xs (quotient n 4)) (quotient n 2)))))
  (check-exn #rx"^list->psek: " (lambda () (list->psek 'x))))

(test-case "sek-sort is stable, and leaves its argument alone"
  (for ([s (in-list (both (for/list ([i (in-range 500)]) (cons (modulo (* i 7) 10) i))))])
    (define before (sek->list s))
    (define sorted (sek->list (sek-sort s (lambda (a b) (< (car a) (car b))))))
    (check-equal? sorted (sort before < #:key car))
    (check-equal? (sek->list s) before)))

(test-case "esek-assign! moves the contents"
  (define a (esek 'x))
  (define b (list->esek (range 300)))
  (esek-assign! a b)
  (check-equal? (esek->list a) (range 300))
  (check-equal? (esek->list b) '())
  (esek-add! a 300)
  (esek-add! b 'y)
  (check-equal? (esek-length a) 301)
  (check-equal? (esek->list b) '(y)))

;; The two-sequence operations walk matching runs of both sequences, which
;; end at different places when the lengths or the flavors differ.
(test-case "two-sequence operations agree with lists at every pair of lengths"
  (for* ([n (in-list '(0 1 33 129 700))]
         [m (in-list '(0 5 128 300 700))]
         [a (in-list (both (range n)))]
         [b (in-list (both (range 1000 (+ 1000 m))))])
    (define xs (range n))
    (define ys (range 1000 (+ 1000 m)))
    (define k (min n m))
    (define pairs (map list (take xs k) (take ys k)))
    (define back-pairs (map list (take-right xs k) (take-right ys k)))
    (define seen '())
    (sek-for-each2 a b (lambda (x y) (set! seen (cons (list x y) seen))))
    (check-equal? (reverse seen) pairs)
    (set! seen '())
    (sek-for-each2 a b (lambda (x y) (set! seen (cons (list x y) seen))) 'backward)
    (check-equal? seen back-pairs)
    (check-equal? (sek->list (sek-map2 a b list)) pairs)
    (check-equal? (sek-fold-left2 a b (lambda (acc x y) (cons (list x y) acc)) '()) (reverse pairs))
    (check-equal? (sek-fold-right2 a b (lambda (x y acc) (cons (list x y) acc)) '()) back-pairs)
    (check-equal? (sek-for-all2? a b <) #t)
    (check-equal? (sek-exists2? a b (lambda (x y) (= (+ x 1000) y (+ 1000 (sub1 k))))) (> k 0))
    (check-equal? (sek-equal? a b) (= n m 0))
    (check-equal? (sek<? a b <) (> m 0))
    (check-equal? (sek<? b a <) (and (= m 0) (> n 0)))
    (check-equal? (sek-equal? a (list->esek xs)) #t)
    (check-equal? (sek<? a (list->psek xs) <) #f)))

(test-case "a sequence used directly, equal? and hashing see every element"
  (for ([n (in-list conversion-lengths)])
    (define xs (range n))
    (for ([s (in-list (both xs))])
      (check-equal? (for/list ([x s]) x) xs)
      (check-equal? (sequence->list s) xs)
      (define same (if (psek? s) (for/psek ([x (in-list xs)]) x) (for/esek ([x (in-list xs)]) x)))
      (check-true (equal? s same))
      (check-equal? (equal-hash-code s) (equal-hash-code same))
      (unless (zero? n)
        (check-false (equal? s (sek-map same (lambda (x) (if (= x (sub1 n)) 'other x)))))))))

;; On an ephemeral sequence these copy, so the argument keeps its contents
;; and the ownership of its chunks.
(test-case "sek-take, sek-drop, sek-insert and sek-delete build a new sequence"
  (for* ([n (in-list conversion-lengths)]
         [s (in-list (both (range n)))]
         [i (in-list (remove-duplicates (list 0 (quotient n 3) (quotient n 2) n)))])
    (define xs (range n))
    (check-equal? (sek->list (sek-take s i)) (take xs i))
    (check-equal? (sek->list (sek-drop s i)) (drop xs i))
    (check-equal? (sek->list (sek-take-right s i)) (take-right xs i))
    (check-equal? (sek->list (sek-drop-right s i)) (drop-right xs i))
    (check-equal? (sek->list (sek-insert s i 'new)) (append (take xs i) '(new) (drop xs i)))
    (when (< i n)
      (check-equal? (sek->list (sek-delete s i)) (append (take xs i) (drop xs (add1 i)))))
    (check-equal? (psek? (sek-insert s i 'new)) (psek? s))
    (check-equal? (sek->list s) xs)))

(test-case "the ephemeral constructors agree at every length"
  (for ([n (in-list conversion-lengths)])
    (define xs (range n))
    (for ([e (in-list (list (list->esek xs) (build-esek n values) (sequence->esek xs)
                            (for/esek ([x (in-list xs)]) x)))])
      (check-equal? (esek->list e) xs)
      (esek-add! e 'end)
      (esek-cons! e 'front)
      (unless (zero? n) (esek-set! e (add1 (quotient n 2)) (quotient n 2)))
      (check-equal? (esek->list e) (append '(front) xs '(end))))
    (define m (make-esek n 'x))
    (check-equal? (esek->list m) (make-list n 'x))
    (unless (zero? n)
      (esek-set! m (sub1 n) 'y)
      (check-equal? (esek-last m) 'y)))
  (check-exn #rx"^list->esek: " (lambda () (list->esek 'x))))
