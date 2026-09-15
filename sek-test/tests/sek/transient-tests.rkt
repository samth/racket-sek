#lang racket/base
;; Differential random testing of the *transient* interface: a pool holding a
;; mixture of ephemeral and persistent sequences, many of which share internal
;; structure because they descend from one another by snapshot and edit.
;;
;; The point of checking the whole pool after every single step is that this is
;; exactly where a mistake in the ownership discipline shows up: an in-place
;; update performed on a chunk that some snapshot still observes corrupts that
;; snapshot, and nothing else notices.

(require rackunit
         racket/list
         sek/config
         sek/persistent
         sek/ephemeral
         sek/check)

(provide run-transient-random-tests)

;; kind is 'p or 'e; seq is a pseq or an eseq; list is the reference
(struct entry (kind seq list) #:transparent)

(define (entry->list en)
  (if (eq? (entry-kind en) 'p)
      (pseq->list (entry-seq en))
      (eseq->list (entry-seq en))))

(define (entry-len en)
  (if (eq? (entry-kind en) 'p)
      (pseq-length (entry-seq en))
      (eseq-length (entry-seq en))))

(define (check-entry en where)
  (if (eq? (entry-kind en) 'p)
      (check-pseq (entry-seq en))
      (check-eseq (entry-seq en)))
  (unless (equal? (entry->list en) (entry-list en))
    (error 'transient-tests "~a: candidate ~s, reference ~s" where (entry->list en) (entry-list en)))
  (let ([ys (if (eq? (entry-kind en) 'p)
                (for/list ([x (in-pseq (entry-seq en))]) x)
                (for/list ([x (in-eseq (entry-seq en))]) x))])
    (unless (equal? ys (entry-list en))
      (error 'transient-tests "~a: streaming gave ~s, reference ~s" where ys (entry-list en))))
  (unless (= (entry-len en) (length (entry-list en)))
    (error 'transient-tests
           "~a: length ~a, reference ~a"
           where
           (entry-len en)
           (length (entry-list en))))
  en)

(define (check-pool pool where)
  (for ([en (in-vector pool)])
    (check-entry en where)))

(define counter 0)
(define (next!)
  (set! counter (add1 counter))
  counter)

(define max-length 300)

(define (random-step! pool)
  (define n (vector-length pool))
  (define i (random n))
  (define en (vector-ref pool i))
  (define s (entry-seq en))
  (define xs (entry-list en))
  (define len (length xs))
  (define ephemeral? (eq? (entry-kind en) 'e))
  (define op (random 16))
  (define en*
    (case op
      [(0 1)
       (define x (next!))
       (cond
         [ephemeral?
          (eseq-push-front! s x)
          (entry 'e s (cons x xs))]
         [else (entry 'p (pseq-push-front s x) (cons x xs))])]
      [(2 3 4)
       (define x (next!))
       (cond
         [ephemeral?
          (eseq-push-back! s x)
          (entry 'e s (append xs (list x)))]
         [else (entry 'p (pseq-push-back s x) (append xs (list x)))])]
      [(5 6)
       (cond
         [(zero? len) en]
         [ephemeral?
          (define x (eseq-pop-front! s))
          (unless (equal? x (car xs))
            (error 'transient-tests "eseq pop-front gave ~s, expected ~s" x (car xs)))
          (entry 'e s (cdr xs))]
         [else
          (define-values (x s*) (pseq-pop-front s))
          (unless (equal? x (car xs))
            (error 'transient-tests "pseq pop-front gave ~s, expected ~s" x (car xs)))
          (entry 'p s* (cdr xs))])]
      [(7 8)
       (cond
         [(zero? len) en]
         [ephemeral?
          (define x (eseq-pop-back! s))
          (unless (equal? x (last xs))
            (error 'transient-tests "eseq pop-back gave ~s, expected ~s" x (last xs)))
          (entry 'e s (drop-right xs 1))]
         [else
          (define-values (x s*) (pseq-pop-back s))
          (unless (equal? x (last xs))
            (error 'transient-tests "pseq pop-back gave ~s, expected ~s" x (last xs)))
          (entry 'p s* (drop-right xs 1))])]
      [(9)
       (cond
         [(zero? len) en]
         [else
          (define j (random len))
          (define x (next!))
          (define got
            (if ephemeral?
                (eseq-ref s j)
                (pseq-ref s j)))
          (unless (equal? got (list-ref xs j))
            (error 'transient-tests "ref ~a gave ~s, expected ~s" j got (list-ref xs j)))
          (define ys (append (take xs j) (list x) (drop xs (add1 j))))
          (cond
            [ephemeral?
             (eseq-set! s j x)
             (entry 'e s ys)]
            [else (entry 'p (pseq-set s j x) ys)])])]
      ;; snapshot: the ephemeral sequence stays usable and the result must be
      ;; immune to whatever happens to it next
      [(10 11)
       ;; park the converted version in another slot so that the original and
       ;; the conversion stay live together and keep sharing structure
       (let ([j (random n)])
         (unless (eqv? j i)
           (vector-set! pool j
                        (if ephemeral?
                            (entry 'p (eseq-snapshot s) xs)
                            (entry 'e (pseq-edit s) xs))))
         en)]
      [(12)
       (if ephemeral?
           (entry 'e (eseq-copy s) xs)
           (entry 'p s xs))]
      [(13 14)
       (define j (random n))
       (define other (vector-ref pool j))
       (cond
         [(> (+ len (length (entry-list other))) max-length) en]
         [(and ephemeral? (eqv? i j)) en]
         [ephemeral?
          (eseq-append! s (entry-seq other))
          ;; appending an ephemeral sequence empties it, so the pool's record
          ;; of the other slot has to follow
          (when (eq? (entry-kind other) 'e)
            (vector-set! pool j (entry 'e (entry-seq other) '())))
          (entry 'e s (append xs (entry-list other)))]
         [else
          (define o
            (if (eq? (entry-kind other) 'e)
                (eseq-snapshot (entry-seq other))
                (entry-seq other)))
          (entry 'p (pseq-append s o) (append xs (entry-list other)))])]
      [(15)
       (define j (random (add1 len)))
       (cond
         [ephemeral?
          (define-values (e1 e2) (eseq-split! s j))
          (check-entry (entry 'e e1 (take xs j)) "eseq-split-left")
          (check-entry (entry 'e e2 (drop xs j)) "eseq-split-right")
          (if (zero? (random 2))
              (entry 'e e1 (take xs j))
              (entry 'e e2 (drop xs j)))]
         [else
          (define-values (s1 s2) (pseq-split s j))
          (check-entry (entry 'p s1 (take xs j)) "pseq-split-left")
          (check-entry (entry 'p s2 (drop xs j)) "pseq-split-right")
          (if (zero? (random 2))
              (entry 'p s1 (take xs j))
              (entry 'p s2 (drop xs j)))])]
      [else en]))
  ;; An ephemeral operation mutates in place, so the slot it came from now
  ;; describes the mutated sequence; overwrite it (and occasionally another).
  (vector-set! pool i en*)
  ;; only persistent versions may be duplicated across slots: two slots naming
  ;; one ephemeral sequence would disagree as soon as it is mutated
  (when (and (eq? (entry-kind en*) 'p) (zero? (random 5)))
    (vector-set! pool (random n) en*))
  (check-pool pool (format "op ~a" op)))

(define (run-transient-random-tests #:steps [steps 1000] #:slots [slots 5] #:seed [seed 1])
  (random-seed seed)
  (define pool (make-vector slots #f))
  (for ([i (in-range slots)])
    (vector-set! pool
                 i
                 (if (even? i)
                     (entry 'e (make-eseq) '())
                     (entry 'p empty-pseq '()))))
  (for ([_ (in-range steps)])
    (random-step! pool))
  (void))

(module+ test
  (for ([cfg (in-list '((2 2 0) (3 2 2) (4 4 4) (8 4 6) (16 8 12) (128 16 32)))])
    (sek-configure! #:leaf-capacity (car cfg)
                    #:node-capacity (cadr cfg)
                    #:short-threshold (caddr cfg))
    (for ([seed (in-range 1 4)])
      (run-transient-random-tests #:steps 600 #:seed seed)))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  ;; A snapshot must not be disturbed by later mutation of its source.
  (define e (list->eseq (build-list 500 values)))
  (define snap (eseq-snapshot e))
  (for ([i (in-range 200)])
    (eseq-pop-front! e))
  (for ([i (in-range 200)])
    (eseq-push-back! e (- i)))
  (check-equal? (pseq->list snap) (build-list 500 values))
  (check-equal? (eseq->list e) (append (build-list 300 (lambda (i) (+ i 200))) (build-list 200 -)))

  ;; ... and neither must an edit of a persistent sequence disturb the original
  (define p (list->pseq (build-list 500 values)))
  (define e2 (pseq-edit p))
  (for ([i (in-range 100)])
    (eseq-set! e2 i 'x))
  (check-equal? (pseq->list p) (build-list 500 values))
  (check-equal? (eseq-ref e2 50) 'x))
