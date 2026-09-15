#lang racket/base
;; Randomized testing of the first-class iterators against a model that is
;; just a list and an integer position in [-1, n].

(require rackunit
         racket/list
         sek/config
         sek/persistent
         sek/ephemeral
         sek/iterator
         sek/segment)

(provide run-iterator-random-tests)

(define (walk it dir)
  (let loop ([acc '()])
    (if (sek-iter-finished? it)
        (reverse acc)
        (let ([x (sek-iter-get-and-move! it dir)]) (loop (cons x acc))))))

;; Traverse by segments rather than element by element; must give the same
;; answer, and is the path that the bulk operations take.
(define (walk-by-segments it dir)
  (let loop ([acc '()])
    (if (sek-iter-finished? it)
        (reverse acc)
        (let* ([s (sek-iter-segment-and-jump! it dir)]
               [xs (segment->list s)])
          (loop (append (reverse (if (eq? dir 'forward)
                                     xs
                                     (reverse xs)))
                        acc))))))

(define (check-traversals s xs)
  (check-equal? (walk (sek-iterator s 'forward) 'forward) xs)
  (check-equal? (walk (sek-iterator s 'backward) 'backward) (reverse xs))
  (check-equal? (walk-by-segments (sek-iterator s 'forward) 'forward) xs)
  (check-equal? (walk-by-segments (sek-iterator s 'backward) 'backward) (reverse xs)))

(define (random-cursor-walk s xs seed)
  (random-seed seed)
  (define n (length xs))
  (define it (sek-iterator-at-sentinel s 'front))
  ;; model position: -1 .. n
  (let loop ([pos -1]
             [steps 0])
    (when (< steps 200)
      (define op (random 8))
      (define pos*
        (case op
          [(0 1 2)
           (cond
             [(= pos n) pos]
             [else
              (sek-iter-move! it 'forward)
              (add1 pos)])]
          [(3 4)
           (cond
             [(= pos -1) pos]
             [else
              (sek-iter-move! it 'backward)
              (sub1 pos)])]
          [(5)
           (define target (- (random (+ n 2)) 1))
           (sek-iter-reach! it target)
           target]
          [(6)
           (cond
             [(or (= pos -1) (= pos n)) pos]
             [else
              (define room (- n pos))
              (define k (random (add1 room)))
              (sek-iter-jump! it 'forward k)
              (+ pos k)])]
          [else
           (cond
             [(or (= pos -1) (= pos n)) pos]
             [else
              (define k (random (+ pos 2)))
              (sek-iter-jump! it 'backward k)
              (- pos k)])]))
      (sek-iter-check it)
      (unless (= (sek-iter-index it) pos*)
        (error 'iterator-tests "op ~a: index ~a, expected ~a" op (sek-iter-index it) pos*))
      (define done? (or (= pos* -1) (= pos* n)))
      (unless (eq? (sek-iter-finished? it) done?)
        (error 'iterator-tests "op ~a: finished? ~a, expected ~a" op (sek-iter-finished? it) done?))
      (unless done?
        (unless (equal? (sek-iter-get it) (list-ref xs pos*))
          (error 'iterator-tests
                 "op ~a: get ~s at ~a, expected ~s"
                 op
                 (sek-iter-get it)
                 pos*
                 (list-ref xs pos*)))
        ;; a segment must be a prefix of what remains, in either direction
        (let* ([sg (sek-iter-segment it 'forward)]
               [got (segment->list sg)])
          (unless (equal? got (take (drop xs pos*) (length got)))
            (error 'iterator-tests "forward segment ~s at ~a" got pos*)))
        (let* ([sg (sek-iter-segment it 'backward)]
               [got (segment->list sg)])
          (unless (equal? got (take (drop xs (- pos* (sub1 (length got)))) (length got)))
            (error 'iterator-tests "backward segment ~s at ~a" got pos*))))
      (loop pos* (add1 steps)))))

(define (run-iterator-random-tests #:seed [seed 1] #:sizes [sizes '(0 1 2 5 17 60 300)])
  (for ([n (in-list sizes)])
    (define xs (build-list n (lambda (i) (list 'x i))))
    (define p (list->pseq xs))
    (define e (list->eseq xs))
    (check-traversals p xs)
    (check-traversals e xs)
    (random-cursor-walk p xs (+ seed n))
    (random-cursor-walk e xs (+ seed n 1000))
    ;; a sequence that has been split and rejoined has partially filled chunks,
    ;; which exercises the two-segments-per-chunk case
    (when (> n 3)
      (define-values (a b) (pseq-split p (quotient n 3)))
      (define q (pseq-append b a))
      (define ys (append (drop xs (quotient n 3)) (take xs (quotient n 3))))
      (check-traversals q ys)
      (random-cursor-walk q ys (+ seed n 2000)))))

(module+ test
  (for ([cfg (in-list '((2 2 0) (3 2 2) (4 4 4) (8 4 6) (128 16 32)))])
    (sek-configure! #:leaf-capacity (car cfg)
                    #:node-capacity (cadr cfg)
                    #:short-threshold (caddr cfg))
    (for ([seed (in-range 1 4)])
      (run-iterator-random-tests #:seed seed)))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  ;; writing through an iterator
  (define e (list->eseq (build-list 500 values)))
  (define it (sek-iterator e 'forward))
  (for ([i (in-range 500)])
    (sek-iter-set! it (- i))
    (sek-iter-move! it 'forward))
  (check-equal? (eseq->list e) (build-list 500 -))

  ;; ... and the write must not disturb a snapshot taken beforehand
  (define e2 (list->eseq (build-list 300 values)))
  (define snap (eseq-snapshot e2))
  (define it2 (sek-iterator e2 'forward))
  (for ([i (in-range 300)])
    (sek-iter-set! it2 'z)
    (sek-iter-move! it2 'forward))
  (check-equal? (pseq->list snap) (build-list 300 values))
  (check-equal? (eseq->list e2) (build-list 300 (lambda (_) 'z)))

  ;; writable segments
  (define e3 (list->eseq (build-list 400 values)))
  (let loop ([it (sek-iterator e3 'forward)])
    (unless (sek-iter-finished? it)
      (define sg (sek-iter-writable-segment it 'forward))
      (for ([i (in-range (segment-length sg))])
        (segment-set! sg i (* 2 (segment-ref sg i))))
      (sek-iter-jump! it 'forward (segment-length sg))
      (loop it)))
  (check-equal? (eseq->list e3) (build-list 400 (lambda (i) (* 2 i))))

  ;; the option-flavoured operations return #f at a sentinel instead of raising
  (define e7 (list->eseq '(1 2 3)))
  (define it8 (sek-iterator e7 'forward))
  (check-true (segment? (sek-iter-segment* it8 'forward)))
  (check-true (segment? (sek-iter-writable-segment* it8 'forward)))
  (sek-iter-reach! it8 3)
  (check-false (sek-iter-segment* it8 'forward))
  (check-false (sek-iter-segment-and-jump*! it8 'forward))
  (check-false (sek-iter-writable-segment* it8 'forward))
  (check-false (sek-iter-writable-segment-and-jump*! it8 'forward))
  (check-false (sek-iter-get* it8))

  ;; set-and-move, and sweeping with writable segments
  (define e8 (list->eseq (build-list 300 values)))
  (let loop ([it (sek-iterator e8 'forward)])
    (unless (sek-iter-finished? it)
      (sek-iter-set-and-move! it (- (sek-iter-get it)) 'forward)
      (loop it)))
  (check-equal? (eseq->list e8) (build-list 300 -))
  (let loop ([it (sek-iterator e8 'forward)])
    (define sg (sek-iter-writable-segment-and-jump*! it 'forward))
    (when sg
      (for ([i (in-range (segment-length sg))])
        (segment-set! sg i (abs (segment-ref sg i))))
      (loop it)))
  (check-equal? (eseq->list e8) (build-list 300 values))

  ;; invalidation
  (define e4 (list->eseq '(1 2 3)))
  (define it4 (sek-iterator e4 'forward))
  (check-equal? (sek-iter-get it4) 1)
  (eseq-push-back! e4 4)
  (check-false (sek-iter-valid? it4))
  (check-exn exn:fail? (lambda () (sek-iter-get it4)))
  (sek-iter-reset! it4 'forward)
  (check-true (sek-iter-valid? it4))
  (check-equal? (walk it4 'forward) '(1 2 3 4))

  ;; a persistent iterator is never invalidated
  (define p5 (list->pseq '(1 2 3)))
  (define it5 (sek-iterator p5 'forward))
  (check-true (sek-iter-valid? it5))
  (check-equal? (sek-iter-get it5) 1)
  ;; copies are independent
  (define it6 (sek-iter-copy it5))
  (sek-iter-move! it6 'forward)
  (check-equal? (sek-iter-get it5) 1)
  (check-equal? (sek-iter-get it6) 2)
  (check-exn exn:fail? (lambda () (sek-iter-set! it5 'no)))

  ;; moving past a sentinel is an error
  (define it7 (sek-iterator-at-sentinel (list->pseq '(1 2)) 'front))
  (check-exn exn:fail? (lambda () (sek-iter-move! it7 'backward)))
  (check-exn exn:fail? (lambda () (sek-iter-get it7))))
