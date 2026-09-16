#lang racket/base
;; The derived operations, checked against the corresponding list operations.

(require rackunit
         racket/list
         racket/vector
         sek/config
         (except-in sek/persistent in-pseq)
         (except-in sek/ephemeral in-eseq)
         sek/generic
         sek/segment
         sek/check)

(provide run-generic-tests)

(define (as-list s)
  (if (pseq? s)
      (pseq->list s)
      (eseq->list s)))

(define (validate s)
  (if (pseq? s)
      (check-pseq s)
      (check-eseq s))
  s)

;; Every producer must return the same flavor it was given.
(define (check-same-flavor s r where)
  (unless (eq? (pseq? s) (pseq? r))
    (error 'generic-tests "~a: flavor not preserved" where))
  (validate r))

(define (check-produces s r expected where)
  (check-same-flavor s r where)
  (unless (equal? (as-list r) expected)
    (error 'generic-tests "~a: got ~s, expected ~s" where (as-list r) expected)))

(define (run-on xs build)
  (define s (build xs))
  (define n (length xs))
  (validate s)

  ;; --- consumers
  (check-equal? (sek-length s) n)
  (check-equal? (sek-empty? s) (null? xs))
  (check-equal? (sek->list s) xs)
  (check-equal? (sek->list s 'backward) (reverse xs))
  (check-equal? (sek->vector s) (list->vector xs))
  (check-equal? (for/list ([x (in-sek s)])
                  x)
                xs)
  (check-equal? (for/list ([x (in-sek s 'backward)])
                  x)
                (reverse xs))
  (check-equal? (sek-fold-left s (lambda (acc x) (cons x acc)) '()) (reverse xs))
  (check-equal? (sek-fold-right s cons '()) xs)
  (check-equal? (let ([acc '()])
                  (sek-for-each/index s (lambda (i x) (set! acc (cons (cons i x) acc))))
                  (reverse acc))
                (for/list ([x (in-list xs)]
                           [i (in-naturals)])
                  (cons i x)))
  ;; segments must partition the sequence
  (check-equal? (let ([acc '()])
                  (sek-segments-for-each s (lambda (sg) (set! acc (cons (segment->list sg) acc))))
                  (apply append (reverse acc)))
                xs)

  ;; --- searching
  (define even-elem (findf even? xs))
  (check-equal? (sek-find s even?) even-elem)
  (check-equal? (sek-find s even? 'backward) (findf even? (reverse xs)))
  (check-equal? (sek-find s (lambda (_) #f)) #f)
  (check-equal? (sek-find-index s even?) (index-where xs even?))
  (check-equal? (sek-find-map s (lambda (x) (and (even? x) (list x))))
                (and even-elem (list even-elem)))
  (check-equal? (sek-for-all? s exact-integer?) #t)
  (check-equal? (sek-for-all? s even?) (andmap even? xs))
  (check-equal? (sek-exists? s even?) (and (ormap even? xs) #t))
  (check-equal? (sek-member? s 3) (and (member 3 xs) #t))
  (check-equal? (sek-memq? s 3) (and (memq 3 xs) #t))

  ;; --- producers
  (check-produces s (sek-map s add1) (map add1 xs) 'map)
  (check-produces s
                  (sek-map/index s (lambda (i x) (cons i x)))
                  (for/list ([x (in-list xs)]
                             [i (in-naturals)])
                    (cons i x))
                  'map/index)
  (check-produces s (sek-filter s even?) (filter even? xs) 'filter)
  (check-produces s
                  (sek-filter-map s (lambda (x) (and (even? x) (* 10 x))))
                  (filter-map (lambda (x) (and (even? x) (* 10 x))) xs)
                  'filter-map)
  (check-produces s (sek-reverse s) (reverse xs) 'reverse)
  (let-values ([(a b) (sek-partition s even?)])
    (check-produces s a (filter even? xs) 'partition-yes)
    (check-produces s b (filter odd? xs) 'partition-no))
  (check-produces s (sek-sort s <) (sort xs <) 'sort)
  (check-produces s (sek-uniq (sek-sort s <)) (remove-duplicates (sort xs <)) 'uniq)
  (check-produces s
                  (sek-append-map s (lambda (x) (build (list x x))))
                  (append-map (lambda (x) (list x x)) xs)
                  'append-map)

  ;; --- slicing
  (for ([start (in-list (list 0 (quotient n 3) (max 0 (sub1 n))))])
    (for ([size (in-list (list 0 1 (max 0 (- n start))))])
      (when (<= (+ start size) n)
        (check-produces s
                        (sek-sub s start size)
                        (take (drop xs start) size)
                        (format "sub ~a ~a" start size)))))
  (for ([k (in-list (list 0 1 (quotient n 2) n))])
    (when (<= k n)
      ;; take/drop consume an ephemeral sequence, so work on a copy
      (define s1 (build xs))
      (check-produces s1 (sek-take s1 k) (take xs k) 'take)
      (define s2 (build xs))
      (check-produces s2 (sek-drop s2 k) (drop xs k) 'drop)))

  ;; --- binary operations
  (define ys (map (lambda (x) (* 2 x)) xs))
  (define t (build ys))
  (check-produces s (sek-map2 s t +) (map + xs ys) 'map2)
  (check-produces s (sek-zip s t) (map cons xs ys) 'zip)
  (let-values ([(a b) (sek-unzip (sek-zip (build xs) (build ys)))])
    (check-equal? (as-list a) xs)
    (check-equal? (as-list b) ys))
  (check-equal? (sek-fold-left2 s t (lambda (acc a b) (cons (+ a b) acc)) '())
                (reverse (map + xs ys)))
  (check-equal? (sek-fold-right2 s t (lambda (a b acc) (cons (+ a b) acc)) '()) (map + xs ys))
  (check-equal? (sek-for-all2? s t <=) (andmap <= xs ys))
  (check-equal? (sek-exists2? s t >) (and (ormap > xs ys) #t))
  (check-equal? (sek-equal? s (build xs)) #t)
  (check-equal? (sek-equal? s t) (equal? xs ys))
  (check-equal? (sek-compare s (build xs) (lambda (a b) (- a b))) 0)
  (unless (null? xs)
    (check-equal? (sek-compare s (build (cdr xs)) (lambda (a b) (- a b)))
                  (let loop ([a xs]
                             [b (cdr xs)])
                    (cond
                      [(and (null? a) (null? b)) 0]
                      [(null? a) -1]
                      [(null? b) 1]
                      [(= (car a) (car b)) (loop (cdr a) (cdr b))]
                      [(< (car a) (car b)) -1]
                      [else 1]))))

  ;; --- merge of two sorted sequences
  (check-produces s (sek-merge (sek-sort s <) (sek-sort t <) <) (sort (append xs ys) <) 'merge)

  ;; --- flatten
  (define nested (build (list (build xs) (build ys))))
  (check-equal? (as-list (sek-append* nested)) (append xs ys)))

(define (run-ephemeral-only xs)
  (define n (length xs))
  ;; fill!
  (when (> n 2)
    (define e (list->eseq xs))
    (eseq-fill! e 'z 1 (- n 1))
    (check-eseq e)
    (check-equal? (eseq->list e)
                  (append (list (car xs)) (build-list (- n 2) (lambda (_) 'z)) (list (last xs)))))
  ;; blit! between two sequences
  (when (> n 3)
    (define src (list->pseq (map (lambda (x) (list 'src x)) xs)))
    (define dst (list->eseq xs))
    (eseq-copy! dst 0 src 1 (- n 1))
    (check-eseq dst)
    (check-equal? (eseq->list dst)
                  (append (take (drop (map (lambda (x) (list 'src x)) xs) 1) (- n 2))
                          (drop xs (- n 2)))))
  ;; blit! within one sequence
  (when (> n 4)
    (define e (list->eseq xs))
    (eseq-copy! e 2 e 0 (- n 2))
    (check-eseq e)
    (check-equal? (eseq->list e) (append (take xs 2) (take xs (- n 2))))))

(define (run-generic-tests #:sizes [sizes '(0 1 2 3 7 40 300)])
  (for ([n (in-list sizes)])
    (define xs (build-list n values))
    (run-on xs list->pseq)
    (run-on xs list->eseq)
    (run-ephemeral-only xs)))

(module+ test
  (for ([cfg (in-list '((2 2 0) (3 2 2) (4 4 4) (8 4 6) (128 16 32)))])
    (sek-configure! #:leaf-capacity (car cfg)
                    #:node-capacity (cadr cfg)
                    #:short-threshold (caddr cfg))
    (run-generic-tests))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  ;; constructors
  (check-equal? (pseq->list (build-pseq 5 (lambda (i) (* i i)))) '(0 1 4 9 16))
  (check-equal? (eseq->list (build-eseq 4 add1)) '(1 2 3 4))
  (check-equal? (pseq->list (make-pseq 3 'a)) '(a a a))
  (check-equal? (eseq->list (make-eseq 3 'b)) '(b b b))
  (check-equal? (pseq->list (sequence->pseq (in-range 4))) '(0 1 2 3))
  (check-equal? (eseq->list (sequence->eseq "abc")) '(#\a #\b #\c))

  ;; assign moves the contents across and empties the source
  (define e1 (list->eseq '(1 2 3)))
  (define e2 (list->eseq '(9 8)))
  (eseq-assign! e1 e2)
  (check-equal? (eseq->list e1) '(9 8))
  (check-equal? (eseq->list e2) '())

  ;; comprehensions
  (check-equal? (eseq->list (for/eseq ([i (in-range 5)]) (* i i))) '(0 1 4 9 16))
  (check-equal? (pseq->list (for/pseq ([i (in-range 3)]) i)) '(0 1 2))
  (check-equal? (eseq->list (for*/eseq ([i 2] [j 2]) (list i j)))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (pseq->list (for*/pseq ([i 2] [j 2]) (+ i j))) '(0 1 1 2))

  ;; flatten consumes its argument and its elements, as the reference's does
  (define f1 (list->eseq '(1 2)))
  (define f2 (list->eseq '(3)))
  (define outer (list->eseq (list f1 f2)))
  (check-equal? (eseq->list (sek-append* outer)) '(1 2 3))
  (check-equal? (eseq->list f1) '())
  (check-equal? (eseq->list f2) '())
  (check-equal? (eseq->list outer) '())
  ;; ... but the persistent one does not
  (define p1 (list->pseq '(1 2)))
  (define pouter (list->pseq (list p1 (list->pseq '(3)))))
  (check-equal? (pseq->list (sek-append* pouter)) '(1 2 3))
  (check-equal? (pseq->list p1) '(1 2))
  (check-equal? (pseq-length pouter) 2)

  ;; segment-wise binary traversal must agree with the element-wise one
  (for ([n (in-list '(0 1 5 40 300))])
    (define xs (build-list n values))
    (define ys (build-list n (lambda (i) (* 10 i))))
    (define a (list->pseq xs))
    (define b (list->eseq ys))
    (define acc '())
    (sek-segments-for-each2 a b
                            (lambda (s1 s2)
                              (check-equal? (segment-length s1) (segment-length s2))
                              (check-true (segment-valid? s1))
                              (segment-for-each2 s1 s2
                                                 (lambda (x y) (set! acc (cons (+ x y) acc))))))
    (check-equal? (reverse acc) (map + xs ys)))

  ;; building from a prefix of any sequence
  (check-equal? (pseq->list (sequence->pseq (in-range 100) 4)) '(0 1 2 3))
  (check-equal? (eseq->list (sequence->eseq '(a b c d) 2)) '(a b))

  ;; the in-place structural operations, which consume their arguments
  (define c1 (list->eseq '(1 2 3)))
  (define c2 (list->eseq '(4 5)))
  (define c3 (eseq-concat! c1 c2))
  (check-equal? (eseq->list c3) '(1 2 3 4 5))
  (check-equal? (eseq->list c1) '())
  (check-equal? (eseq->list c2) '())
  (define rest (eseq-carve! c3 2 'back))
  (check-equal? (eseq->list c3) '(1 2))
  (check-equal? (eseq->list rest) '(3 4 5))
  (define front-part (eseq-carve! rest 1 'front))
  (check-equal? (eseq->list rest) '(4 5))
  (check-equal? (eseq->list front-part) '(3))
  (define c4 (list->eseq '(1 2 3 4 5)))
  (eseq-take! c4 2 'front)
  (check-equal? (eseq->list c4) '(1 2))
  (define c5 (list->eseq '(1 2 3 4 5)))
  (eseq-take! c5 2 'back)
  (check-equal? (eseq->list c5) '(3 4 5))
  (define c6 (list->eseq '(1 2 3 4 5)))
  (eseq-drop! c6 2 'front)
  (check-equal? (eseq->list c6) '(3 4 5))
  (define c7 (list->eseq '(1 2 3 4 5)))
  (define-values (h t) (eseq-split! c7 2))
  (check-equal? (eseq->list h) '(1 2))
  (check-equal? (eseq->list t) '(3 4 5))
  (check-equal? (eseq->list c7) '())
  ;; ... while sek-take and sek-drop leave theirs alone
  (define c8 (list->eseq '(1 2 3 4 5)))
  (check-equal? (eseq->list (sek-take c8 2)) '(1 2))
  (check-equal? (eseq->list (sek-drop c8 2)) '(3 4 5))
  (check-equal? (eseq->list c8) '(1 2 3 4 5))

  ;; append at either end, leaving the other sequence alone
  (define ea (list->eseq '(3 4)))
  (define eb (list->eseq '(1 2)))
  (eseq-append! ea eb 'front)
  (check-equal? (eseq->list ea) '(1 2 3 4))
  ;; appending an ephemeral sequence empties it
  (check-equal? (eseq->list eb) '())
  (eseq-append! ea (list->pseq '(5)) 'back)
  (check-equal? (eseq->list ea) '(1 2 3 4 5))

  ;; snapshot-and-clear
  (define e3 (list->eseq '(1 2 3)))
  (define p3 (eseq-snapshot-and-clear! e3))
  (check-equal? (pseq->list p3) '(1 2 3))
  (check-equal? (eseq->list e3) '())

  ;; copy modes are observationally the same
  (for ([mode (in-list '(share copy))])
    (define a (list->eseq (build-list 300 values)))
    (define b (eseq-copy a #:mode mode))
    (eseq-set! b 0 'changed)
    (check-equal? (eseq-ref a 0) 0)
    (check-equal? (eseq-ref b 0) 'changed)
    (check-equal? (eseq->list a) (build-list 300 values)))

  ;; the two settings that the paper exposes
  (sek-configure! #:overwrite-empty-slots? #f)
  (define e5 (list->eseq (build-list 400 values)))
  (for ([_ (in-range 200)])
    (eseq-pop-back! e5))
  (void (check-eseq e5))
  (check-equal? (eseq->list e5) (build-list 200 values))
  (sek-configure! #:overwrite-empty-slots? #t)

  (sek-configure! #:check-iterator-validity? #f)
  (define e6 (list->eseq '(1 2 3)))
  (check-equal? (sek->list e6) '(1 2 3))
  (sek-configure! #:check-iterator-validity? #t))
