#lang racket/base
;; The derived operations, checked against the corresponding list operations.

(require rackunit
         racket/list
         racket/vector
         sek/private/config
         (except-in sek/private/persistent in-psek)
         (except-in sek/private/ephemeral in-esek)
         sek/private/generic
         sek/private/segment
         sek/private/check)

(provide run-generic-tests)

(define (as-list s)
  (if (psek? s)
      (psek->list s)
      (esek->list s)))

(define (validate s)
  (if (psek? s)
      (check-psek s)
      (check-esek s))
  s)

;; Every producer must return the same flavor it was given.
(define (check-same-flavor s r where)
  (unless (eq? (psek? s) (psek? r))
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
    (define e (list->esek xs))
    (esek-fill! e 'z 1 (- n 1))
    (check-esek e)
    (check-equal? (esek->list e)
                  (append (list (car xs)) (build-list (- n 2) (lambda (_) 'z)) (list (last xs)))))
  ;; blit! between two sequences
  (when (> n 3)
    (define src (list->psek (map (lambda (x) (list 'src x)) xs)))
    (define dst (list->esek xs))
    (esek-copy! dst 0 src 1 (- n 1))
    (check-esek dst)
    (check-equal? (esek->list dst)
                  (append (take (drop (map (lambda (x) (list 'src x)) xs) 1) (- n 2))
                          (drop xs (- n 2)))))
  ;; blit! within one sequence
  (when (> n 4)
    (define e (list->esek xs))
    (esek-copy! e 2 e 0 (- n 2))
    (check-esek e)
    (check-equal? (esek->list e) (append (take xs 2) (take xs (- n 2))))))

(define (run-generic-tests #:sizes [sizes '(0 1 2 3 7 40 300)])
  (for ([n (in-list sizes)])
    (define xs (build-list n values))
    (run-on xs list->psek)
    (run-on xs list->esek)
    (run-ephemeral-only xs)))

(module+ test
  (for ([cfg (in-list '((2 2 0) (3 2 2) (4 4 4) (8 4 6) (128 16 32)))])
    (sek-configure! #:leaf-capacity (car cfg)
                    #:node-capacity (cadr cfg)
                    #:short-threshold (caddr cfg))
    (run-generic-tests))
  (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32)

  ;; constructors
  (check-equal? (psek->list (build-psek 5 (lambda (i) (* i i)))) '(0 1 4 9 16))
  (check-equal? (esek->list (build-esek 4 add1)) '(1 2 3 4))
  (check-equal? (psek->list (make-psek 3 'a)) '(a a a))
  (check-equal? (esek->list (make-esek 3 'b)) '(b b b))
  (check-equal? (psek->list (sequence->psek (in-range 4))) '(0 1 2 3))
  (check-equal? (esek->list (sequence->esek "abc")) '(#\a #\b #\c))

  ;; assign moves the contents across and empties the source
  (define e1 (list->esek '(1 2 3)))
  (define e2 (list->esek '(9 8)))
  (esek-assign! e1 e2)
  (check-equal? (esek->list e1) '(9 8))
  (check-equal? (esek->list e2) '())

  ;; comprehensions
  (check-equal? (esek->list (for/esek ([i (in-range 5)]) (* i i))) '(0 1 4 9 16))
  (check-equal? (psek->list (for/psek ([i (in-range 3)]) i)) '(0 1 2))
  (check-equal? (esek->list (for*/esek ([i 2] [j 2]) (list i j)))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (psek->list (for*/psek ([i 2] [j 2]) (+ i j))) '(0 1 1 2))

  ;; flatten consumes its argument and its elements, as the reference's does
  (define f1 (list->esek '(1 2)))
  (define f2 (list->esek '(3)))
  (define outer (list->esek (list f1 f2)))
  (check-equal? (esek->list (sek-append* outer)) '(1 2 3))
  (check-equal? (esek->list f1) '())
  (check-equal? (esek->list f2) '())
  (check-equal? (esek->list outer) '())
  ;; ... but the persistent one does not
  (define p1 (list->psek '(1 2)))
  (define pouter (list->psek (list p1 (list->psek '(3)))))
  (check-equal? (psek->list (sek-append* pouter)) '(1 2 3))
  (check-equal? (psek->list p1) '(1 2))
  (check-equal? (psek-length pouter) 2)

  ;; segment-wise binary traversal must agree with the element-wise one
  (for ([n (in-list '(0 1 5 40 300))])
    (define xs (build-list n values))
    (define ys (build-list n (lambda (i) (* 10 i))))
    (define a (list->psek xs))
    (define b (list->esek ys))
    (define acc '())
    (sek-segments-for-each2 a b
                            (lambda (s1 s2)
                              (check-equal? (segment-length s1) (segment-length s2))
                              (check-true (segment-valid? s1))
                              (segment-for-each2 s1 s2
                                                 (lambda (x y) (set! acc (cons (+ x y) acc))))))
    (check-equal? (reverse acc) (map + xs ys)))

  ;; building from a prefix of any sequence
  (check-equal? (psek->list (sequence->psek (in-range 100) 4)) '(0 1 2 3))
  (check-equal? (esek->list (sequence->esek '(a b c d) 2)) '(a b))

  ;; the in-place structural operations, which consume their arguments
  (define c1 (list->esek '(1 2 3)))
  (define c2 (list->esek '(4 5)))
  (define c3 (esek-concat! c1 c2))
  (check-equal? (esek->list c3) '(1 2 3 4 5))
  (check-equal? (esek->list c1) '())
  (check-equal? (esek->list c2) '())
  (define rest (esek-carve! c3 2 'back))
  (check-equal? (esek->list c3) '(1 2))
  (check-equal? (esek->list rest) '(3 4 5))
  (define front-part (esek-carve! rest 1 'front))
  (check-equal? (esek->list rest) '(4 5))
  (check-equal? (esek->list front-part) '(3))
  (define c4 (list->esek '(1 2 3 4 5)))
  (esek-take! c4 2 'front)
  (check-equal? (esek->list c4) '(1 2))
  (define c5 (list->esek '(1 2 3 4 5)))
  (esek-take! c5 2 'back)
  (check-equal? (esek->list c5) '(3 4 5))
  (define c6 (list->esek '(1 2 3 4 5)))
  (esek-drop! c6 2 'front)
  (check-equal? (esek->list c6) '(3 4 5))
  (define c7 (list->esek '(1 2 3 4 5)))
  (define-values (h t) (esek-split! c7 2))
  (check-equal? (esek->list h) '(1 2))
  (check-equal? (esek->list t) '(3 4 5))
  (check-equal? (esek->list c7) '())
  ;; ... while sek-take and sek-drop leave theirs alone
  (define c8 (list->esek '(1 2 3 4 5)))
  (check-equal? (esek->list (sek-take c8 2)) '(1 2))
  (check-equal? (esek->list (sek-drop c8 2)) '(3 4 5))
  (check-equal? (esek->list c8) '(1 2 3 4 5))

  ;; append at either end, leaving the other sequence alone
  (define ea (list->esek '(3 4)))
  (define eb (list->esek '(1 2)))
  (esek-append! ea eb 'front)
  (check-equal? (esek->list ea) '(1 2 3 4))
  ;; appending an ephemeral sequence empties it
  (check-equal? (esek->list eb) '())
  (esek-append! ea (list->psek '(5)) 'back)
  (check-equal? (esek->list ea) '(1 2 3 4 5))

  ;; snapshot-and-clear
  (define e3 (list->esek '(1 2 3)))
  (define p3 (esek-snapshot-and-clear! e3))
  (check-equal? (psek->list p3) '(1 2 3))
  (check-equal? (esek->list e3) '())

  ;; copy modes are observationally the same
  (for ([mode (in-list '(share copy))])
    (define a (list->esek (build-list 300 values)))
    (define b (esek-copy a #:mode mode))
    (esek-set! b 0 'changed)
    (check-equal? (esek-ref a 0) 0)
    (check-equal? (esek-ref b 0) 'changed)
    (check-equal? (esek->list a) (build-list 300 values)))

  ;; the two settings that the paper exposes
  (sek-configure! #:overwrite-empty-slots? #f)
  (define e5 (list->esek (build-list 400 values)))
  (for ([_ (in-range 200)])
    (esek-pop-back! e5))
  (void (check-esek e5))
  (check-equal? (esek->list e5) (build-list 200 values))
  (sek-configure! #:overwrite-empty-slots? #t)

  (sek-configure! #:check-iterator-validity? #f)
  (define e6 (list->esek '(1 2 3)))
  (check-equal? (sek->list e6) '(1 2 3))
  (sek-configure! #:check-iterator-validity? #t))
