#lang racket/base
;; Segments -- the OCaml library's `Segment` module.
;;
;; A segment is a contiguous run of slots inside one of the arrays that back a
;; sequence: a vector, a start index, and a length.  Because a chunk is a
;; circular buffer, it decomposes into at most two segments.
;;
;; Segments are what make bulk operations fast: an iterator can hand out the
;; whole run of elements it is sitting on, and the caller processes them with
;; a tight vector loop instead of one iterator step per element.  A segment is
;; a *view* into the sequence, not a copy; it stays valid only as long as the
;; iterator that produced it does.

(require racket/vector
         (only-in racket/unsafe/ops unsafe-vector*-ref unsafe-fx+ unsafe-fx-))

;; Compiled in unsafe mode.  Every function here that a caller outside the
;; library can reach checks its arguments explicitly, with `unless` rather
;; than by relying on a struct accessor or a vector reference to raise --
;; in unsafe mode those do not raise, they read whatever is at the offset.
(#%declare #:unsafe)

(provide (rename-out [seg segment])
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
         segment->vector)

(struct seg (vector start length)
  #:authentic #:sealed
  #:reflection-name 'segment
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (write-string "#<segment:" port)
     (for ([x (in-segment s)])
       (write-string " " port)
       (write x port))
     (write-string ">" port))])


;; Argument checking is explicit here, because this module is compiled in
;; unsafe mode: a struct accessor no longer raises on the wrong kind of
;; value, it reads whatever happens to be at that offset.
(define-syntax-rule (check-segment who v)
  (unless (seg? v) (raise-argument-error who "segment?" v)))

(define segment? seg?)
(define segment-vector seg-vector)
(define segment-start seg-start)
(define segment-length seg-length)

(define (segment-valid? s)
  (and (seg? s)
       (exact-nonnegative-integer? (seg-start s))
       (exact-nonnegative-integer? (seg-length s))
       (<= (+ (seg-start s) (seg-length s)) (vector-length (seg-vector s)))))

(define (segment-empty? s)
  (check-segment 'segment-empty? s)
  (eqv? 0 (seg-length s)))

(define (segment-ref s i)
  (check-segment 'segment-ref s)
  (unless (and (exact-nonnegative-integer? i) (< i (seg-length s)))
    (raise-arguments-error 'segment-ref "index out of range" "index" i "length" (seg-length s)))
  (vector-ref (seg-vector s) (+ (seg-start s) i)))

;; Writing through a segment writes into the sequence itself.  Only a segment
;; obtained from a writable iterator on an ephemeral sequence may be written.
(define (segment-set! s i x)
  (check-segment 'segment-set! s)
  (unless (and (exact-nonnegative-integer? i) (< i (seg-length s)))
    (raise-arguments-error 'segment-set! "index out of range" "index" i "length" (seg-length s)))
  (vector-set! (seg-vector s) (+ (seg-start s) i) x))

(define (segment-for-each s proc [dir 'forward])
  (check-segment 'segment-for-each s)
  (define v (seg-vector s))
  (define i (seg-start s))
  (define n (seg-length s))
  (if (eq? dir 'forward)
      (for ([j (in-range i (unsafe-fx+ i n))])
        (proc (unsafe-vector*-ref v j)))
      (for ([j (in-range (unsafe-fx- (unsafe-fx+ i n) 1) (unsafe-fx- i 1) -1)])
        (proc (unsafe-vector*-ref v j)))))

(define (segment-for-each2 s1 s2 proc [dir 'forward])
  (check-segment 'segment-for-each2 s1)
  (check-segment 'segment-for-each2 s2)
  (define n (min (seg-length s1) (seg-length s2)))
  (define v1 (seg-vector s1))
  (define v2 (seg-vector s2))
  (define o1 (seg-start s1))
  (define o2 (seg-start s2))
  (if (eq? dir 'forward)
      (for ([j (in-range n)])
        (proc (vector-ref v1 (+ o1 j)) (vector-ref v2 (+ o2 j))))
      (for ([j (in-range (sub1 n) -1 -1)])
        (proc (vector-ref v1 (+ o1 j)) (vector-ref v2 (+ o2 j))))))

(define (in-segment s)
  (check-segment 'in-segment s)
  (in-vector (seg-vector s) (seg-start s) (+ (seg-start s) (seg-length s))))

(define (segment->list s)
  (check-segment 'segment->list s)
  (for/list ([x (in-segment s)])
    x))

(define (segment->vector s)
  (check-segment 'segment->vector s)
  (vector-copy (seg-vector s) (seg-start s) (+ (seg-start s) (seg-length s))))
