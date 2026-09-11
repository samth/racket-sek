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

(require racket/vector)

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
  (eqv? 0 (seg-length s)))

(define (segment-ref s i)
  (unless (and (exact-nonnegative-integer? i) (< i (seg-length s)))
    (raise-arguments-error 'segment-ref "index out of range" "index" i "length" (seg-length s)))
  (vector-ref (seg-vector s) (+ (seg-start s) i)))

;; Writing through a segment writes into the sequence itself.  Only a segment
;; obtained from a writable iterator on an ephemeral sequence may be written.
(define (segment-set! s i x)
  (unless (and (exact-nonnegative-integer? i) (< i (seg-length s)))
    (raise-arguments-error 'segment-set! "index out of range" "index" i "length" (seg-length s)))
  (vector-set! (seg-vector s) (+ (seg-start s) i) x))

(define (segment-for-each s proc [dir 'forward])
  (define v (seg-vector s))
  (define i (seg-start s))
  (define n (seg-length s))
  (if (eq? dir 'forward)
      (for ([j (in-range i (+ i n))])
        (proc (vector-ref v j)))
      (for ([j (in-range (sub1 (+ i n)) (sub1 i) -1)])
        (proc (vector-ref v j)))))

(define (segment-for-each2 s1 s2 proc [dir 'forward])
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
  (in-vector (seg-vector s) (seg-start s) (+ (seg-start s) (seg-length s))))

(define (segment->list s)
  (for/list ([x (in-segment s)])
    x))

(define (segment->vector s)
  (vector-copy (seg-vector s) (seg-start s) (+ (seg-start s) (seg-length s))))
