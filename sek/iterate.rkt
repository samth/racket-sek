#lang racket/base
;; Pull-based traversal of a Sek tree.
;;
;; The paper mentions iterators (§5.4) but omits their design for space; what
;; is provided here is the minimum that makes `for` over a sequence run in
;; O(n) without materializing a list: a reader that walks the tree left to
;; right, holding one frame per level of descent.
;;
;; A frame is a chunk together with the depth of its items and the position
;; reached in it.  Expanding a level pushes its front chunk, its middle tree
;; and its back chunk; expanding an item of depth d > 0 pushes that item, which
;; is itself a chunk of depth d-1 items.

(require "chunk.rkt"
         "ptree.rkt")

(provide done
         make-reader
         reader->sequence)

;; Returned by a reader once it is exhausted.  A private value, so that a
;; sequence element may be anything at all, including eof.
(define done (string->uninterned-symbol "sek-end-of-iteration"))

;; sources : a list of either (cons tree depth) or (cons chunk depth), read in
;; order.  Returns a thunk producing successive elements and then `done`.
(define (make-reader sources)
  (define stack
    (for/list ([src (in-list sources)])
      (if (or (not (car src)) (lvl? (car src)))
          (vector 'tree (car src) (cdr src) 0)
          (vector 'chunk (car src) (cdr src) 0))))
  (define (pop!)
    (set! stack (cdr stack)))
  (define (next!)
    (cond
      [(null? stack) done]
      [else
       (define f (car stack))
       (cond
         [(eq? (vector-ref f 0) 'tree)
          (define t (vector-ref f 1))
          (define d (vector-ref f 2))
          (pop!)
          (when t
            (set! stack
                  (list* (vector 'chunk (lvl-front t) d 0)
                         (vector 'tree (lvl-middle t) (add1 d) 0)
                         (vector 'chunk (lvl-back t) d 0)
                         stack)))
          (next!)]
         [else
          (define c (vector-ref f 1))
          (define d (vector-ref f 2))
          (define i (vector-ref f 3))
          (cond
            [(>= i (chunk-length c))
             (pop!)
             (next!)]
            [else
             (vector-set! f 3 (add1 i))
             (define x (chunk-ref c i))
             (cond
               [(eqv? d 0) x]
               [else
                ;; x is a chunk of depth d-1 items; descend into it
                (set! stack (cons (vector 'chunk x (sub1 d) 0) stack))
                (next!)])])])]))
  next!)

;; Wrap a reader-producing thunk as a Racket sequence.
(define (reader->sequence make-next!)
  (make-do-sequence
   (lambda ()
     (define next! (make-next!))
     (values (lambda (x) x) (lambda (x) (next!)) (next!) (lambda (x) (not (eq? x done))) #f #f))))
