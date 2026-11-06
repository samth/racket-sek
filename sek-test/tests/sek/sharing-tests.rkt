#lang racket/base
;; Sequences far longer than memory, built by sharing: appending a sequence to
;; itself doubles its length at the cost of one concatenation.  Such a
;; sequence is deep, so its item weights grow past what the packed-chunk
;; arithmetic may use, and long, so its length meets the limit on length.
;; Nothing here enumerates the elements; each check samples indices.
;;
;; Element i of every sequence here is (modulo i 7).

(require rackunit
         sek
         (submod sek config))

(define (with-settings leaf node threshold thunk)
  (dynamic-wind
   (lambda () (sek-configure! #:leaf-capacity leaf #:node-capacity node #:short-threshold threshold))
   thunk
   (lambda () (sek-configure! #:leaf-capacity 128 #:node-capacity 16 #:short-threshold 32))))


;; Indices spread over the whole length, the ends included.
(define (sample-indices n)
  (append (list 0 1 (quotient n 2) (- n 2) (- n 1))
          (for/list ([z (in-range 1 30)])
            (modulo (* z 137687136247) n))))

(define (check-elements s)
  (define n (psek-length s))
  (define e (psek-edit s))
  (for ([i (in-list (sample-indices n))])
    (check-equal? (psek-ref s i) (modulo i 7))
    (check-equal? (esek-ref e i) (modulo i 7))
    (check-equal? (sek-iter-get (sek-iterator s i)) (modulo i 7)))
  (define-values (a b) (psek-split s (quotient n 3)))
  (check-equal? (+ (psek-length a) (psek-length b)) n)
  (check-equal? (psek-first b) (modulo (quotient n 3) 7)))

;; Doubles a sequence of seven elements until the next doubling would pass
;; the limit, checking elements on the way, and returns the last sequence.
;; The seven are built here, under the settings in force: a sequence built
;; under other settings does not mix with these.
(define (double-to-the-limit)
  (let loop ([s (psek 0 1 2 3 4 5 6)])
    (check-elements s)
    (if (> (* 2 (psek-length s)) (max-length))
        s
        (loop (psek-append s s)))))

;; The limit is the largest power of two that is a fixnum.
(define (max-length)
  (let loop ([n 1])
    (if (fixnum? (* 2 n))
        (loop (* 2 n))
        n)))

(define-syntax-rule (too-long who expr)
  (check-exn (lambda (x)
               (and (exn:fail:contract? x)
                    (regexp-match? (string-append "^" (regexp-quote (symbol->string who))
                                                  ": resulting sequence")
                                   (exn-message x))))
             (lambda () expr)))

(for ([settings (in-list '((2 2 0) (3 3 0) (5 3 2) (128 16 32)))])
  (test-case (format "a doubled sequence, with capacities and threshold ~a" settings)
    (with-settings (car settings)
                   (cadr settings)
                   (caddr settings)
                   (lambda ()
                     (define s (double-to-the-limit))
                     (check-true (> (* 2 (psek-length s)) (max-length)))
                     (too-long 'psek-append (psek-append s s))
                     ;; an ephemeral concatenation checks before it empties anything
                     (define e (psek-edit s))
                     (too-long 'esek-append! (esek-append! e s))
                     (check-equal? (esek-length e) (psek-length s))
                     (define e2 (psek-edit s))
                     (too-long 'esek-concat! (esek-concat! e e2))
                     (check-equal? (esek-length e) (psek-length s))
                     (check-equal? (esek-length e2) (psek-length s))
                     (too-long 'sek-append* (sek-append* (esek e e2)))
                     (check-equal? (esek-length e) (psek-length s))
                     ;; a sequence exactly at the limit takes no more elements
                     (define full
                       (psek-append s (psek-take s (- (max-length) (psek-length s)))))
                     (check-equal? (psek-length full) (max-length))
                     (check-elements full)
                     (too-long 'sek-insert (sek-insert full 0 'x))))))
