#lang racket/base
;; The Racket half of the differential harness: executes the same script as
;; the OCaml driver and prints a trace in the same format.

(require racket/list
         racket/string
         racket/port
         sek)

(define nslots 6)
(define es (build-vector nslots (lambda (_) (make-eseq))))
(define ps (build-vector nslots (lambda (_) empty-pseq)))

(define (E i)
  (vector-ref es i))
(define (E! i v)
  (vector-set! es i v))
(define (P i)
  (vector-ref ps i))
(define (P! i v)
  (vector-set! ps i v))

(define (show-list l)
  (string-join (map number->string l) ","))

(define (side n)
  (if (eqv? n 0) 'front 'back))
(define (dir n)
  (if (eqv? n 0) 'forward 'backward))

;; The OCaml driver reports which exception an operation raised; match its
;; vocabulary so that the traces line up.
(define (protect thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (define m (exn-message e))
                               (cond
                                 [(regexp-match? #rx"empty" m) "empty"]
                                 [(regexp-match? #rx"not found|notfound" m) "notfound"]
                                 [else "invalid"]))])
    (thunk)))

(define (dump-state)
  (for ([i (in-range nslots)])
    (printf "  e~a=[~a]\n" i (show-list (eseq->list (E i)))))
  (for ([i (in-range nslots)])
    (printf "  p~a=[~a]\n" i (show-list (pseq->list (P i))))))

(define (iter->list s d)
  (define it (sek-iterator s (dir d)))
  (let loop ([acc '()])
    (if (sek-iter-finished? it)
        (reverse acc)
        (loop (cons (sek-iter-get-and-move! it (dir d)) acc)))))

(define (cmp3 a b)
  (cond
    [(< a b) -1]
    [(> a b) 1]
    [else 0]))

(define (run line)
  (define toks (string-split (string-trim line)))
  (unless (null? toks)
    (define cmd (car toks))
    (define args (map string->number (cdr toks)))
    (define (a n)
      (list-ref args n))
    (printf "~a\n" line)
    (case cmd
      ;; ---- ephemeral core
      [("ecreate") (E! (a 0) (make-eseq))]
      [("eclear") (eseq-clear! (E (a 0)))]
      [("epush")
       (if (eqv? (a 1) 0)
           (eseq-push-front! (E (a 0)) (a 2))
           (eseq-push-back! (E (a 0)) (a 2)))]
      [("epop")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (number->string (if (eqv? (a 1) 0)
                                              (eseq-pop-front! (E (a 0)))
                                              (eseq-pop-back! (E (a 0))))))))]
      [("epeek")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (number->string (if (eqv? (a 1) 0)
                                              (eseq-first (E (a 0)))
                                              (eseq-last (E (a 0))))))))]
      [("eget") (printf "-> ~a\n" (protect (lambda () (number->string (eseq-ref (E (a 0)) (a 1))))))]
      [("eset")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (eseq-set! (E (a 0)) (a 1) (a 2))
                          "ok")))]
      [("elen") (printf "-> ~a\n" (eseq-length (E (a 0))))]
      [("eempty") (printf "-> ~a\n" (if (eseq-empty? (E (a 0))) "true" "false"))]
      ;; ---- ephemeral structure
      [("eassign") (eseq-assign! (E (a 0)) (E (a 1)))]
      [("ecopy") (E! (a 0) (eseq-copy (E (a 1)) #:mode 'copy))]
      [("ecopyshare") (E! (a 0) (eseq-copy (E (a 1)) #:mode 'share))]
      [("eappend") (eseq-append! (E (a 0)) (E (a 1)) (side (a 2)))]
      [("econcat") (E! (a 0) (eseq-concat! (E (a 1)) (E (a 2))))]
      [("esplit")
       (define-values (s1 s2) (eseq-split! (E (a 2)) (a 3)))
       (E! (a 0) s1)
       (E! (a 1) s2)]
      [("ecarve") (E! (a 0) (eseq-carve! (E (a 1)) (a 3) (side (a 2))))]
      [("etake") (eseq-take! (E (a 0)) (a 2) (side (a 1)))]
      [("edrop") (eseq-drop! (E (a 0)) (a 2) (side (a 1)))]
      [("esub") (E! (a 0) (sek-sub (E (a 1)) (a 2) (a 3)))]
      [("efill") (sek-fill! (E (a 0)) (a 1) (a 2) (a 3))]
      [("eblit") (sek-blit! (E (a 1)) (a 2) (E (a 0)) (a 3) (a 4))]
      ;; ---- conversions
      [("esnap") (P! (a 0) (eseq-snapshot (E (a 1))))]
      [("esnapclear") (P! (a 0) (eseq-snapshot-and-clear! (E (a 1))))]
      [("eedit") (E! (a 0) (pseq-edit (P (a 1))))]
      ;; ---- ephemeral derived
      [("edump") (printf "-> [~a]\n" (show-list (eseq->list (E (a 0)))))]
      [("edumpiter") (printf "-> [~a]\n" (show-list (iter->list (E (a 0)) (a 1))))]
      [("efold")
       (printf "-> ~a\n" (sek-fold-left (E (a 0)) (lambda (acc x) (+ (* acc 3) x)) 7))
       (printf "-> ~a\n" (sek-fold-right (E (a 0)) (lambda (x acc) (+ (* acc 3) x)) 7))]
      [("emap") (E! (a 0) (sek-map (E (a 1)) (lambda (x) (+ (* x 2) 1))))]
      [("emapi") (E! (a 0) (sek-map/index (E (a 1)) (lambda (i x) (+ (* i 100) x))))]
      [("efilter") (E! (a 0) (sek-filter (E (a 1)) (lambda (x) (not (zero? (modulo x 3))))))]
      [("efiltermap")
       (E! (a 0) (sek-filter-map (E (a 1)) (lambda (x) (and (even? x) (quotient x 2)))))]
      [("erev") (E! (a 0) (sek-reverse (E (a 1))))]
      [("esort") (E! (a 0) (sek-sort (E (a 0)) <))]
      [("euniq") (E! (a 0) (sek-uniq (E (a 1)) =))]
      [("emerge")
       (E! (a 1) (sek-sort (E (a 1)) <))
       (E! (a 2) (sek-sort (E (a 2)) <))
       (E! (a 0) (sek-merge (E (a 1)) (E (a 2)) <))]
      [("epart")
       (define-values (s1 s2) (sek-partition (E (a 2)) even?))
       (E! (a 0) s1)
       (E! (a 1) s2)]
      [("ezip")
       (define n (min (eseq-length (E (a 1))) (eseq-length (E (a 2)))))
       (define z (sek-zip (sek-sub (E (a 1)) 0 n) (sek-sub (E (a 2)) 0 n)))
       (printf "-> [~a]\n"
               (string-join (for/list ([p (in-sek z)])
                              (format "~a/~a" (car p) (cdr p)))
                            ","))]
      [("efind")
       (printf "-> ~a\n"
               (let ([r (sek-find (E (a 0)) (lambda (x) (> x (a 2))) (dir (a 1)))])
                 (if r
                     (number->string r)
                     "notfound")))]
      [("eforall")
       (printf "-> ~a\n" (if (sek-for-all? (E (a 0)) (lambda (x) (< x (a 1)))) "true" "false"))]
      [("eexists")
       (printf "-> ~a\n" (if (sek-exists? (E (a 0)) (lambda (x) (> x (a 1)))) "true" "false"))]
      [("emem") (printf "-> ~a\n" (if (sek-member? (E (a 0)) (a 1)) "true" "false"))]
      [("eequal") (printf "-> ~a\n" (if (sek-equal? (E (a 0)) (E (a 1))) "true" "false"))]
      [("ecompare") (printf "-> ~a\n" (sek-compare (E (a 0)) (E (a 1)) cmp3))]
      ;; ---- persistent core
      [("pcreate") (P! (a 0) empty-pseq)]
      [("ppush")
       (P! (a 0)
           (if (eqv? (a 1) 0)
               (pseq-push-front (P (a 0)) (a 2))
               (pseq-push-back (P (a 0)) (a 2))))]
      [("ppop")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (define-values (x s)
                            (if (eqv? (a 1) 0)
                                (pseq-pop-front (P (a 0)))
                                (pseq-pop-back (P (a 0)))))
                          (P! (a 0) s)
                          (number->string x))))]
      [("ppeek")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (number->string (if (eqv? (a 1) 0)
                                              (pseq-first (P (a 0)))
                                              (pseq-last (P (a 0))))))))]
      [("pget") (printf "-> ~a\n" (protect (lambda () (number->string (pseq-ref (P (a 0)) (a 1))))))]
      [("pset")
       (printf "-> ~a\n"
               (protect (lambda ()
                          (P! (a 0) (pseq-set (P (a 0)) (a 1) (a 2)))
                          "ok")))]
      [("plen") (printf "-> ~a\n" (pseq-length (P (a 0))))]
      [("pconcat") (P! (a 0) (pseq-append (P (a 1)) (P (a 2))))]
      [("psplit")
       (define-values (s1 s2) (pseq-split (P (a 2)) (a 3)))
       (P! (a 0) s1)
       (P! (a 1) s2)]
      [("ptake")
       (P! (a 0)
           (if (eqv? (a 2) 0)
               (sek-take (P (a 1)) (a 3))
               (sek-drop (P (a 1)) (a 3))))]
      [("pdrop")
       (P! (a 0)
           (if (eqv? (a 2) 0)
               (sek-drop (P (a 1)) (a 3))
               (sek-take (P (a 1)) (a 3))))]
      [("psub") (P! (a 0) (sek-sub (P (a 1)) (a 2) (a 3)))]
      [("pdump") (printf "-> [~a]\n" (show-list (pseq->list (P (a 0)))))]
      [("pdumpiter") (printf "-> [~a]\n" (show-list (iter->list (P (a 0)) (a 1))))]
      [("pmap") (P! (a 0) (sek-map (P (a 1)) (lambda (x) (+ (* x 2) 1))))]
      [("pfilter") (P! (a 0) (sek-filter (P (a 1)) (lambda (x) (not (zero? (modulo x 3))))))]
      [("prev") (P! (a 0) (sek-reverse (P (a 1))))]
      [("psort") (P! (a 0) (sek-sort (P (a 1)) <))]
      [("pfold")
       (printf "-> ~a\n" (sek-fold-left (P (a 0)) (lambda (acc x) (+ (* acc 3) x)) 7))
       (printf "-> ~a\n" (sek-fold-right (P (a 0)) (lambda (x acc) (+ (* acc 3) x)) 7))]
      [("pequal") (printf "-> ~a\n" (if (sek-equal? (P (a 0)) (P (a 1))) "true" "false"))]
      ;; ---- iterator walk
      [("piterwalk")
       (define s (P (a 0)))
       (define it (sek-iterator s 'forward))
       (define n (pseq-length s))
       (printf "-> ~a\n"
               (string-join (for/list ([k (in-range 8)])
                              (define raw (+ (a 1) (* k 7)))
                              (define target
                                (if (zero? n)
                                    -1
                                    (- (modulo raw (+ n 2)) 1)))
                              (sek-iter-reach! it target)
                              (format "~a:~a:~a"
                                      k
                                      (sek-iter-index it)
                                      (if (sek-iter-finished? it)
                                          "-"
                                          (sek-iter-get it))))
                            ";"))]
      [("eitersweep")
       (define it (sek-iterator (E (a 0)) 'forward))
       (let loop ()
         (unless (sek-iter-finished? it)
           (sek-iter-set! it (+ (sek-iter-get it) (a 1)))
           (sek-iter-move! it 'forward)
           (loop)))]
      [("echeck") (sek-validate-eseq (E (a 0)))]
      [("pcheck") (sek-validate-pseq (P (a 0)))]
      [else (printf "-> UNKNOWN\n")])
    (dump-state)))

(define (getenv-int name default)
  (define v (getenv name))
  (or (and v (string->number v)) default))

(module+ main
  ;; the same knobs the OCaml driver reads, so both run the same tree shape
  (sek-configure! #:leaf-capacity (getenv-int "SEK_LEAF" 128)
                  #:node-capacity (getenv-int "SEK_NODE" 16)
                  #:short-threshold (getenv-int "SEK_THRESHOLD" 32)
                  #:overwrite-empty-slots? (not (eqv? 0 (getenv-int "SEK_OVERWRITE" 1)))
                  #:check-iterator-validity? (not (eqv? 0 (getenv-int "SEK_CHECKITER" 1))))
  ;; the slots were built before the settings were read
  (for ([i (in-range nslots)]) (vector-set! es i (make-eseq)))
  (for ([line (in-lines)]) (run line)))
