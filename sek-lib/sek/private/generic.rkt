#lang racket/base
;; The derived operations of the OCaml library's SEK signature.
;;
;; Everything here is written once, over iterators, and works on either
;; flavor of sequence.  Operations that build a sequence return the same
;; flavor as the sequence they were given, which is how the OCaml library's
;; two parallel modules are collapsed into one set of names.
;;
;; Traversal goes through segments rather than one element at a time: an
;; iterator hands out the whole contiguous run it is sitting on, and the run
;; is processed with a tight vector loop.  That is what keeps `fold` and
;; friends close to the speed of the same loop over an array.

(require (for-syntax racket/base)
         (only-in "unchecked.rkt"
                  unsafe-vector*-ref unsafe-vector*-set! unsafe-fx+ unsafe-fx- unsafe-fx< unsafe-fx>
                  unsafe-fx= unsafe-fxabs)
         racket/vector
         (only-in "unchecked.rkt" in-fx-range)
         "config.rkt"
         "persistent.rkt"
         "ephemeral.rkt"
         "iterator.rkt"
         "segment.rkt")

;; Every function here that a caller outside the library can reach checks its
;; arguments explicitly, with `unless`: the accessors of the library's own
;; structures do not check (see unchecked.rkt), so they would not raise on a
;; value of the wrong kind.

(provide sek?
         sek-length
         sek-empty?
         sek-ref
         sek-first
         sek-last
         in-sek
         in-psek
         in-esek
         sek-segments-for-each
         sek-for-each
         sek-for-each/index
         sek-fold-left
         sek-fold-right
         sek-find
         sek-find-index
         sek-find-map
         sek-for-all?
         sek-exists?
         sek-member?
         sek-memq?
         sek->list
         sek->vector
         sek-equal?
         sek<?
         sek-segments-for-each2
         sek-for-each2
         sek-fold-left2
         sek-fold-right2
         sek-for-all2?
         sek-exists2?
         sek-map
         sek-map/index
         sek-map2
         sek-filter
         sek-filter-map
         sek-partition
         sek-reverse
         sek-zip
         sek-unzip
         sek-append*
         sek-append-map
         sek-sort
         sek-uniq
         sek-merge
         sek-sub
         sek-take
         sek-drop
         sek-take-right
         sek-drop-right
         sek-insert
         sek-delete
         sek-index-of
         sek-copy
         esek-fill!
         esek-copy!
         esek-map!
         esek-map/index!
         make-psek
         build-psek
         build-esek
         sequence->psek
         sequence->esek
         for/esek
         for*/esek
         for/psek
         for*/psek)

(define (sek? v)
  (or (psek? v) (esek? v)))

(define (check-sek who v)
  (unless (sek? v)
    (raise-argument-error who "(or/c psek? esek?)" v))
  v)

(define (sek-length s)
  (check-sek 'sek-length s)
  (if (psek? s)
      (psek-length s)
      (esek-length s)))

(define (sek-empty? s)
  (check-sek 'sek-empty? s)
  (eqv? 0 (sek-length s)))

(define (sek-ref s i)
  (check-sek 'sek-ref s)
  (if (psek? s)
      (psek-ref/who 'sek-ref s i)
      (esek-ref/who 'sek-ref s i)))

(define (check-nonempty who s)
  (when (eqv? 0 (sek-length s))
    (raise-arguments-error who "sequence is empty")))

(define (sek-first s)
  (check-sek 'sek-first s)
  (check-nonempty 'sek-first s)
  (if (psek? s)
      (psek-first s)
      (esek-first s)))
(define (sek-last s)
  (check-sek 'sek-last s)
  (check-nonempty 'sek-last s)
  (if (psek? s)
      (psek-last s)
      (esek-last s)))

;; ------------------------------------------------------- watched traversal

;; An update to an ephemeral sequence while a traversal hands its elements to
;; the caller's code would show through a run already read from, and could
;; hand over the private empty-slot marker that a pop leaves behind (see
;; `raise-updated` in ephemeral.rkt).  So each traversal here watches the
;; sequence: before each element, and before each step of its iterators, it
;; checks that the sequence has not been updated since the traversal began.
;; `watched` is the sequence to check, or #f for a persistent sequence or
;; when the checks are turned off.
(define (watched s)
  (and (esek? s) (check-iterator-validity?) s))

(define (birth-of w)
  (if w (esek-iterator-born! w) 0))

(define-syntax-rule (unchanged? w birth)
  (unsafe-fx= birth (unsafe-fxabs (esek-version w))))

(define-syntax-rule (check-unchanged! who w birth)
  (when w
    (unless (unchanged? w birth)
      (raise-updated who))))

;; The same check as a procedure, for the traversals that step iterators one
;; element at a time: they call it after the caller's code and before the
;; next step, so that an update is reported in the name of the operation the
;; caller called rather than of the iterator operation that noticed it.
(define (watch who s)
  (define w (watched s))
  (if w
      (let ([birth (esek-iterator-born! w)])
        (lambda ()
          (unless (unchanged? w birth)
            (raise-updated who))))
      void))

;; The vector of a short persistent sequence, or #f.  A traversal walks it
;; directly: making an iterator costs more than reading thirty-two elements.
(define (short-vector s)
  (and (psek? s)
       (let ([r (psek-rep s)])
         (and (vector? r) r))))

;; Fold over the elements of s in direction dir a run at a time, binding x to
;; each element in turn and acc to the value of body for the one before,
;; starting from init.  The loop is expanded three times: for a short
;; persistent sequence, and with and without the check, so that a sequence
;; with nothing to watch pays nothing for it.
(define-syntax-rule (fold-elements who s dir ([acc init]) (x) body ...)
  (let* ([sq s]
         [d dir]
         [sv (short-vector sq)])
    (cond
      [sv (fold-run sv 0 (vector-length sv) d ([acc init]) (x) (void) body ...)]
      [else
       (let ([w (watched sq)]
             [it (sek-iterator sq d)])
         (if w
             (let ([birth (esek-iterator-born! w)])
               (fold-runs it d ([acc init]) (x) (check-unchanged! who w birth) body ...))
             (fold-runs it d ([acc init]) (x) (void) body ...)))])))

;; Fold over slots [o, e) of v, in direction d, running `check` before each.
(define-syntax-rule (fold-run v o e d ([acc init]) (x) check body ...)
  (if (eq? d 'forward)
      (let run ([j o] [acc init])
        (cond
          [(unsafe-fx= j e) acc]
          [else
           check
           (run (unsafe-fx+ j 1) (let ([x (unsafe-vector*-ref v j)]) body ...))]))
      (let run ([j (unsafe-fx- e 1)] [acc init])
        (cond
          [(unsafe-fx< j o) acc]
          [else
           check
           (run (unsafe-fx- j 1) (let ([x (unsafe-vector*-ref v j)]) body ...))]))))

;; The loop of `fold-elements`, which runs `check` before each run and each
;; element.
(define-syntax-rule (fold-runs it d ([acc init]) (x) check body ...)
  (let loop ([acc init])
    check
    (if (sek-iter-finished? it)
        acc
        (let* ([sg (sek-iter-segment-and-jump! it d)]
               [v (segment-vector sg)]
               [o (segment-start sg)]
               [e (unsafe-fx+ o (segment-length sg))])
          (loop (fold-run v o e d ([acc acc]) (x) check body ...))))))

;; The same for two sequences at once: x and y are bound to the elements of
;; s1 and s2 at the same distance from the end the traversal starts at, and
;; the fold stops where the shorter sequence does.  Matching runs of the two
;; are walked together.
(define-syntax-rule (fold-elements2 who s1 s2 dir ([acc init]) (x y) body ...)
  (let* ([d dir]
         [fwd? (eq? d 'forward)]
         [w1 (watched s1)]
         [birth1 (birth-of w1)]
         [w2 (watched s2)]
         [birth2 (birth-of w2)]
         [it1 (sek-iterator s1 d)]
         [it2 (sek-iterator s2 d)])
    (let loop ([acc init])
      (check-unchanged! who w1 birth1)
      (check-unchanged! who w2 birth2)
      (if (or (sek-iter-finished? it1) (sek-iter-finished? it2))
          acc
          (let*-values ([(v1 p1 n1) (run-toward it1 d fwd?)]
                        [(v2 p2 n2) (run-toward it2 d fwd?)]
                        [(k) (if (unsafe-fx< n1 n2) n1 n2)]
                        [(step) (if fwd? 1 -1)])
            (let run ([j 0] [p1 p1] [p2 p2] [acc acc])
              (check-unchanged! who w1 birth1)
              (check-unchanged! who w2 birth2)
              (cond
                [(unsafe-fx= j k)
                 (sek-iter-jump! it1 d k)
                 (sek-iter-jump! it2 d k)
                 (loop acc)]
                [else
                 (run (unsafe-fx+ j 1) (unsafe-fx+ p1 step) (unsafe-fx+ p2 step)
                      (let ([x (unsafe-vector*-ref v1 p1)]
                            [y (unsafe-vector*-ref v2 p2)])
                        body ...))])))))))

;; The run an iterator is on, as its vector, the slot of the current element,
;; and how many elements lie from there to the run's end in the direction of
;; travel.
(define (run-toward it d fwd?)
  (define sg (sek-iter-segment it d))
  (define o (segment-start sg))
  (define n (segment-length sg))
  (values (segment-vector sg) (if fwd? o (unsafe-fx+ o (unsafe-fx- n 1))) n))

;; ------------------------------------------------------------- builders

;; Results are accumulated in a psek builder, which fills one chunk-sized
;; buffer at a time, and handed over as the flavor the caller started from.
(define (close-builder b like)
  (define r (psek-builder-close b))
  (if (psek? like) r (psek-edit r)))

;; Build a sequence shaped like `like` by walking `s` one segment at a time,
;; running `body` for each element with `x` bound to it and `add!` bound to a
;; procedure that appends to the result.
;;
;; The point is what it does not do.  Written with `sek-for-each` and
;; `build-from`, a per-element operation costs three closure calls: the
;; segment loop calls the traversal's procedure, that one calls the caller's,
;; and the caller calls `emit`.  Here the segment loop is the caller's loop,
;; `add!` is a let-bound lambda in operator position, and the hot half of
;; `psek-builder-add!` is small enough to inline across the module boundary --
;; so an element that the body drops costs the test and nothing else.
(define-syntax-rule (build-by-segments who like s x add! body ...)
  (let* ([lk like]
         [sq s]
         [b (make-psek-builder)]
         [add! (lambda (y) (psek-builder-add! b y))])
    (fold-elements who sq 'forward ([ignored (void)]) (x) body ... (void))
    (close-builder b lk)))

;; The same accumulation, for the operations whose traversal is not a plain
;; forward walk of one sequence -- `map/index`, `map2`, `reverse`, `append*`,
;; `append-map`.  They pass a thunk that calls `emit`, and pay for the call.
(define (build-from like xs-thunk)
  (define b (make-psek-builder))
  (xs-thunk (lambda (x) (psek-builder-add! b x)))
  (close-builder b like))

;; ------------------------------------------------------------- traversal

;; Hand each maximal run of contiguous elements to proc, as a segment.
(define (sek-segments-for-each s proc [dir 'forward])
  (check-sek 'sek-segments-for-each s)
  (define it (sek-iterator s dir))
  (let loop ()
    (unless (sek-iter-finished? it)
      (proc (sek-iter-segment-and-jump! it dir))
      (loop))))

(define (sek-for-each s proc [dir 'forward])
  (check-sek 'sek-for-each s)
  (check-procedure 'sek-for-each proc 1)
  (check-direction 'sek-for-each dir)
  (fold-elements 'sek-for-each s dir ([ignored (void)]) (x) (proc x) (void)))

(define (sek-for-each/index s proc [dir 'forward])
  (check-sek 'sek-for-each/index s)
  (check-procedure 'sek-for-each/index proc 2)
  (check-direction 'sek-for-each/index dir)
  (for-each/index 'sek-for-each/index s proc dir))

;; The index rides along as the fold's accumulator.
(define (for-each/index who s proc dir)
  (if (eq? dir 'forward)
      (fold-elements who s 'forward ([i 0]) (x) (proc i x) (unsafe-fx+ i 1))
      (fold-elements who s 'backward ([i (sub1 (sek-length s))]) (x) (proc i x) (unsafe-fx- i 1)))
  (void))

(define (sek-fold-left s proc init)
  (check-sek 'sek-fold-left s)
  (check-procedure 'sek-fold-left proc 2)
  (fold-elements 'sek-fold-left s 'forward ([acc init]) (x) (proc acc x)))

(define (sek-fold-right s proc init)
  (check-sek 'sek-fold-right s)
  (check-procedure 'sek-fold-right proc 2)
  (fold-elements 'sek-fold-right s 'backward ([acc init]) (x) (proc x acc)))

;; The generic-dispatch version, used when a sequence value is passed around
;; rather than written directly in a for clause.
(define (in-sek/proc s [dir 'forward])
  (check-in-sek s dir)
  (in-sek/unchecked 'in-sek s dir))

;; A run at a time, like the for-clause form: the state is the run's vector
;; and a position in it, and the iterator is consulted once per run.
(define (in-sek/unchecked who s dir)
  (make-do-sequence
   (lambda ()
     (define w (watched s))
     (define birth (birth-of w))
     (define sv (short-vector s))
     (define it (and (not sv) (sek-iterator s dir)))
     (define step (if (eq? dir 'forward) 1 -1))
     (define v sv)
     (define j 0)
     (define end 0)
     (define (refill!)
       (check-unchanged! who w birth)
       (define-values (v2 j2 end2 more?)
         (if it (sek-loop-refill it dir) (values #f 0 0 #f)))
       (set! v v2)
       (set! j j2)
       (set! end end2)
       more?)
     (define more?
       (cond
         [sv
          (define n (vector-length sv))
          (set! j (if (eq? dir 'forward) 0 (unsafe-fx- n 1)))
          (set! end (if (eq? dir 'forward) n -1))
          (unsafe-fx< 0 n)]
         [else (refill!)]))
     (values (lambda (_)
               (check-unchanged! who w birth)
               (unsafe-vector*-ref v j))
             (lambda (_)
               (set! j (unsafe-fx+ j step))
               (when (unsafe-fx= j end)
                 (set! more? (refill!)))
               #f)
             #f
             (lambda (_) more?)
             #f
             #f))))


;; Refill the loop state from the iterator's next run of contiguous storage.
;; Returns the vector, the position to read, the limit, and whether there is
;; anything left.
(define (sek-loop-refill it dir)
  (define sg (sek-iter-segment-and-jump*! it dir))
  (if sg
      (let ([o (segment-start sg)] [k (segment-length sg)])
        (if (eq? dir 'forward)
            (values (segment-vector sg) o (+ o k) #t)
            ;; going backward the run is read from its far end inwards
            (values (segment-vector sg) (+ o k -1) (- o 1) #t)))
      (values #f 0 0 #f)))

;; In a for clause these expand to a loop over the underlying storage, so the
;; common step is a vector reference and an increment -- the same shape the
;; segment-based traversals use, rather than one iterator call per element.
;;
;; The four loop variables are the run's vector, the position to read, the
;; limit, and the step (+1 forward, -1 backward).  All the work happens in one
;; inner binding, because :do-in binds its inner clauses in parallel.
;;
;; Like the other traversals, the loop watches an ephemeral sequence, and
;; checks before each element that it has not been updated.
;;
;; `check` tests the sequence and the direction, and `who` names the form.
(begin-for-syntax
  (define ((sek-for-clause check who) stx)
    (define (expand seq-expr dir-expr id)
      (with-syntax ([seq-expr seq-expr] [dir-expr dir-expr] [id id] [check check] [who who])
        #'[(id)
           (:do-in
            ([(it dir w birth v0 i0 n0)
              (let* ([s seq-expr] [d dir-expr])
                (check s d)
                (let ([w (watched s)] [sv (short-vector s)])
                  ;; a short persistent sequence is its own single run
                  (if sv
                      (let ([len (vector-length sv)])
                        (if (eq? d 'forward)
                            (values #f d w 0 sv 0 len)
                            (values #f d w 0 sv (unsafe-fx- len 1) -1)))
                      (values (sek-iterator s d) d w (birth-of w) #f 0 0))))])
            #t
            ([v v0] [i i0] [n n0] [step (if (eq? dir 'forward) 1 -1)])
            #t
            ([(v* i* n* step* id)
              (begin
                (check-unchanged! who w birth)
                (if (if (unsafe-fx< step 0) (unsafe-fx> i n) (unsafe-fx< i n))
                    (values v i n step (unsafe-vector*-ref v i))
                    (let-values ([(v2 i2 n2 ok?)
                                  (if it (sek-loop-refill it dir) (values #f 0 0 #f))])
                      (if ok?
                          (values v2 i2 n2
                                  (if (eq? dir 'forward) 1 -1)
                                  (unsafe-vector*-ref v2 i2))
                          (values #f 0 0 1 #f)))))])
            (if (unsafe-fx< step* 0) (unsafe-fx> i* n*) (unsafe-fx< i* n*))
            #t
            (v* (unsafe-fx+ i* step*) n* step*))]))
    (syntax-case stx ()
      [[(id) (_ seq-expr)] (expand #'seq-expr #''forward #'id)]
      [[(id) (_ seq-expr dir-expr)] (expand #'seq-expr #'dir-expr #'id)]
      [_ #f])))

(define (check-in-sek s d)
  (check-sek 'in-sek s)
  (check-direction 'in-sek d))

(define-sequence-syntax in-sek
  (lambda () #'in-sek/proc)
  (sek-for-clause #'check-in-sek #''in-sek))

;; The same loop, restricted to one flavor.  These shadow the plain readers
;; that persistent.rkt and ephemeral.rkt define.
(define (check-in-psek s d)
  (unless (psek? s) (raise-argument-error 'in-psek "psek?" s))
  (check-direction 'in-psek d))

(define (check-in-esek e d)
  (unless (esek? e) (raise-argument-error 'in-esek "esek?" e))
  (check-direction 'in-esek d))

(define (in-psek/proc s [dir 'forward])
  (check-in-psek s dir)
  (in-sek/unchecked 'in-psek s dir))

(define (in-esek/proc e [dir 'forward])
  (check-in-esek e dir)
  (in-sek/unchecked 'in-esek e dir))

(define-sequence-syntax in-psek
  (lambda () #'in-psek/proc)
  (sek-for-clause #'check-in-psek #''in-psek))

(define-sequence-syntax in-esek
  (lambda () #'in-esek/proc)
  (sek-for-clause #'check-in-esek #''in-esek))

(define (sek->list s [dir 'forward])
  (check-sek 'sek->list s)
  (check-direction 'sek->list dir)
  (define acc '())
  (sek-for-each s (lambda (x) (set! acc (cons x acc))) (if (eq? dir 'forward) 'backward 'forward))
  acc)

(define (sek->vector s)
  (check-sek 'sek->vector s)
  (define v (make-vector (sek-length s) #f))
  (define i 0)
  (sek-for-each s
                (lambda (x)
                  (vector-set! v i x)
                  (set! i (add1 i))))
  v)

;; ------------------------------------------------------------- searching

;; Search the elements of s in direction dir a run at a time.  x is bound to
;; each element and i to its index; `hit` is bound to the value of test, and
;; the first time that is true the search returns result.  If it never is,
;; the search returns default.  Like `fold-elements`, it checks before it
;; hands over each element, and once more before it returns, that the caller's
;; code has not updated the sequence.
(define-syntax-rule (search-elements who s dir (x i) [hit test] result default)
  (let* ([sq s]
         [d dir]
         [sv (short-vector sq)])
    (cond
      [sv
       (let ([n (vector-length sv)])
         (search-run sv 0 n d (if (eq? d 'forward) 0 (unsafe-fx- n 1))
                     (x i) [hit test] (void) result (lambda (i) default)))]
      [else
       (let* ([w (watched sq)]
              [birth (birth-of w)]
              [it (sek-iterator sq d)])
         (let loop ([i0 (if (eq? d 'forward) 0 (unsafe-fx- (sek-length sq) 1))])
           (check-unchanged! who w birth)
           (if (sek-iter-finished? it)
               default
               (let* ([sg (sek-iter-segment-and-jump! it d)]
                      [v (segment-vector sg)]
                      [o (segment-start sg)]
                      [e (unsafe-fx+ o (segment-length sg))])
                 (search-run v o e d i0
                             (x i) [hit test] (check-unchanged! who w birth) result loop)))))])))

;; Search slots [o, e) of v in direction d, counting indices from i0; `check`
;; runs after each test, and `next` gets the index after the last one.
(define-syntax-rule (search-run v o e d i0 (x i) [hit test] check result next)
  (if (eq? d 'forward)
      (let run ([j o] [i i0])
        (if (unsafe-fx= j e)
            (next i)
            (let* ([x (unsafe-vector*-ref v j)] [hit test])
              check
              (if hit result (run (unsafe-fx+ j 1) (unsafe-fx+ i 1))))))
      (let run ([j (unsafe-fx- e 1)] [i i0])
        (if (unsafe-fx< j o)
            (next i)
            (let* ([x (unsafe-vector*-ref v j)] [hit test])
              check
              (if hit result (run (unsafe-fx- j 1) (unsafe-fx- i 1))))))))

;; The search for two sequences at once, forward, as `fold-elements2` pairs
;; their elements.
(define-syntax-rule (search-elements2 who s1 s2 (x y) [hit test] result default)
  (let* ([w1 (watched s1)]
         [birth1 (birth-of w1)]
         [w2 (watched s2)]
         [birth2 (birth-of w2)]
         [it1 (sek-iterator s1 'forward)]
         [it2 (sek-iterator s2 'forward)])
    (let loop ()
      (check-unchanged! who w1 birth1)
      (check-unchanged! who w2 birth2)
      (if (or (sek-iter-finished? it1) (sek-iter-finished? it2))
          default
          (let*-values ([(v1 p1 n1) (run-toward it1 'forward #t)]
                        [(v2 p2 n2) (run-toward it2 'forward #t)]
                        [(k) (if (unsafe-fx< n1 n2) n1 n2)])
            (let run ([j 0])
              (cond
                [(unsafe-fx= j k)
                 (sek-iter-jump! it1 'forward k)
                 (sek-iter-jump! it2 'forward k)
                 (loop)]
                [else
                 (let* ([x (unsafe-vector*-ref v1 (unsafe-fx+ p1 j))]
                        [y (unsafe-vector*-ref v2 (unsafe-fx+ p2 j))]
                        [hit test])
                   (check-unchanged! who w1 birth1)
                   (check-unchanged! who w2 birth2)
                   (if hit result (run (unsafe-fx+ j 1))))])))))))

;; The first element satisfying pred, or #f.  Use sek-find-index when an
;; element could itself be #f.
(define (sek-find s pred [dir 'forward])
  (check-sek 'sek-find s)
  (check-procedure 'sek-find pred 1)
  (check-direction 'sek-find dir)
  (search-elements 'sek-find s dir (x i) [hit (pred x)] x #f))

;; The index of the first (or last) element satisfying pred, or #f.
(define (sek-find-index s pred [dir 'forward])
  (check-sek 'sek-find-index s)
  (check-procedure 'sek-find-index pred 1)
  (check-direction 'sek-find-index dir)
  (find-index 'sek-find-index s pred dir))

(define (find-index who s pred dir)
  (search-elements who s dir (x i) [hit (pred x)] i #f))

(define (sek-find-map s proc [dir 'forward])
  (check-sek 'sek-find-map s)
  (check-procedure 'sek-find-map proc 1)
  (check-direction 'sek-find-map dir)
  (search-elements 'sek-find-map s dir (x i) [hit (proc x)] hit #f))

(define (sek-for-all? s pred)
  (check-sek 'sek-for-all? s)
  (check-procedure 'sek-for-all? pred 1)
  (search-elements 'sek-for-all? s 'forward (x i) [miss (not (pred x))] #f #t))

(define (sek-exists? s pred)
  (check-sek 'sek-exists? s)
  (check-procedure 'sek-exists? pred 1)
  (exists 'sek-exists? s pred))

(define (exists who s pred)
  (search-elements who s 'forward (x i) [hit (pred x)] #t #f))

(define (sek-member? s v [same? equal?])
  (check-sek 'sek-member? s)
  (check-procedure 'sek-member? same? 2)
  (exists 'sek-member? s (lambda (y) (same? v y))))

(define (sek-memq? s v)
  (check-sek 'sek-memq? s)
  (exists 'sek-memq? s (lambda (y) (eq? v y))))

;; --------------------------------------------------------- binary traversal

;; Hand matching runs of the two sequences to proc, as a pair of segments of
;; equal length -- the fast path for a binary loop.
(define (sek-segments-for-each2 s1 s2 proc [dir 'forward])
  (check-sek 'sek-segments-for-each2 s1)
  (check-sek 'sek-segments-for-each2 s2)
  (define it1 (sek-iterator s1 dir))
  (define it2 (sek-iterator s2 dir))
  (define (cut sg k)
    ;; going backward, a segment ends at the cursor, so trim it at the front
    (if (eq? dir 'forward)
        (segment (segment-vector sg) (segment-start sg) k)
        (segment (segment-vector sg)
                 (- (+ (segment-start sg) (segment-length sg)) k)
                 k)))
  (let loop ()
    (unless (or (sek-iter-finished? it1) (sek-iter-finished? it2))
      (define a (sek-iter-segment it1 dir))
      (define b (sek-iter-segment it2 dir))
      (define k (min (segment-length a) (segment-length b)))
      (proc (cut a k) (cut b k))
      (sek-iter-jump! it1 dir k)
      (sek-iter-jump! it2 dir k)
      (loop))))

(define (sek-for-each2 s1 s2 proc [dir 'forward])
  (check-sek 'sek-for-each2 s1)
  (check-sek 'sek-for-each2 s2)
  (check-procedure 'sek-for-each2 proc 2)
  (check-direction 'sek-for-each2 dir)
  (for-each2 'sek-for-each2 s1 s2 proc dir))

(define (for-each2 who s1 s2 proc dir)
  (fold-elements2 who s1 s2 dir ([ignored (void)]) (x y) (proc x y) (void)))

(define (sek-fold-left2 s1 s2 proc init)
  (check-sek 'sek-fold-left2 s1)
  (check-sek 'sek-fold-left2 s2)
  (check-procedure 'sek-fold-left2 proc 3)
  (fold-elements2 'sek-fold-left2 s1 s2 'forward ([acc init]) (x y) (proc acc x y)))

;; As in the reference, where fold_right2 is iter2 backward: the two
;; sequences are paired from their last elements, so when the lengths differ
;; the longer one's extra elements at the front are left out.
(define (sek-fold-right2 s1 s2 proc init)
  (check-sek 'sek-fold-right2 s1)
  (check-sek 'sek-fold-right2 s2)
  (check-procedure 'sek-fold-right2 proc 3)
  (fold-elements2 'sek-fold-right2 s1 s2 'backward ([acc init]) (x y) (proc x y acc)))

(define (sek-for-all2? s1 s2 pred)
  (check-sek 'sek-for-all2? s1)
  (check-sek 'sek-for-all2? s2)
  (check-procedure 'sek-for-all2? pred 2)
  (for-all2 'sek-for-all2? s1 s2 pred))

(define (for-all2 who s1 s2 pred)
  (search-elements2 who s1 s2 (x y) [miss (not (pred x y))] #f #t))

(define (sek-exists2? s1 s2 pred)
  (check-sek 'sek-exists2? s1)
  (check-sek 'sek-exists2? s2)
  (check-procedure 'sek-exists2? pred 2)
  (search-elements2 'sek-exists2? s1 s2 (x y) [hit (pred x y)] #t #f))

(define (sek-equal? s1 s2 [same? equal?])
  (check-sek 'sek-equal? s1)
  (check-sek 'sek-equal? s2)
  (check-procedure 'sek-equal? same? 2)
  (and (= (sek-length s1) (sek-length s2)) (for-all2 'sek-equal? s1 s2 same?)))

;; Sequences used directly, and `equal?` and hashing, traverse by runs too.
(set-sek-traversals! (lambda (who s) (in-sek/unchecked who s 'forward))
                     for-all2)

;; Whether s1 comes before s2 in lexicographic order: at the first position
;; where they differ, s1's element is less, or s1 runs out first.  Elements
;; are ordered by less-than?, as in `sort`, and two elements neither of which
;; is less than the other are equal.  Like less-than?, the order is strict, so
;; it can serve as `sort`'s less-than? for sequences.
(define (sek<? s1 s2 less-than?)
  (check-sek 'sek<? s1)
  (check-sek 'sek<? s2)
  (check-procedure 'sek<? less-than? 2)
  ;; when neither differs first, the shorter sequence comes first
  (define shorter? (< (sek-length s1) (sek-length s2)))
  (search-elements2 'sek<? s1 s2 (x y)
                    [order (cond
                             [(less-than? x y) 'before]
                             [(less-than? y x) 'after]
                             [else #f])]
                    (eq? order 'before)
                    shorter?))

;; ------------------------------------------------------------- producers

(define (sek-map s proc)
  (check-sek 'sek-map s)
  (check-procedure 'sek-map proc 1)
  (build-by-segments 'sek-map s s x add! (add! (proc x))))

(define (sek-map/index s proc)
  (check-sek 'sek-map/index s)
  (check-procedure 'sek-map/index proc 2)
  (build-from s (lambda (emit)
                  (for-each/index 'sek-map/index s (lambda (i x) (emit (proc i x))) 'forward))))

(define (sek-map2 s1 s2 proc)
  (check-sek 'sek-map2 s1)
  (check-sek 'sek-map2 s2)
  (check-procedure 'sek-map2 proc 2)
  (build-from s1 (lambda (emit)
                   (for-each2 'sek-map2 s1 s2 (lambda (x y) (emit (proc x y))) 'forward))))

(define (sek-filter s pred)
  (check-sek 'sek-filter s)
  (check-procedure 'sek-filter pred 1)
  (build-by-segments 'sek-filter s s x add! (when (pred x) (add! x))))

(define (sek-filter-map s proc)
  (check-sek 'sek-filter-map s)
  (check-procedure 'sek-filter-map proc 1)
  (build-by-segments 'sek-filter-map s s x add! (let ([y (proc x)]) (when y (add! y)))))

(define (sek-partition s pred)
  (check-sek 'sek-partition s)
  (check-procedure 'sek-partition pred 1)
  (define yes (make-psek-builder))
  (define no (make-psek-builder))
  (fold-elements 'sek-partition s 'forward ([ignored (void)]) (x)
    (psek-builder-add! (if (pred x) yes no) x))
  (values (close-builder yes s) (close-builder no s)))

(define (sek-reverse s)
  (check-sek 'sek-reverse s)
  (build-from s (lambda (emit) (sek-for-each s emit 'backward))))

(define (sek-zip s1 s2)
  (check-sek 'sek-zip s1)
  (check-sek 'sek-zip s2)
  (sek-map2 s1 s2 cons))

(define (sek-unzip s)
  (check-sek 'sek-unzip s)
  (define as (make-psek-builder))
  (define bs (make-psek-builder))
  (sek-for-each s
                (lambda (p)
                  (unless (pair? p)
                    (raise-arguments-error 'sek-unzip "element is not a pair" "element" p))
                  (psek-builder-add! as (car p))
                  (psek-builder-add! bs (cdr p))))
  (values (close-builder as s) (close-builder bs s)))

;; Concatenate a sequence of sequences.  On the ephemeral side this is a fold
;; of esek-append!, exactly as in the reference library, so -- as there -- it
;; empties every sequence it is given, including the outer one.  Handing over
;; each sequence's representation rather than copying its elements is also the
;; faster way to do it.  On the persistent side nothing is consumed.
;;
;; Every element is checked before any is consumed, so that an error leaves
;; the arguments as they were.
(define (sek-append* s)
  (check-sek 'sek-append* s)
  (define total
    (sek-fold-left s
                   (lambda (n t)
                     (unless (sek? t)
                       (raise-arguments-error 'sek-append* "element is not a sequence"
                                              "element" t))
                     (when (eq? t s)
                       (raise-arguments-error 'sek-append* "sequence contains itself"))
                     (+ n (sek-length t)))
                   0))
  (unless (<= total max-length)
    (raise-arguments-error 'sek-append* "resulting sequence would be too long"
                           "length" total "maximum length" max-length))
  (cond
    [(psek? s)
     (build-from s (lambda (emit) (sek-for-each s (lambda (t) (sek-for-each t emit)))))]
    [else
     (define acc (make-esek))
     (sek-for-each s (lambda (t) (append-esek! 'sek-append* acc t 'back)))
     (esek-clear! s)
     acc]))

(define (sek-append-map s proc)
  (check-sek 'sek-append-map s)
  (check-procedure 'sek-append-map proc 1)
  (build-from s (lambda (emit)
                  (fold-elements 'sek-append-map s 'forward ([ignored (void)]) (x)
                    (define t (proc x))
                    (unless (sek? t)
                      (raise-result-error 'sek-append-map "sek?" t))
                    (sek-for-each t emit)))))

;; ---------------------------------------------------------------- ordering

;; A stable sort, by way of a vector.
(define (sek-sort s less?)
  (check-sek 'sek-sort s)
  (check-procedure 'sek-sort less? 2)
  (define v (sek->vector s))
  (vector-sort! v less?)
  (define r (psek-build (vector-length v) (lambda (i) (vector-ref v i))))
  (if (psek? s) r (psek-edit r)))

;; Drop each element that is equal to the one before it, which removes all
;; duplicates when the sequence is sorted.
(define (sek-uniq s [same? equal?])
  (check-sek 'sek-uniq s)
  (check-procedure 'sek-uniq same? 2)
  (define b (make-psek-builder))
  (define first? #t)
  (define previous #f)
  (fold-elements 'sek-uniq s 'forward ([ignored (void)]) (x)
    (when (or first? (not (same? previous x)))
      (psek-builder-add! b x))
    (set! first? #f)
    (set! previous x))
  (close-builder b s))

;; Merge two sequences that are already sorted.
(define (sek-merge s1 s2 less?)
  (check-sek 'sek-merge s1)
  (check-sek 'sek-merge s2)
  (check-procedure 'sek-merge less? 2)
  (define unchanged1! (watch 'sek-merge s1))
  (define unchanged2! (watch 'sek-merge s2))
  (define b (make-psek-builder))
  (define it1 (sek-iterator s1 'forward))
  (define it2 (sek-iterator s2 'forward))
  (let loop ()
    (cond
      [(sek-iter-finished? it1)
       (let drain ()
         (unless (sek-iter-finished? it2)
           (psek-builder-add! b (sek-iter-get-and-move! it2 'forward))
           (drain)))]
      [(sek-iter-finished? it2)
       (let drain ()
         (unless (sek-iter-finished? it1)
           (psek-builder-add! b (sek-iter-get-and-move! it1 'forward))
           (drain)))]
      [else
       ;; take from the left on a tie, which is what makes the merge stable
       (define take-right? (less? (sek-iter-get it2) (sek-iter-get it1)))
       (unchanged1!)
       (unchanged2!)
       (if take-right?
           (psek-builder-add! b (sek-iter-get-and-move! it2 'forward))
           (psek-builder-add! b (sek-iter-get-and-move! it1 'forward)))
       (loop)]))
  (close-builder b s1))

;; ---------------------------------------------------------------- slicing

;; The `size` elements starting at `start`.
;;
;; A persistent sequence is sliced by splitting, which shares its chunks and
;; costs O(K.log n + log^2 n) whatever the size -- faster than copying even at
;; sixteen elements, and three orders of magnitude faster at half a million.
;; The OCaml library shares only above the threshold T; below it, `psek-take`
;; normalizes the result into the compact vector anyway.
;;
;; An ephemeral sequence is copied instead, at O(size + K.log n): splitting it
;; goes through a snapshot, which would cost `s` the ownership of its chunks
;; and make its own later updates more expensive.
(define (sek-sub s start size)
  (check-sek 'sek-sub s)
  (define n (sek-length s))
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? size)
               (<= (+ start size) n))
    (raise-arguments-error 'sek-sub
                           "slice outside the sequence"
                           "start"
                           start
                           "size"
                           size
                           "length"
                           n))
  (if (psek? s)
      (psek-take (psek-drop s start) size)
      (copy-slice s start size)))

(define (copy-slice s start size)
  (define b (make-psek-builder))
  (copy-range! b s start size)
  (close-builder b s))

;; Add the `size` elements of s from `start` on to builder b, a run at a time.
(define (copy-range! b s start size)
  (unless (eqv? size 0)
    (define it (sek-iterator-at-sentinel s 'front))
    (sek-iter-reach! it start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define sg (sek-iter-segment it 'forward))
        (define k (min remaining (segment-length sg)))
        (define v (segment-vector sg))
        (define o (segment-start sg))
        (for ([j (in-fx-range o (unsafe-fx+ o k))])
          (psek-builder-add! b (unsafe-vector*-ref v j)))
        (sek-iter-jump! it 'forward k)
        (loop (- remaining k))))))

;; None of these modifies s; the in-place versions are esek-take! and
;; esek-drop!.  On a persistent sequence, insert and delete go through split
;; and append, so they cost a split and a concatenation rather than O(N), and
;; take and drop are the one-sided splits.  On an ephemeral sequence all of
;; them copy, as `sek-sub` does and for the same reason: sharing would cost s
;; the ownership of its chunks.
(define (psek-insert-at s i v)
  (define-values (before after) (psek-split s i))
  (append-psek 'sek-insert (psek-add before v) after))

(define (sek-insert s i v)
  (check-sek 'sek-insert s)
  (define n (sek-length s))
  (unless (and (exact-nonnegative-integer? i) (<= i n))
    (raise-arguments-error 'sek-insert "index out of range" "index" i "length" n))
  (cond
    [(psek? s) (psek-insert-at s i v)]
    [else
     (check-append-length 'sek-insert n 1)
     (define b (make-psek-builder))
     (copy-range! b s 0 i)
     (psek-builder-add! b v)
     (copy-range! b s i (- n i))
     (close-builder b s)]))

(define (psek-delete-at s i)
  (psek-append (psek-take s i) (psek-drop s (add1 i))))

(define (sek-delete s i)
  (check-sek 'sek-delete s)
  (define n (sek-length s))
  (unless (and (exact-nonnegative-integer? i) (< i n))
    (raise-arguments-error 'sek-delete "index out of range" "index" i "length" n))
  (cond
    [(psek? s) (psek-delete-at s i)]
    [else
     (define b (make-psek-builder))
     (copy-range! b s 0 i)
     (copy-range! b s (add1 i) (- n i 1))
     (close-builder b s)]))

;; `same?` receives the value being searched for first, as
;; `treelist-index-of` calls its `eql?`.
(define (sek-index-of s v [same? equal?])
  (check-sek 'sek-index-of s)
  (check-procedure 'sek-index-of same? 2)
  (find-index 'sek-index-of s (lambda (x) (same? v x)) 'forward))

(define (sek-take-right s n)
  (check-sek 'sek-take-right s)
  (define len (sek-length s))
  (unless (and (exact-nonnegative-integer? n) (<= n len))
    (raise-arguments-error 'sek-take-right "index out of range" "index" n "length" len))
  (sek-drop s (- len n)))

(define (sek-drop-right s n)
  (check-sek 'sek-drop-right s)
  (define len (sek-length s))
  (unless (and (exact-nonnegative-integer? n) (<= n len))
    (raise-arguments-error 'sek-drop-right "index out of range" "index" n "length" len))
  (sek-take s (- len n)))

(define (check-split-index who s i)
  (define n (sek-length s))
  (unless (and (exact-nonnegative-integer? i) (<= i n))
    (raise-arguments-error who "index out of range" "index" i "length" n)))

(define (sek-take s n)
  (check-sek 'sek-take s)
  (check-split-index 'sek-take s n)
  (if (psek? s)
      (psek-take s n)
      (copy-slice s 0 n)))

(define (sek-drop s n)
  (check-sek 'sek-drop s)
  (check-split-index 'sek-drop s n)
  (if (psek? s)
      (psek-drop s n)
      (copy-slice s n (- (sek-length s) n))))

(define (sek-copy s #:mode [mode 'copy])
  (check-sek 'sek-copy s)
  (unless (memq mode '(copy share))
    (raise-argument-error 'sek-copy "(or/c 'share 'copy)" mode))
  (if (psek? s)
      s
      (esek-copy s #:mode mode)))

;; ------------------------------------------------------- bulk writes

;; Overwrite `size` elements starting at `start` with x.  Uses writable
;; segments, so the cost is O(size + K.log n) rather than one tree descent
;; per element -- plus, for each chunk in the range shared with a snapshot,
;; the cost of taking ownership of it.
(define (esek-fill! e x [start 0] [end #f])
  (unless (esek? e)
    (raise-argument-error 'esek-fill! "esek?" e))
  (define n (esek-length e))
  (define stop (or end n))
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? stop)
               (<= start stop n))
    (raise-arguments-error 'esek-fill!
                           "range outside the sequence"
                           "start"
                           start
                           "end"
                           stop
                           "length"
                           n))
  (define size (- stop start))
  (unless (eqv? size 0)
    (define it (sek-iterator-at-sentinel e 'front))
    (sek-iter-reach! it start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define sg (sek-iter-writable-segment it 'forward))
        (define k (min remaining (segment-length sg)))
        ;; the segment constructor checked that the run fits in its vector
        (define v (segment-vector sg))
        (define o (segment-start sg))
        (for ([j (in-fx-range o (unsafe-fx+ o k))])
          (unsafe-vector*-set! v j x))
        (sek-iter-jump! it 'forward k)
        (loop (- remaining k))))))

;; Replace each element with the result of proc on it, in place, as
;; `vector-map!` and `mutable-treelist-map!` do; the /index version passes
;; the index as well.  Like esek-fill!, this writes through writable segments,
;; one run at a time.
;;
;; proc is the caller's code, and runs between the writes to one run, so after
;; each call two things are checked.  An update to e invalidates the iterator,
;; and raises here as it would in any traversal.  A snapshot of e is a read,
;; and leaves the iterator valid, but it takes ownership of every chunk away
;; from e: writing on into the run would change the snapshot.  So the map then
;; writes through the iterator instead, which copies the chunk, and takes the
;; rest of the run afresh.
(define (esek-map! e proc)
  (unless (esek? e) (raise-argument-error 'esek-map! "esek?" e))
  (check-procedure 'esek-map! proc 1)
  (map-in-place! 'esek-map! e (lambda (i x) (proc x))))

(define (esek-map/index! e proc)
  (unless (esek? e) (raise-argument-error 'esek-map/index! "esek?" e))
  (check-procedure 'esek-map/index! proc 2)
  (map-in-place! 'esek-map/index! e proc))

(define (map-in-place! who e proc)
  (define it (sek-iterator e 'forward))
  (let loop ([i 0])
    (unless (sek-iter-finished? it)
      (define sg (sek-iter-writable-segment it 'forward))
      (define id (esek-id e))
      (define v (segment-vector sg))
      (define o (segment-start sg))
      (define k (segment-length sg))
      (let run ([j 0])
        (cond
          [(unsafe-fx= j k)
           (sek-iter-jump! it 'forward k)
           (loop (unsafe-fx+ i k))]
          [else
           (define y (proc (unsafe-fx+ i j) (unsafe-vector*-ref v (unsafe-fx+ o j))))
           (cond
             [(not (sek-iter-valid? it)) (raise-updated who)]
             [(eq? (esek-id e) id)
              (unsafe-vector*-set! v (unsafe-fx+ o j) y)
              (run (unsafe-fx+ j 1))]
             [else
              (sek-iter-jump! it 'forward j)
              (sek-iter-set! it y)
              (sek-iter-move! it 'forward)
              (loop (unsafe-fx+ i (unsafe-fx+ j 1)))])])))))

;; Shaped like `vector-copy!`: the destination and its start, then the source
;; and the range of it to take.
(define (esek-copy! dst dst-start src [src-start 0] [src-end #f])
  (unless (esek? dst)
    (raise-argument-error 'esek-copy! "esek?" dst))
  (check-sek 'esek-copy! src)
  (define src-stop (or src-end (sek-length src)))
  (unless (and (exact-nonnegative-integer? src-start)
               (exact-nonnegative-integer? dst-start)
               (exact-nonnegative-integer? src-stop)
               (<= src-start src-stop (sek-length src))
               (<= (+ dst-start (- src-stop src-start)) (esek-length dst)))
    (raise-arguments-error 'esek-copy!
                           "range outside a sequence"
                           "dst-start"
                           dst-start
                           "src-start"
                           src-start
                           "src-end"
                           src-stop))
  (define size (- src-stop src-start))
  (unless (eqv? size 0)
    ;; take a snapshot when the two sequences might be the same, so that
    ;; overlapping ranges behave as if the source were read first
    (define source
      (if (eq? src dst)
          (esek-snapshot dst)
          src))
    (define si (sek-iterator-at-sentinel source 'front))
    (sek-iter-reach! si src-start)
    (define di (sek-iterator-at-sentinel dst 'front))
    (sek-iter-reach! di dst-start)
    (let loop ([remaining size])
      (unless (eqv? remaining 0)
        (define dseg (sek-iter-writable-segment di 'forward))
        (define sseg (sek-iter-segment si 'forward))
        (define k (min remaining (segment-length dseg) (segment-length sseg)))
        (define so (segment-start sseg))
        (vector-copy! (segment-vector dseg) (segment-start dseg)
                      (segment-vector sseg) so (+ so k))
        (sek-iter-jump! si 'forward k)
        (sek-iter-jump! di 'forward k)
        (loop (- remaining k))))))

;; ------------------------------------------------------------ construction

;; Build from any Racket sequence -- the OCaml library's of_seq.  With n, take
;; at most the first n elements.
(define (sequence->esek seq [n #f])
  (psek-edit (sequence->psek-builder 'sequence->esek seq n)))

(define (sequence->psek seq [n #f])
  (sequence->psek-builder 'sequence->psek seq n))

(define (sequence->psek-builder who seq n)
  (unless (sequence? seq)
    (raise-argument-error who "sequence?" seq))
  (unless (or (not n) (exact-nonnegative-integer? n))
    (raise-argument-error who "(or/c #f exact-nonnegative-integer?)" n))
  (define b (make-psek-builder))
  (if n
      (for ([x seq] [_ (in-range n)]) (psek-builder-add! b x))
      (for ([x seq]) (psek-builder-add! b x)))
  (psek-builder-close b))

(define (check-build who n proc)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error who "exact-nonnegative-integer?" n))
  (check-procedure who proc 1))

(define (build-psek n proc)
  (check-build 'build-psek n proc)
  (psek-build n proc))

(define (build-esek n proc)
  (check-build 'build-esek n proc)
  (esek-build n proc))

(define (make-psek n [v #f])
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'make-psek "exact-nonnegative-integer?" n))
  (psek-build n (lambda (_) v)))

;; Comprehensions, in the shape of for/gvector and for/mutable-treelist.
(define-syntax (for/esek stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-psek-builder)])
         (for/fold/derived #,stx () clauses body ... (psek-builder-add! acc tail-expr) (values))
         (psek-edit (psek-builder-close acc))))]))

(define-syntax (for*/esek stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-psek-builder)])
         (for*/fold/derived #,stx () clauses body ... (psek-builder-add! acc tail-expr) (values))
         (psek-edit (psek-builder-close acc))))]))

;; The persistent forms close the builder and stop: going through for/esek
;; and a snapshot would turn the result into an ephemeral sequence and back.
(define-syntax (for/psek stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-psek-builder)])
         (for/fold/derived #,stx () clauses body ... (psek-builder-add! acc tail-expr) (values))
         (psek-builder-close acc)))]))

(define-syntax (for*/psek stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (quasisyntax/loc stx
       (let ([acc (make-psek-builder)])
         (for*/fold/derived #,stx () clauses body ... (psek-builder-add! acc tail-expr) (values))
         (psek-builder-close acc)))]))
