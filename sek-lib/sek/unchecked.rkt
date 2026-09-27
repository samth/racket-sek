#lang racket/base
;; The library is compiled in safe mode.  Two kinds of operation are unchecked
;; instead, because nothing -- not even a data race -- can give them a value of
;; the wrong kind:
;;
;;   * the accessors and mutators of the library's own structure types.  A
;;     field only ever holds a value of its declared kind: every write stores a
;;     whole value, and the entry points check that a caller's argument is the
;;     structure it claims to be before any accessor sees it.
;;
;;   * fixnum arithmetic and comparison.  The operands are fields that hold
;;     fixnums or indices the entry points have checked.  A result that
;;     overflowed would wrap, which would be a wrong answer but not a memory
;;     error, and an index reaches a vector only through `checked-index` in
;;     chunk.rkt or a range the iterator checked when it was installed.
;;
;; Everything else -- vector references, pair operations, generic arithmetic --
;; keeps its checks, except the unchecked vector operations that the modules
;; take from here, each of which is guarded where it is used.
;;
;; This module is the only one in the library that requires
;; racket/unsafe/ops, so that two builds for testing can replace every
;; unchecked operation at once.  Both are chosen when the library is compiled,
;; by environment variables; compile such a build into its own directory, since
;; `raco make` does not notice a change of environment.
;;
;;   SEK_CHECKED=1  Every unchecked operation first checks the precondition
;;                  that makes it safe, and raises an error whose message
;;                  begins "sek: unchecked-operation precondition violated" if
;;                  it does not hold.  A fixnum operation whose result would
;;                  overflow raises one beginning "sek: unchecked fixnum
;;                  operation overflowed" instead, since wrapping is a wrong
;;                  answer rather than a memory error.  Running the tests and
;;                  the race fuzzer on this build tests the argument above
;;                  directly: any race that would hand an unchecked operation a
;;                  bad argument is reported at that operation.
;;
;;   SEK_JITTER=1   `(jitter!)`, placed at the points where concurrent
;;                  operations can interleave, delays for a random moment, so
;;                  that races happen far more often than they would.
;;
;; With neither set, this module provides the primitives themselves.
(require (for-syntax racket/base racket/syntax)
         racket/fixnum
         racket/performance-hint
         racket/unsafe/ops)

(begin-for-syntax
  (define (flag name)
    (let ([v (getenv name)]) (and v (not (member v '("" "0"))))))
  (define checked-build? (flag "SEK_CHECKED"))
  (define jitter-build? (flag "SEK_JITTER")))

;; (provide-op name fast slow): provide `fast` as `name` in a normal build,
;; and `slow` in a checked one
(define-syntax (provide-op stx)
  (syntax-case stx ()
    [(_ name fast slow)
     (with-syntax ([impl (if checked-build? #'slow #'fast)])
       #'(provide (rename-out [impl name])))]))

(provide struct/unchecked
         in-fx-range
         jitter!
         (except-out (all-from-out racket/fixnum)
                     fx+ fx- fx* fx= fx< fx<= fx> fx>= fxand fxior fxxor
                     fxlshift fxrshift fxquotient fxremainder fxmodulo fxmin fxmax
                     fxabs))

;; ------------------------------------------------ the checked implementations

(define (violated who . args)
  (error 'sek "unchecked-operation precondition violated: ~a applied to ~e" who args))

(define (vector-ok? v) (and (vector? v) (not (impersonator? v))))

(define (checked-vector*-length v)
  (unless (vector-ok? v) (violated 'unsafe-vector*-length v))
  (vector-length v))
(define (checked-vector*-ref v i)
  (unless (and (vector-ok? v) (fixnum? i) (<= 0 i) (< i (vector-length v)))
    (violated 'unsafe-vector*-ref v i))
  (vector-ref v i))
(define (checked-vector*-set! v i x)
  (unless (and (vector-ok? v) (fixnum? i) (<= 0 i) (< i (vector-length v)))
    (violated 'unsafe-vector*-set! v i))
  (vector-set! v i x))
(define (checked-vector*-set/copy v i x)
  (unless (and (vector-ok? v) (fixnum? i) (<= 0 i) (< i (vector-length v)))
    (violated 'unsafe-vector*-set/copy v i))
  (define w (make-vector (vector-length v)))
  (vector-copy! w 0 v)
  (vector-set! w i x)
  w)

;; The racket/fixnum operations take any number of arguments, and so do these.
(define-syntax-rule (define-checked-fx (name who generic) ...)
  (begin
    (define (name . args)
      (unless (andmap fixnum? args) (apply violated 'who args))
      (let ([r (apply generic args)])
        (unless (fixnum? r)
          (error 'sek "unchecked fixnum operation overflowed: ~a applied to ~e" 'who args))
        r))
    ...))
(define-checked-fx
  (checked-fx+ fx+ +) (checked-fx- fx- -) (checked-fx* fx* *)
  (checked-fxand fxand bitwise-and) (checked-fxior fxior bitwise-ior)
  (checked-fxxor fxxor bitwise-xor) (checked-fxmin fxmin min) (checked-fxmax fxmax max))

(define-syntax-rule (define-checked-fx-compare (name who generic) ...)
  (begin
    (define (name . args)
      (unless (andmap fixnum? args) (apply violated 'who args))
      (apply generic args))
    ...))
(define-checked-fx-compare
  (checked-fx= fx= =) (checked-fx< fx< <) (checked-fx<= fx<= <=)
  (checked-fx> fx> >) (checked-fx>= fx>= >=))

(define (checked-fxlshift a n)
  (unless (and (fixnum? a) (fixnum? n) (<= 0 n 60)) (violated 'fxlshift a n))
  (let ([r (arithmetic-shift a n)])
    (unless (fixnum? r)
      (error 'sek "unchecked fixnum operation overflowed: fxlshift applied to ~e ~e" a n))
    r))
(define (checked-fxrshift a n)
  (unless (and (fixnum? a) (fixnum? n) (<= 0 n 60)) (violated 'fxrshift a n))
  (arithmetic-shift a (- n)))
(define-syntax-rule (define-checked-fx-division (name who generic) ...)
  (begin
    (define (name a b)
      (unless (and (fixnum? a) (fixnum? b) (not (eqv? b 0))) (violated 'who a b))
      (generic a b))
    ...))
(define-checked-fx-division
  (checked-fxquotient fxquotient quotient)
  (checked-fxremainder fxremainder remainder)
  (checked-fxmodulo fxmodulo modulo))
(define (checked-fxabs a)
  (unless (fixnum? a) (violated 'fxabs a))
  (abs a))

;; The fixnum operations under their racket/fixnum names, and under the unsafe
;; names that some modules spell out.
(provide-op fx+ unsafe-fx+ checked-fx+)
(provide-op fx- unsafe-fx- checked-fx-)
(provide-op fx* unsafe-fx* checked-fx*)
(provide-op fx= unsafe-fx= checked-fx=)
(provide-op fx< unsafe-fx< checked-fx<)
(provide-op fx<= unsafe-fx<= checked-fx<=)
(provide-op fx> unsafe-fx> checked-fx>)
(provide-op fx>= unsafe-fx>= checked-fx>=)
(provide-op fxand unsafe-fxand checked-fxand)
(provide-op fxior unsafe-fxior checked-fxior)
(provide-op fxxor unsafe-fxxor checked-fxxor)
(provide-op fxlshift unsafe-fxlshift checked-fxlshift)
(provide-op fxrshift unsafe-fxrshift checked-fxrshift)
(provide-op fxquotient unsafe-fxquotient checked-fxquotient)
(provide-op fxremainder unsafe-fxremainder checked-fxremainder)
(provide-op fxmodulo unsafe-fxmodulo checked-fxmodulo)
(provide-op fxmin unsafe-fxmin checked-fxmin)
(provide-op fxmax unsafe-fxmax checked-fxmax)
(provide-op fxabs unsafe-fxabs checked-fxabs)
(provide-op unsafe-fx+ unsafe-fx+ checked-fx+)
(provide-op unsafe-fx- unsafe-fx- checked-fx-)
(provide-op unsafe-fx= unsafe-fx= checked-fx=)
(provide-op unsafe-fx< unsafe-fx< checked-fx<)
(provide-op unsafe-fx<= unsafe-fx<= checked-fx<=)
(provide-op unsafe-fx> unsafe-fx> checked-fx>)
(provide-op unsafe-fx>= unsafe-fx>= checked-fx>=)
(provide-op unsafe-fxabs unsafe-fxabs checked-fxabs)
(provide-op unsafe-vector*-ref unsafe-vector*-ref checked-vector*-ref)
(provide-op unsafe-vector*-set! unsafe-vector*-set! checked-vector*-set!)
(provide-op unsafe-vector*-length unsafe-vector*-length checked-vector*-length)
(provide-op unsafe-vector*-set/copy unsafe-vector*-set/copy checked-vector*-set/copy)

;; ------------------------------------------------------------------- jitter

;; A short pause at a random fraction of the points it is placed, drawn from a
;; generator shared by all threads.  The generator is updated with a
;; compare-and-swap whose failure is ignored, so that racing threads draw
;; different numbers without a lock -- `random` itself is not safe to call from
;; parallel threads.
(define jitter-state (box 1))
(define jitter-sink (box 0))
(define (jitter-now!)
  (define s (unbox jitter-state))
  (define x (bitwise-and (+ (* s 1103515245) 12345) #x3FFFFFFF))
  (box-cas! jitter-state s x)
  (when (< (bitwise-and x 7) 3)
    (let loop ([i (bitwise-and (arithmetic-shift x -3) 1023)] [acc 0])
      (if (eqv? i 0)
          (set-box! jitter-sink acc)
          (loop (sub1 i) (+ acc i)))))
  (when (eqv? (bitwise-and x #xFFF) 0)
    (sleep 0)))

(define-syntax (jitter! stx)
  (syntax-case stx ()
    [(_) (if jitter-build? #'(jitter-now!) #'(void))]))

;; (in-fx-range end), (in-fx-range start end), (in-fx-range start end -1)
;;
;; Like `in-range` over fixnum bounds, in a `for` clause.  `in-range` chooses
;; unchecked fixnum operations for its loop only in a module compiled in unsafe
;; mode, so in this library it would test and add generically on every
;; iteration of the element loops.  The bounds here are lengths and indices
;; that are fixnums already, and the loops that use this read their elements
;; through checked indices or from runs checked once, beforehand.
(define (check-fx-bounds s e)
  (unless (and (fixnum? s) (fixnum? e)) (violated 'in-fx-range s e)))
(define-sequence-syntax in-fx-range
  (lambda () #'in-range)
  (lambda (stx)
    (with-syntax ([(check ...) (if checked-build? #'((check-fx-bounds s e)) #'())])
      (syntax-case stx ()
        [[(id) (_ end)]
         #'[(id) (in-fx-range 0 end)]]
        [[(id) (_ start end)]
         #'[(id) (:do-in ([(s) start] [(e) end])
                         (begin check ... #t)
                         ([i s])
                         (unsafe-fx< i e)
                         ([(id) i])
                         #t
                         #t
                         [(unsafe-fx+ i 1)])]]
        [[(id) (_ start end step)]
         (eqv? (syntax-e #'step) -1)
         #'[(id) (:do-in ([(s) start] [(e) end])
                         (begin check ... #t)
                         ([i s])
                         (unsafe-fx> i e)
                         ([(id) i])
                         #t
                         #t
                         [(unsafe-fx- i 1)])]]
        [_ #f]))))

;; (struct/unchecked name (field ...) option ...)
;;
;; Like `struct`, except that `name-field` and `set-name-field!` read and write
;; the field without checking that their argument is a `name`.  A mutable field
;; also gets `name-field-cas!`, a compare-and-swap on the field.  The type is
;; defined under a hidden name; `name` is still its constructor (unless
;; #:constructor-name says otherwise) and `name?` its predicate.  In a checked
;; build, each of them checks that its argument is a `name` after all.
(define-syntax (struct/unchecked stx)
  (syntax-case stx ()
    [(_ name (field ...) option ...)
     (let* ([fields (syntax->list #'(field ...))]
            [field-id (lambda (f) (syntax-case f () [(id . _) #'id] [id #'id]))]
            [mutable? (lambda (f)
                        (syntax-case f ()
                          [(id kw ...) (memq '#:mutable (syntax->datum #'(kw ...)))]
                          [_ #f]))]
            [options (syntax->datum #'(option ...))]
            [hidden-id (format-id #'name "~a/checked" #'name)]
            [ref (lambda (f) (format-id #'name "~a-~a" #'name (field-id f)))]
            [set (lambda (f) (format-id #'name "set-~a-~a!" #'name (field-id f)))]
            [cas (lambda (f) (format-id #'name "~a-~a-cas!" #'name (field-id f)))])
       (with-syntax* ([hidden hidden-id]
                      [hidden? (format-id #'name "~a?" hidden-id)]
                      [name? (format-id #'name "~a?" #'name)]
                      [(guard ...) (if checked-build?
                                       #'((unless (hidden? x) (violated 'name x)))
                                       #'())]
                      [(extra ...)
                       (append
                        (if (memq '#:constructor-name options) '() (list #'#:constructor-name #'name))
                        (if (memq '#:reflection-name options) '() (list #'#:reflection-name #''name)))]
                      [(accessor ...)
                       (for/list ([f (in-list fields)] [i (in-naturals)])
                         (with-syntax ([ref (ref f)] [i i])
                           #'(define (ref x) guard ... (unsafe-struct*-ref x i))))]
                      [(mutator ...)
                       (apply append
                              (for/list ([f (in-list fields)] [i (in-naturals)] #:when (mutable? f))
                                (with-syntax ([set (set f)] [cas (cas f)] [i i])
                                  (list #'(define (set x v) guard ... (unsafe-struct*-set! x i v))
                                        #'(define (cas x old new)
                                            guard ...
                                            (unsafe-struct*-cas! x i old new))))))])
         #'(begin
             (struct hidden (field ...) option ... extra ...)
             (define name? hidden?)
             (begin-encourage-inline
               accessor ...
               mutator ...))))]))
