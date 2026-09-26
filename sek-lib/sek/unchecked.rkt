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
;; keeps its checks.
(require (for-syntax racket/base racket/syntax)
         racket/fixnum
         racket/performance-hint
         racket/unsafe/ops)

(provide struct/unchecked
         (except-out (all-from-out racket/fixnum)
                     fx+ fx- fx* fx= fx< fx<= fx> fx>= fxand fxior fxxor
                     fxlshift fxrshift fxquotient fxremainder fxmodulo fxmin fxmax)
         (rename-out [unsafe-fx+ fx+] [unsafe-fx- fx-] [unsafe-fx* fx*]
                     [unsafe-fx= fx=] [unsafe-fx< fx<] [unsafe-fx<= fx<=]
                     [unsafe-fx> fx>] [unsafe-fx>= fx>=]
                     [unsafe-fxand fxand] [unsafe-fxior fxior] [unsafe-fxxor fxxor]
                     [unsafe-fxlshift fxlshift] [unsafe-fxrshift fxrshift]
                     [unsafe-fxquotient fxquotient] [unsafe-fxremainder fxremainder]
                     [unsafe-fxmodulo fxmodulo]
                     [unsafe-fxmin fxmin] [unsafe-fxmax fxmax]))

;; (struct/unchecked name (field ...) option ...)
;;
;; Like `struct`, except that `name-field` and `set-name-field!` read and write
;; the field without checking that their argument is a `name`.  The type is
;; defined under a hidden name; `name` is still its constructor (unless
;; #:constructor-name says otherwise) and `name?` its predicate.
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
            [hidden (format-id #'name "~a/checked" #'name)]
            [ref (lambda (f) (format-id #'name "~a-~a" #'name (field-id f)))]
            [set (lambda (f) (format-id #'name "set-~a-~a!" #'name (field-id f)))])
       (with-syntax ([hidden hidden]
                     [hidden? (format-id #'name "~a?" hidden)]
                     [name? (format-id #'name "~a?" #'name)]
                     [(extra ...)
                      (append
                       (if (memq '#:constructor-name options) '() (list #'#:constructor-name #'name))
                       (if (memq '#:reflection-name options) '() (list #'#:reflection-name #''name)))]
                     [(accessor ...)
                      (for/list ([f (in-list fields)] [i (in-naturals)])
                        (with-syntax ([ref (ref f)] [i i])
                          #'(define (ref x) (unsafe-struct*-ref x i))))]
                     [(mutator ...)
                      (for/list ([f (in-list fields)] [i (in-naturals)] #:when (mutable? f))
                        (with-syntax ([set (set f)] [i i])
                          #'(define (set x v) (unsafe-struct*-set! x i v))))])
         #'(begin
             (struct hidden (field ...) option ... extra ...)
             (define name? hidden?)
             (begin-encourage-inline
               accessor ...
               mutator ...))))]))
