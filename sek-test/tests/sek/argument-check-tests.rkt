#lang racket/base
;; Every public operation must check its principal argument.
;;
;; The accessors of sek-lib's own structures do not check their argument
;; (see sek/private/unchecked), so one handed the wrong kind of value does not
;; raise -- it reads whatever is at that field offset.  An operation that
;; reaches an accessor without testing its argument is therefore reading
;; arbitrary memory, and on some platforms that read lands on something
;; harmless and passes.
;;
;; `error-tests.rkt` lists the operations by hand, so it only covers what
;; someone remembered to add.  This derives the list from the module's exports
;; instead, which is what makes it a guard: an operation added later is
;; covered the day it is exported.
;;
;; The property tested is that the operation blames *itself*.  An operation
;; cannot name itself in an error without having checked, so this catches a
;; missing check even when the unchecked read happens not to crash -- which is
;; the case a crash test would miss.
(require rackunit
         racket/list
         racket/string
         racket/set
         sek)

(provide run-argument-check-tests)

;; Values of the shapes a caller is most likely to pass by mistake.  None is a
;; sequence, an iterator or a segment.
(define bad-values (list 7 '(1 2) (vector 1 2) "s"))

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'sek))

(define (value-of name)
  (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
    (parameterize ([current-namespace ns])
      (namespace-variable-value name))))

(define exports
  (let-values ([(vars stxs) (module->exports 'sek)])
    (sort (for*/list ([ph (in-list vars)]
                      [e (in-list (cdr ph))])
            (car e))
          symbol<?)))

;; A predicate answers #f instead of raising, and a constructor or conversion
;; takes what it is given; neither is a missing check.  The named exceptions
;; are aliases, which blame the operation they are an alias for.
(define exempt
  (set 'psek
       'esek
       'segment
       'psek-push-front
       'psek-push-back
       'esek-push-front!
       'esek-push-back!))

(define (exempt? name)
  (define s (symbol->string name))
  (or (set-member? exempt name)
      (string-suffix? s "?")
      (string-prefix? s "sequence->")
      (string-prefix? s "list->")
      (string-prefix? s "vector->")
      (string-prefix? s "make-")
      (string-prefix? s "build-")))

;; Arguments after the first, which only have to be harmless: the check under
;; test is on the first one, and it has to happen before anything looks at
;; these.
(define (fillers n)
  (for/list ([i (in-range n)])
    0))

;; The `who` of an error, which for `raise-argument-error` is the text before
;; the first colon.
(define (blamed-by thunk)
  (with-handlers ([exn:fail? (lambda (e) (car (string-split (exn-message e) ":")))]
                  [(lambda (_) #t) (lambda (_) "a non-exn raise")])
    (call-with-values thunk (lambda vs "no error at all"))))

(define (run-argument-check-tests)
  (define probed 0)
  (for ([name (in-list exports)])
    (define proc (value-of name))
    (when (and (procedure? proc) (not (exempt? name)))
      (define arity
        (for/first ([n (in-range 1 6)]
                    #:when (bitwise-bit-set? (procedure-arity-mask proc) n))
          n))
      (when arity
        (set! probed (add1 probed))
        (for ([bad (in-list bad-values)])
          (check-equal? (blamed-by (lambda () (apply proc bad (fillers (sub1 arity)))))
                        (symbol->string name)
                        (format "~a must check its first argument, given ~s" name bad))))))
  ;; If the export list is ever filtered down to nothing by a change to the
  ;; exemptions above, the loop passes vacuously and the guard is gone.
  (check-true (> probed 50) (format "expected to probe most of the library, probed ~a" probed)))

(module+ test
  (run-argument-check-tests))
