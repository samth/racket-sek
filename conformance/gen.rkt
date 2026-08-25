#lang racket/base
;; Generates a random script for the differential harness.  A list model of
;; every slot is maintained so that structural commands get in-range
;; arguments; the commands whose failure both drivers report symmetrically
;; (get, set, pop, peek, find) are also given out-of-range arguments on
;; purpose.

(require racket/list
         racket/string)

(define nslots 6)
(define maxlen 260)

(define me (make-vector nslots '()))
(define mp (make-vector nslots '()))
(define counter 0)
(define (next!)
  (set! counter (add1 counter))
  counter)

(define out '())
(define (emit! . toks)
  (set! out (cons (string-join (map (lambda (t) (format "~a" t)) toks) " ") out)))

(define (slot)
  (random nslots))
(define (E i)
  (vector-ref me i))
(define (E! i v)
  (vector-set! me i v))
(define (P i)
  (vector-ref mp i))
(define (P! i v)
  (vector-set! mp i v))

(define (idx-in l)
  (if (null? l)
      0
      (random (length l))))
(define (cut-in l)
  (random (add1 (length l))))
(define (uniq-adjacent l)
  (cond
    [(null? l) '()]
    [else
     (let loop ([xs (cdr l)]
                [prev (car l)]
                [acc (list (car l))])
       (cond
         [(null? xs) (reverse acc)]
         [(equal? (car xs) prev) (loop (cdr xs) prev acc)]
         [else (loop (cdr xs) (car xs) (cons (car xs) acc))]))]))
(define (merge2 a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(< (car b) (car a)) (cons (car b) (merge2 a (cdr b)))]
    [else (cons (car a) (merge2 (cdr a) b))]))
(define (replace-range l start len x)
  (append (take l start) (build-list len (lambda (_) x)) (drop l (+ start len))))
(define (blit-into dst dstart src sstart len)
  (append (take dst dstart) (take (drop src sstart) len) (drop dst (+ dstart len))))

(define (step!)
  (define k (random 44))
  (case k
    [(0 1 2 3)
     (define i (slot))
     (define s (random 2))
     (define x (next!))
     (when (< (length (E i)) maxlen)
       (emit! "epush" i s x)
       (E! i
           (if (eqv? s 0)
               (cons x (E i))
               (append (E i) (list x)))))]
    [(4 5)
     (define i (slot))
     (define s (random 2))
     (emit! "epop" i s)
     (unless (null? (E i))
       (E! i
           (if (eqv? s 0)
               (cdr (E i))
               (drop-right (E i) 1))))]
    [(6)
     (define i (slot))
     (emit! "epeek" i (random 2))]
    [(7)
     (define i (slot))
     (emit! "eget"
            i
            (if (zero? (random 8))
                (+ 5 (length (E i)))
                (idx-in (E i))))]
    [(8)
     (define i (slot))
     (define x (next!))
     (define j
       (if (zero? (random 8))
           (+ 5 (length (E i)))
           (idx-in (E i))))
     (emit! "eset" i j x)
     (when (< j (length (E i)))
       (E! i (append (take (E i) j) (list x) (drop (E i) (add1 j)))))]
    [(9) (emit! "elen" (slot))]
    [(10) (emit! "eempty" (slot))]
    [(11)
     (define i (slot))
     (define j (slot))
     (emit! "eassign" i j)
     (unless (eqv? i j)
       (E! i (E j))
       (E! j '()))]
    [(12)
     (define i (slot))
     (define j (slot))
     (emit! (if (zero? (random 2)) "ecopy" "ecopyshare") i j)
     (E! i (E j))]
    [(13 14)
     (define i (slot))
     (define j (slot))
     (define s (random 2))
     (when (and (not (eqv? i j)) (<= (+ (length (E i)) (length (E j))) maxlen))
       (emit! "eappend" i j s)
       (define r
         (if (eqv? s 0)
             (append (E j) (E i))
             (append (E i) (E j))))
       (E! j '())
       (E! i r))]
    [(15)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (when (and (not (eqv? j l)) (<= (+ (length (E j)) (length (E l))) maxlen))
       (emit! "econcat" i j l)
       (define r (append (E j) (E l)))
       (E! j '())
       (E! l '())
       (E! i r))]
    [(16)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (define n (cut-in (E l)))
     (emit! "esplit" i j l n)
     (define a (take (E l) n))
     (define b (drop (E l) n))
     (E! l '())
     (E! i a)
     (E! j b)]
    [(17)
     (define i (slot))
     (define j (slot))
     (define s (random 2))
     (define n (cut-in (E j)))
     (emit! "ecarve" i j s n)
     (define a (take (E j) n))
     (define b (drop (E j) n))
     (if (eqv? s 1)
         (begin
           (E! j a)
           (E! i b))
         (begin
           (E! j b)
           (E! i a)))]
    [(18)
     (define i (slot))
     (define s (random 2))
     (define n (cut-in (E i)))
     (emit! "etake" i s n)
     (E! i
         (if (eqv? s 0)
             (take (E i) n)
             (drop (E i) n)))]
    [(19)
     (define i (slot))
     (define s (random 2))
     (define n (cut-in (E i)))
     (emit! "edrop" i s n)
     (E! i
         (if (eqv? s 0)
             (drop (E i) n)
             (take (E i) n)))]
    [(20)
     (define i (slot))
     (define j (slot))
     (define st (cut-in (E j)))
     (define len (random (add1 (- (length (E j)) st))))
     (emit! "esub" i j st len)
     (E! i (take (drop (E j) st) len))]
    [(21)
     (define i (slot))
     (define st (cut-in (E i)))
     (define len (random (add1 (- (length (E i)) st))))
     (define x (next!))
     (emit! "efill" i st len x)
     (E! i (replace-range (E i) st len x))]
    [(22)
     (define i (slot))
     (define j (slot))
     (define len (min (length (E i)) (length (E j))))
     (when (> len 0)
       (define l (add1 (random len)))
       (define ss (random (add1 (- (length (E j)) l))))
       (define ds (random (add1 (- (length (E i)) l))))
       (emit! "eblit" i j ss ds l)
       (E! i (blit-into (E i) ds (E j) ss l)))]
    [(23 24)
     (define i (slot))
     (define j (slot))
     (emit! "esnap" i j)
     (P! i (E j))]
    [(25)
     (define i (slot))
     (define j (slot))
     (emit! "esnapclear" i j)
     (P! i (E j))
     (E! j '())]
    [(26 27)
     (define i (slot))
     (define j (slot))
     (emit! "eedit" i j)
     (E! i (P j))]
    [(28) (emit! "edump" (slot))]
    [(29) (emit! "edumpiter" (slot) (random 2))]
    [(30) (emit! "efold" (slot))]
    [(31)
     (define i (slot))
     (define j (slot))
     (case (random 6)
       [(0)
        (emit! "emap" i j)
        (E! i (map (lambda (x) (+ (* x 2) 1)) (E j)))]
       [(1)
        (emit! "emapi" i j)
        (E! i
            (for/list ([x (in-list (E j))]
                       [n (in-naturals)])
              (+ (* n 100) x)))]
       [(2)
        (emit! "efilter" i j)
        (E! i (filter (lambda (x) (not (zero? (modulo x 3)))) (E j)))]
       [(3)
        (emit! "efiltermap" i j)
        (E! i (filter-map (lambda (x) (and (even? x) (quotient x 2))) (E j)))]
       [(4)
        (emit! "erev" i j)
        (E! i (reverse (E j)))]
       [(5)
        (emit! "euniq" i j)
        (E! i (uniq-adjacent (E j)))])]
    [(32)
     (define i (slot))
     (emit! "esort" i)
     (E! i (sort (E i) <))]
    [(33)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (when (<= (+ (length (E j)) (length (E l))) maxlen)
       (emit! "emerge" i j l)
       (E! j (sort (E j) <))
       (E! l (sort (E l) <))
       (E! i (merge2 (E j) (E l))))]
    [(34)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (emit! "epart" i j l)
     (define a (filter even? (E l)))
     (define b (filter odd? (E l)))
     (E! i a)
     (E! j b)]
    [(35) (emit! "ezip" (slot) (slot) (slot))]
    [(36)
     (define i (slot))
     (case (random 6)
       [(0) (emit! "efind" i (random 2) (random 40))]
       [(1) (emit! "eforall" i (random 40))]
       [(2) (emit! "eexists" i (random 40))]
       [(3) (emit! "emem" i (random 40))]
       [(4) (emit! "eequal" i (slot))]
       [(5) (emit! "ecompare" i (slot))])]
    ;; ---- persistent
    [(37 38)
     (define i (slot))
     (define s (random 2))
     (define x (next!))
     (when (< (length (P i)) maxlen)
       (emit! "ppush" i s x)
       (P! i
           (if (eqv? s 0)
               (cons x (P i))
               (append (P i) (list x)))))]
    [(39)
     (define i (slot))
     (define s (random 2))
     (emit! "ppop" i s)
     (unless (null? (P i))
       (P! i
           (if (eqv? s 0)
               (cdr (P i))
               (drop-right (P i) 1))))]
    [(40)
     (define i (slot))
     (case (random 5)
       [(0) (emit! "ppeek" i (random 2))]
       [(1)
        (emit! "pget"
               i
               (if (zero? (random 8))
                   (+ 5 (length (P i)))
                   (idx-in (P i))))]
       [(2)
        (define j
          (if (zero? (random 8))
              (+ 5 (length (P i)))
              (idx-in (P i))))
        (define x (next!))
        (emit! "pset" i j x)
        (when (< j (length (P i)))
          (P! i (append (take (P i) j) (list x) (drop (P i) (add1 j)))))]
       [(3) (emit! "plen" i)]
       [(4) (emit! "pdump" i)])]
    [(41)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (when (<= (+ (length (P j)) (length (P l))) maxlen)
       (emit! "pconcat" i j l)
       (P! i (append (P j) (P l))))]
    [(42)
     (define i (slot))
     (define j (slot))
     (define l (slot))
     (define n (cut-in (P l)))
     (case (random 5)
       [(0)
        (emit! "psplit" i j l n)
        (P! i (take (P l) n))
        (P! j (drop (P l) n))]
       [(1)
        (define s (random 2))
        (emit! "ptake" i l s n)
        (P! i
            (if (eqv? s 0)
                (take (P l) n)
                (drop (P l) n)))]
       [(2)
        (define s (random 2))
        (emit! "pdrop" i l s n)
        (P! i
            (if (eqv? s 0)
                (drop (P l) n)
                (take (P l) n)))]
       [(3)
        (define st (cut-in (P l)))
        (define len (random (add1 (- (length (P l)) st))))
        (emit! "psub" i l st len)
        (P! i (take (drop (P l) st) len))]
       [(4)
        (case (random 5)
          [(0)
           (emit! "pmap" i l)
           (P! i (map (lambda (x) (+ (* x 2) 1)) (P l)))]
          [(1)
           (emit! "pfilter" i l)
           (P! i (filter (lambda (x) (not (zero? (modulo x 3)))) (P l)))]
          [(2)
           (emit! "prev" i l)
           (P! i (reverse (P l)))]
          [(3)
           (emit! "psort" i l)
           (P! i (sort (P l) <))]
          [(4) (emit! "pequal" i l)])])]
    [(43)
     (case (random 4)
       [(0) (emit! "pdumpiter" (slot) (random 2))]
       [(1) (emit! "pfold" (slot))]
       [(2) (emit! "piterwalk" (slot) (random 20))]
       [(3)
        (define i (slot))
        (define d (add1 (random 5)))
        (emit! "eitersweep" i d)
        (E! i (map (lambda (x) (+ x d)) (E i)))])]
    [else (void)])
  ;; validate often, so a structural mistake shows up at its source
  (when (zero? (random 6))
    (emit! (if (zero? (random 2)) "echeck" "pcheck") (slot))))

(module+ main
  (define args (current-command-line-arguments))
  (define seed (string->number (vector-ref args 0)))
  (define steps
    (if (> (vector-length args) 1)
        (string->number (vector-ref args 1))
        1200))
  (random-seed seed)
  (for ([_ (in-range steps)])
    (step!))
  (for ([i (in-range nslots)])
    (emit! "edump" i)
    (emit! "pdump" i))
  (for ([line (in-list (reverse out))])
    (displayln line)))
