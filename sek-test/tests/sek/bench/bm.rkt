#lang racket/base
;; The benchmark from racket/data PR #34 (samth:gvector-fast), used there to
;; compare the revised gvector against treelist and mutable-treelist, with sek
;; added as a further contender.  Everything below the sek rows is upstream.
;;
;;   racket -y bm.rkt
;;
(require racket/list
         racket/treelist
         racket/vector
	 data/gvector
         racket/mutable-treelist
         sek)

(provide measure)

(define (measure M N [impls '(treelist mut-treelist vector gvector cons hasheq sek)])
  (printf "~s of length ~s\n" M N)

  (collect-garbage)
  
  (define-syntax-rule (bm impl who body)
    (when (memq 'impl impls)
      (display who)
      (display (make-string (max 0 (- 20 (string-length who))) #\space))
      (collect-garbage)
      (time body)
      (void)))

  (bm treelist "+ 1treelist-append"
      (let ([l (for/treelist ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (treelist-append l l))))

  (bm sek "+ 1pseq-append"
      (let ([l (for/pseq ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (pseq-append l l))))

  (bm vector "+ 1vector-append"
      (let ([l (for/vector ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector-append l l))))

  (bm treelist "+ 1vec->treelist"
      (let ([vec (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector->treelist vec))))

  (bm cons "+ cons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l null]) ([i (in-range 0 N)])
          (cons i l))))

  (bm cons "+ 1list-append"
      (let ([l (for/list ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (append l l))))

  (bm vector "+ for/vector #:len"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/vector #:length N ([i (in-range 0 N)])
                    i)))

  (bm vector "+ for/vec->tree"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (vector->treelist
         (for/vector #:length N ([i (in-range 0 N)]) i))))

  (bm treelist "+ 1list->treelist"
      (let ([lst (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (list->treelist lst))))

  (bm cons "+ for/list"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/list ([i (in-range 0 N)])
          i)))

  (bm vector "+ for/vector"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/vector ([i (in-range 0 N)])
          i)))

  (bm cons "+ listcons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l null]) ([i (in-range 0 N)])
          (if (list? l)
              (cons i l)
              (error "oops")))))

  (bm treelist "+ for/treelist"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/treelist ([i (in-range 0 N)])
          i)))

  (bm mut-treelist "+ for/mut-treelist"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/mutable-treelist ([i (in-range 0 N)])
          i)))

  (bm treelist "+ treelist-add"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l empty-treelist]) ([i (in-range 0 N)])
          (treelist-add l i))))

  (bm treelist "+ treelist-cons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l empty-treelist]) ([i (in-range 0 N)])
          (treelist-cons l i))))

  (bm treelist "+ for/list->treelst"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (list->treelist
         (for/list ([i (in-range 0 N)]) i))))

  (bm mut-treelist "+ mut-treelist-add!"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (let ([mtl (make-mutable-treelist 0)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-add! mtl i)))))

  (bm hasheq "+ hasheq-set"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([t #hasheq()]) ([i (in-range 0 N)])
          (hash-set t i i))))

  (bm hasheq "+ hasheq-set!"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (define t (make-hasheq))
        (for ([i (in-range 0 N)])
          (hash-set! t i i))))

  (bm gvector "+ for/gvector"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/gvector ([i (in-range 0 N)])
          i)))

  (bm gvector "+ gvector-add!"
      (for ([i (in-range 0 M)])
        (let ([gv (make-gvector)])
          (for ([i (in-range 0 N)])
            (gvector-add! gv i)))))

  (bm gvector "+ gvector-add! cap"
      (for ([i (in-range 0 M)])
        (let ([gv (make-gvector #:capacity N)])
          (for ([i (in-range 0 N)])
            (gvector-add! gv i)))))

  (bm sek "+ for/eseq"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/eseq ([i (in-range 0 N)])
          i)))

  (bm sek "+ eseq-push-back!"
      (for ([i (in-range 0 M)])
        (let ([e (make-eseq)])
          (for ([i (in-range 0 N)])
            (eseq-push-back! e i)))))

  (bm sek "+ eseq-push-front!"
      (for ([i (in-range 0 M)])
        (let ([e (make-eseq)])
          (for ([i (in-range 0 N)])
            (eseq-push-front! e i)))))

  (bm sek "+ pseq-push-back"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([s empty-pseq]) ([i (in-range 0 N)])
          (pseq-push-back s i))))

  (bm sek "+ 1list->pseq"
      (let ([lst (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (list->pseq lst))))

  (bm cons "- cdr"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (cdr l)))))

  (bm cons "- rest"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (rest l)))))

  (bm treelist "- treelist-rest"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (treelist-rest l)))))

  (bm sek "- pseq-pop-front"
      (let ([l (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (let-values ([(x l) (pseq-pop-front l)]) l)))))

  ;; edit is cheap, so what these measure is the popping, matching the way
  ;; the treelist and list rows above build their sequence once
  (bm sek "- eseq-pop-front!"
      (let ([src (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (let ([e (pseq-edit src)])
            (for ([i (in-range 0 N)])
              (eseq-pop-front! e))))))

  (bm sek "- eseq-pop-back!"
      (let ([src (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (let ([e (pseq-edit src)])
            (for ([i (in-range 0 N)])
              (eseq-pop-back! e))))))

  (when (N . <= . 1000)
    (bm vector "- vector-copy N-1"
        (let ([v (for/vector ([i (in-range 0 N)]) i)])
          (for/fold ([r #f]) ([i (in-range 0 M)])
            (for/fold ([v v]) ([i (in-range 0 N)])
              (vector-copy v 1))))))

  (bm hasheq "- hasheq-remove"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([t ht]) ([i (in-range 0 N)])
            (hash-remove t i)))))

  (bm cons "- 1list-tail 1/2"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (list-tail l (quotient N 2)))))

  (bm treelist "- 1treelst-drop 1/2"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (treelist-drop l (quotient N 2)))))

  (bm sek "- 1pseq-drop 1/2"
      (let ([l (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (sek-drop l (quotient N 2)))))

  (bm vector "- 1vector-copy 1/2"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector-copy v (quotient N 2)))))

  (bm vector "! vector-set!/fx"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (vector-set! v i (+ i j))))))

  (bm vector "! vector-set!/lit"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (vector-set! v i 17)))))


  (bm vector "! vector-set!/ptr"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (vector-set! v i "x")))))

  (bm gvector "! gvector-set!/fx"
      (let ([v (for/gvector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (gvector-set! v i (+ i j))))))

  (bm gvector "! gvector-set!/lit"
      (let ([v (for/gvector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (gvector-set! v i 17)))))

  (bm gvector "! gvector-set!/ptr"
      (let ([v (for/gvector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (gvector-set! v i "x")))))

  (bm mut-treelist "! mut-tree-set!/fx"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-set! l i (+ i j))))))

  (bm mut-treelist "! mut-tree-set!/lit"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-set! l i 17)))))

  (bm mut-treelist "! mut-tree-set!/ptr"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-set! l i "x")))))

  (bm sek "! eseq-set!/fx"
      (let ([e (for/eseq ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (eseq-set! e i (+ i j))))))

  (bm sek "! eseq-set!/lit"
      (let ([e (for/eseq ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (eseq-set! e i 17)))))

  (bm sek "! eseq-set!/ptr"
      (let ([e (for/eseq ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (eseq-set! e i "x")))))

  (bm sek "! eseq-fill!"
      (let ([e (for/eseq ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (eseq-fill! e 17))))

  (bm sek "! pseq-set"
      (let ([l (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([l l]) ([j (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (pseq-set l i (+ i j))))))

  (bm treelist "! treelist-set"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([l l]) ([j (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (treelist-set l i (+ i j))))))

  (bm hasheq "! hasheq-set"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([ht ht]) ([j (in-range 0 M)])
          (for/fold ([ht ht]) ([i (in-range 0 N)])
            (hash-set ht i (+ i j))))))

  (bm hasheq "! hasheq-set!"
      (let ([ht (hash-copy (for/hasheq ([i (in-range 0 N)]) (values i i)))])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (hash-set! ht i (+ i j))))))

  (when (N . <= . 10)
    (bm cons "! list-set"
        (let ([l (for/list ([i (in-range 0 N)]) i)])
          (for/fold ([l l]) ([j (in-range 0 M)])
            (for/fold ([l l]) ([i (in-range 0 N)])
              (list-set l i (+ i j)))))))

  (bm vector "^ in-vector"
      (let ([l (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-vector l)])
            i))))

  (bm cons "^ in-list"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-list l)])
            i))))

  (bm treelist "^ in-treelist"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-treelist l)])
            i))))
	  
  (bm mut-treelist "^ in-mut-treelist"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-mutable-treelist l)])
            i))))

  (bm gvector "^ in-gvector"
      (let ([l (for/gvector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-gvector l)])
            i))))

  (bm sek "^ in-eseq"
      (let ([l (for/eseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-eseq l)])
            i))))

  (bm sek "^ in-sek"
      (let ([l (for/eseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-sek l)])
            i))))

  (bm sek "^ sek-for-each"
      (let ([l (for/eseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (sek-for-each l void))))

  (bm hasheq "^ in-hash-keys"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-hash-keys ht)])
            i))))

  (bm hasheq "^ in-hash-keys!"
      (let ([ht (hash-copy (for/hasheq ([i (in-range 0 N)]) (values i i)))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-hash-keys ht)])
            i))))

  (bm vector "^ vector-ref"
      (let ([l (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (vector-ref l i)))))

  (when (N . <= . 1000)
    (bm cons "^ list-ref"
        (let ([l (for/list ([i (in-range 0 N)]) i)])
          (for/fold ([r #f]) ([i (in-range 0 M)])
            (for/fold ([v #f]) ([i (in-range 0 N)])
              (list-ref l i))))))

  (bm treelist "^ treelist-ref"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (treelist-ref l i)))))

  (bm mut-treelist "^ mut-tree-ref"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (mutable-treelist-ref l i)))))

  (bm hasheq "^ hasheq-ref"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (hash-ref ht i)))))

    (bm hasheq "^ hasheq-ref!"
      (let ([ht (hash-copy (for/hasheq ([i (in-range 0 N)]) (values i i)))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (hash-ref ht i)))))

  (bm gvector "^ gvector-ref"
      (let ([l (for/gvector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (gvector-ref l i)))))

  (bm sek "^ eseq-ref"
      (let ([l (for/eseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (eseq-ref l i)))))

  (bm sek "^ pseq-ref"
      (let ([l (for/pseq ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (pseq-ref l i)))))

  (bm cons "^ dyn in-list"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (car (list (in-list l)))])
            i))))

  (bm treelist "^ dyn in-treelist"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (car (list (in-treelist l)))])
            i))))

  (void))

(module+ main
  (measure 1000000
           10)
  (measure 100000
           100)
  (measure 10000
           1000)
  (measure 1000
           10000)
  (measure 100
           100000)
  (measure 10
           1000000))

(module+ test
  (module config info
    (define timeout 120))
  (measure 100000
           100))
