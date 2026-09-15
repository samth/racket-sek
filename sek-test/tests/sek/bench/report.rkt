#lang racket/base
;; Turns the JSON that `run.sh --json` records into one self-contained page.
;;
;;   ./run.sh --all --quick --json results.json
;;   racket -y nqueens.rkt --json results.json      (appends)
;;   racket -y report.rkt results.json report.html
;;
;; The page is static: the charts are SVG generated here, so it needs no
;; scripts, no network, and no build step to look at.

(require racket/list
         racket/string
         racket/math
         racket/format
         json)

;; ------------------------------------------------------------------- reading

(define (read-tables path)
  (call-with-input-file path
                        (lambda (in)
                          (for/list ([line (in-lines in)]
                                     #:unless (string=? (string-trim line) ""))
                            (string->jsexpr line)))))

(define (num v)
  (and (real? v) v))

;; ---------------------------------------------------------------- provenance

;; What each scenario measures, and where the shape came from.  The scenarios
;; in the first group follow the OCaml library's own benchmark suite and the
;; paper's Figures 17 and 18; the second group is transcribed from the suites
;; that ship with the other chunked-sequence libraries.
(define notes
  (hash
   'stack
   '("the paper, Figure 17"
     "Repeat \"n pushes then n pops\" at the back, so the cost is charged against the length of the stack.")
   'front-stack
   '("the paper, Figure 18"
     "The same, at the front. A structure with a chunk at each end should not care which end it is.")
   'queue
   '("sek's own suite"
     "Push at the back and pop at the front, the pattern neither end-chunk alone can serve.")
   'traversal
   '("sek's own suite"
     "Walk the whole sequence in order. What a segment buys is that the inner loop touches a raw vector.")
   'random-access
   '("sek's own suite"
     "Read at uniformly random indices: the operation a chunked tree is worst at and a vector is best at.")
   'hops
   '("sek's own suite"
     "Read at indices a fixed distance apart, which is where an iterator's cached segment starts to pay.")
   'update
   '("sek's own suite" "Write at uniformly random indices.")
   'construction
   '("sek's own suite" "Build a sequence of n elements from nothing.")
   'concat
   '("sek's own suite" "Append two sequences of n/2 elements.")
   'split
   '("sek's own suite" "Cut a sequence in half.")
   'snapshot
   '("sek's own suite" "Freeze an ephemeral sequence into a persistent one, and thaw it again.")
   'transient
   '("the paper, section 2"
     "The round trip the whole design exists for: edit a persistent sequence, change m elements in place, snapshot.")
   'filter
   '("the paper's motivating example" "Keep one element in three.")
   'fill
   '("sek's own suite" "Overwrite every element in place.")
   'capacities
   '("sek's own suite"
     "The same operations at several chunk capacities, which is the one tuning knob.")
   'burst-check
   '("methodology"
     "A structure whose push walks the sequence cannot be grown to n one push at a time -- that loop is quadratic -- so those rows are measured as a bounded burst against a sequence already at length n. This runs both paths against everything that can afford either, so the size of that substitution is on the page rather than asserted.")
   'sync-cost
   '("methodology"
     "What concurrency safety costs a growable array. The memory-safety invariant is what lets an unsafe read stay in bounds while another thread grows the array; a lock is what it takes for two threads pushing at once to both be recorded. The invariant is free. The lock is not.")
   'apply-sequential
   '("Scala, vApplySequential"
     "Index at ascending positions. A structure that remembers the chunk it last touched answers all but one lookup in K from cache. sek does not cache on ref -- that is what its iterators are for -- so the last row shows the same walk through one.")
   'update-sequential
   '("Scala, vUpdateSequential"
     "A persistent set at ascending indices, threading the result. Scala measures this apart from the random case because a sequential walk is what a tree can exploit.")
   'apprepend
   '("Scala, vApprepend"
     "Alternate append and prepend. An RRB tree pays for a prepend what it pays for an append.")
   'ends
   '("Scala, vHead / vLast / vTail"
     "Peek at both ends, then walk the sequence off the front one persistent pop at a time.")
   'slice
   '("Scala, vSlice" "Every sub-range on a 10% grid, so a fixed number of slices whatever the size.")
   'bulk-append
   '("Scala, vBulkAppend*"
     "Append a sequence of two elements, a tenth of the receiver, all of it, and the receiver itself.")
   'map
   '("Scala, vMapNew" "Build a new sequence by applying a function to every element.")
   'filter-ratio
   '("Scala, vFilter100p / 50p / 0p"
     "Filter keeping all, half, and none. A structure that shares unchanged runs wins at 100%; one that allocates per survivor wins at 0%.")
   'take-drop
   '("immer, take_lin / drop_lin"
     "Shorten the sequence by a tenth, then shorten that, and so on -- so the structure is asked to re-split its own output. immer's _mut variant is the transient row.")
   'push-move
   '("immer, push vs push_move"
     "Building by repeated persistent push against building through a transient and freezing at the end. Clojure spells the second (persistent! (reduce conj! (transient []) xs)).")
   'split-parts
   '("bifurcan, ICollection.split"
     "Cut into eight pieces and traverse every one, which is how bifurcan sets up a parallel fold.")
   'nqueens
   '("the Scheme benchmark suite"
     "Eight queens, counted 10000 times, from racket-benchmarks/tests/racket/benchmarks/common/nqueens.sch.")))

(define main-order
  '(stack front-stack
          queue
          traversal
          random-access
          hops
          update
          construction
          concat
          split
          snapshot
          transient
          filter
          fill
          capacities
          burst-check
          sync-cost))
(define external-order
  '(apply-sequential update-sequential
                     apprepend
                     ends
                     slice
                     bulk-append
                     map
                     filter-ratio
                     take-drop
                     push-move
                     split-parts))

;; -------------------------------------------------------------------- color

;; One hue per family, two values within it, so that a chart reads as
;; "the subject against its rivals" before it reads as seven separate lines.
(define (series-color name)
  (define n (string-downcase name))
  (define (has? s)
    (regexp-match? (regexp (regexp-quote s)) n))
  (cond
    [(has? "box of list") "grey2"]
    [(has? "mut-treelist") "clay2"]
    [(has? "mutable-treelist") "clay2"]
    [(has? "treelist") "clay"]
    [(has? "gvector") "green"]
    [(has? "eseq") "indigo"]
    [(has? "pseq") "indigo2"]
    [(has? "sek") "indigo"]
    [(has? "array") "steel"]
    [(has? "vector") "slate"]
    [(has? "list") "grey"]
    [else "slate"]))

;; ------------------------------------------------------------------- numbers

(define (fmt x)
  (cond
    [(not x) "-"]
    [(>= x 1000) (~r x #:precision 0)]
    [(>= x 100) (~r x #:precision 1)]
    [else (~r x #:precision 2)]))

(define (tick-label v)
  (cond
    [(>= v 1e6) (format "~aM" (~r (/ v 1e6) #:precision 0))]
    [(>= v 1e3) (format "~ak" (~r (/ v 1e3) #:precision 0))]
    [(>= v 1) (~r v #:precision 0)]
    [else (~r v #:precision 2)]))

(define (esc s)
  (regexp-replaces (format "~a" s)
                   '((#rx"&" "\\&amp;") (#rx"<" "\\&lt;") (#rx">" "\\&gt;") (#rx"\"" "\\&quot;"))))

;; --------------------------------------------------------------------- chart

;; A log axis, because a single table can span a nanosecond and a millisecond.
(define (decades lo hi)
  (for/list ([e (in-range (inexact->exact (floor (log10 lo)))
                          (add1 (inexact->exact (ceiling (log10 hi)))))])
    (expt 10.0 e)))

(define (log10 x)
  (/ (log x) (log 10)))

(define (bounds vals)
  (define lo (apply min vals))
  (define hi (apply max vals))
  (values (expt 10.0 (floor (log10 (max lo 1e-3))))
          (expt 10.0 (max (add1 (floor (log10 (max lo 1e-3)))) (ceiling (log10 hi))))))

;; Several series across several sizes: vertical bars, grouped by size.
(define (drawable rows)
  (filter (lambda (r) (ormap num (hash-ref r 'values))) rows))

(define (grouped-chart t)
  (define sizes (hash-ref t 'sizes))
  (define rows (drawable (hash-ref t 'rows)))
  (define vals
    (filter values
            (append* (for/list ([r rows])
                       (map num (hash-ref r 'values))))))
  (cond
    [(null? vals) ""]
    [else
     (define-values (lo hi) (bounds vals))
     (define W 1000.0)
     (define H 300.0)
     (define ml 56.0)
     (define mr 12.0)
     (define mt 14.0)
     (define mb 44.0)
     (define pw (- W ml mr))
     (define ph (- H mt mb))
     (define (y v)
       (+ mt (* ph (- 1.0 (/ (- (log10 v) (log10 lo)) (- (log10 hi) (log10 lo)))))))
     (define ng (length sizes))
     (define gw (/ pw ng))
     (define inner (* gw 0.80))
     (define bw (/ inner (length rows)))
     (define out (open-output-string))
     (define (say . xs)
       (for ([x xs])
         (display x out)))
     (say "<svg class=\"chart\" viewBox=\"0 0 " W " " H "\" role=\"img\">")
     ;; decade grid
     (for ([d (in-list (decades lo hi))]
           #:when (<= lo d hi))
       (say "<line class=\"grid\" x1=\""
            ml
            "\" x2=\""
            (- W mr)
            "\" y1=\""
            (y d)
            "\" y2=\""
            (y d)
            "\"/>"
            "<text class=\"tick\" x=\""
            (- ml 8)
            "\" y=\""
            (+ (y d) 3.5)
            "\" text-anchor=\"end\">"
            (tick-label d)
            "</text>"))
     (say "<line class=\"axis\" x1=\""
          ml
          "\" x2=\""
          (- W mr)
          "\" y1=\""
          (+ mt ph)
          "\" y2=\""
          (+ mt ph)
          "\"/>")
     ;; bars
     (for ([sz (in-list sizes)]
           [g (in-naturals)])
       (define gx (+ ml (* g gw) (/ (- gw inner) 2.0)))
       (for ([r (in-list rows)]
             [k (in-naturals)])
         (define v (num (list-ref (hash-ref r 'values) g)))
         (when v
           (define x (+ gx (* k bw)))
           (define top (y v))
           (say "<rect class=\"bar "
                (series-color (hash-ref r 'name))
                "\" x=\""
                (+ x 0.8)
                "\" y=\""
                top
                "\" width=\""
                (max 1.0 (- bw 1.6))
                "\" height=\""
                (max 1.0 (- (+ mt ph) top))
                "\">"
                "<title>"
                (esc (hash-ref r 'name))
                " at "
                (esc sz)
                ": "
                (fmt v)
                "</title>"
                "</rect>")
           (when (>= bw 15.0)
             (say "<text class=\"val\" x=\""
                  (+ x (/ bw 2.0))
                  "\" y=\""
                  (- top 4)
                  "\" text-anchor=\"middle\">"
                  (fmt v)
                  "</text>"))))
       (say "<text class=\"xlab\" x=\""
            (+ ml (* g gw) (/ gw 2.0))
            "\" y=\""
            (+ mt ph 20)
            "\" text-anchor=\"middle\">"
            (esc sz)
            "</text>"))
     (say "</svg>")
     (get-output-string out)]))

;; One column: horizontal bars, which give the row names room to be read.
(define (single-chart t)
  (define rows (drawable (hash-ref t 'rows)))
  (define vals
    (filter values
            (for/list ([r rows])
              (num (car (hash-ref r 'values))))))
  (cond
    [(null? vals) ""]
    [else
     (define-values (lo hi) (bounds vals))
     (define W 1000.0)
     (define rh 26.0)
     (define ml 170.0)
     (define mr 14.0)
     (define mt 10.0)
     (define mb 30.0)
     (define H (+ mt mb (* rh (length rows))))
     (define pw (- W ml mr))
     (define (x v)
       (+ ml (* pw (/ (- (log10 v) (log10 lo)) (- (log10 hi) (log10 lo))))))
     (define out (open-output-string))
     (define (say . xs)
       (for ([q xs])
         (display q out)))
     (say "<svg class=\"chart\" viewBox=\"0 0 " W " " H "\" role=\"img\">")
     (for ([d (in-list (decades lo hi))]
           #:when (<= lo d hi))
       (say "<line class=\"grid\" y1=\""
            mt
            "\" y2=\""
            (+ mt (* rh (length rows)))
            "\" x1=\""
            (x d)
            "\" x2=\""
            (x d)
            "\"/>"
            "<text class=\"tick\" x=\""
            (x d)
            "\" y=\""
            (+ mt (* rh (length rows)) 18)
            "\" text-anchor=\"middle\">"
            (tick-label d)
            "</text>"))
     (for ([r (in-list rows)]
           [k (in-naturals)])
       (define v (num (car (hash-ref r 'values))))
       (define top (+ mt (* k rh) 4))
       (say "<text class=\"rowlab\" x=\""
            (- ml 10)
            "\" y=\""
            (+ top 13)
            "\" text-anchor=\"end\">"
            (esc (hash-ref r 'name))
            "</text>")
       (when v
         (say "<rect class=\"bar "
              (series-color (hash-ref r 'name))
              "\" x=\""
              ml
              "\" y=\""
              top
              "\" width=\""
              (max 1.0 (- (x v) ml))
              "\" height=\""
              (- rh 8)
              "\"/>"
              "<text class=\"val\" x=\""
              (+ (x v) 6)
              "\" y=\""
              (+ top 13)
              "\">"
              (fmt v)
              "</text>")))
     (say "</svg>")
     (get-output-string out)]))

(define (chart t)
  (if (= 1 (length (hash-ref t 'sizes)))
      (single-chart t)
      (grouped-chart t)))

;; --------------------------------------------------------------------- table

(define (data-table t)
  (define sizes (hash-ref t 'sizes))
  (define rows (hash-ref t 'rows))
  ;; the best number in each column, so the winner is visible without reading
  (define bests
    (for/list ([g (in-range (length sizes))])
      (define vs
        (filter values
                (for/list ([r rows])
                  (num (list-ref (hash-ref r 'values) g)))))
      (and (pair? vs) (apply min vs))))
  (define out (open-output-string))
  (define (say . xs)
    (for ([x xs])
      (display x out)))
  (say "<div class=\"scroll\"><table>")
  (say "<thead><tr><th scope=\"col\"></th>")
  (for ([s sizes])
    (say "<th scope=\"col\">" (esc s) "</th>"))
  (say "</tr></thead><tbody>")
  (for ([r rows])
    (say "<tr><th scope=\"row\"><span class=\"swatch "
         (series-color (hash-ref r 'name))
         "\"></span>"
         (esc (hash-ref r 'name))
         "</th>")
    (for ([v (hash-ref r 'values)]
          [g (in-naturals)])
      (define x (num v))
      (define best? (and x (list-ref bests g) (= x (list-ref bests g))))
      (say "<td" (if best? " class=\"best\"" "") ">" (fmt x) "</td>"))
    (say "</tr>"))
  (say "</tbody></table></div>")
  (get-output-string out))

;; ---------------------------------------------------------------------- page

(define (section id title blurb tables)
  (define out (open-output-string))
  (define (say . xs)
    (for ([x xs])
      (display x out)))
  (say "<section id=\"" id "\">")
  (say "<h2>" (esc title) "</h2>")
  (when blurb
    (say "<p class=\"blurb\">" blurb "</p>"))
  (for ([t tables])
    (say (card t)))
  (say "</section>")
  (get-output-string out))

(define (card t)
  (define scen (string->symbol (hash-ref t 'scenario)))
  (define note (hash-ref notes scen '("" "")))
  (string-append "<article class=\"card\">"
                 "<div class=\"eyebrow\">"
                 (esc (hash-ref t 'scenario))
                 (if (string=? (car note) "")
                     ""
                     (string-append " &middot; after " (esc (car note))))
                 "</div>"
                 "<h3>"
                 (esc (hash-ref t 'title))
                 "</h3>"
                 "<p class=\"units\">"
                 (esc (hash-ref t 'units))
                 " &mdash; lower is better</p>"
                 (chart t)
                 (data-table t)
                 "</article>"))

;; A scenario's cards appear once, under the first heading that claims it, and
;; every table of that scenario stays with it.
(define (tables-for tables scen)
  (filter (lambda (t) (equal? (hash-ref t 'scenario) (symbol->string scen))) tables))

(define (render tables racket-version)
  (define (group order)
    (append* (for/list ([s (in-list order)])
               (let ([ts (tables-for tables s)])
                 (if (null? ts)
                     '()
                     (list (cons s ts)))))))
  (define main-groups (group main-order))
  (define ext-groups (group external-order))
  (define nq (tables-for tables 'nqueens))
  (define (toc groups)
    (string-append "<ul class=\"toc\">"
                   (apply string-append
                          (for/list ([g groups])
                            (format "<li><a href=\"#~a\">~a</a><span>~a</span></li>"
                                    (car g)
                                    (esc (car g))
                                    (esc (car (hash-ref notes (car g) '("" "")))))))
                   "</ul>"))
  (string-append
   HEAD
   "<main>"
   "<header class=\"masthead\">"
   "<p class=\"kicker\">A catenable, splittable, transient sequence &mdash; in Racket</p>"
   "<h1>sek, measured</h1>"
   "<p class=\"lede\">Chargu&eacute;raud and Pottier's chunked sequence, ported to Racket, "
   "against Racket's own sequence types. The first group of measurements follows the "
   "benchmark suite that ships with the authors' OCaml library and the two figures in "
   "the paper. The second is transcribed from the suites that ship with the other "
   "chunked-sequence libraries &mdash; Scala's <code>Vector</code>, immer, and bifurcan "
   "&mdash; so that those comparisons are ones their authors chose.</p>"
   "<dl class=\"meta\">"
   "<div><dt>Runtime</dt><dd>Racket "
   (esc racket-version)
   " CS</dd></div>"
   "<div><dt>Reported</dt><dd>best of three trials, after a warm-up to a 150&nbsp;ms floor</dd></div>"
   "<div><dt>Sizes</dt><dd>10<sup>2</sup> to 10<sup>5</sup> elements</dd></div>"
   "<div><dt>Scale</dt><dd>every chart is logarithmic</dd></div>"
   "</dl>"
   "</header>"
   "<nav class=\"nav\">"
   "<h2>The paper and the OCaml library</h2>"
   (toc main-groups)
   "<h2>Borrowed from other libraries</h2>"
   (toc ext-groups)
   (if (null? nq)
       ""
       "<h2>A whole program</h2><ul class=\"toc\"><li><a href=\"#nqueens\">nqueens</a><span>the Scheme benchmark</span></li></ul>")
   "</nav>"
   (apply string-append
          (for/list ([g main-groups])
            (section (format "~a" (car g))
                     (format "~a" (car g))
                     (cadr (hash-ref notes (car g) '("" "")))
                     (cdr g))))
   (apply string-append
          (for/list ([g ext-groups])
            (section (format "~a" (car g))
                     (format "~a" (car g))
                     (cadr (hash-ref notes (car g) '("" "")))
                     (cdr g))))
   (if (null? nq)
       ""
       (section "nqueens" "nqueens" (cadr (hash-ref notes 'nqueens)) nq))
   "<footer><p>Generated by <code>bench/report.rkt</code> from the JSON that "
   "<code>bench/run.sh --json</code> records. Every number on this page came from "
   "one run on one machine; treat the ratios as the result and the absolute "
   "figures as incidental.</p></footer>"
   "</main>"))

;; ----------------------------------------------------------------- the shell

(define HEAD
  #<<HTML
<title>sek, measured</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap">
<style>
:root {
  --ground: #f4f6f8;
  --surface: #ffffff;
  --ink: #161b22;
  --muted: #67707d;
  --hair: #dce1e7;
  --hair-soft: #eaeef2;
  --accent: #363c9c;
  --indigo: #3f46b0;
  --indigo2: #8288dd;
  --clay: #a8562a;
  --clay2: #d9974f;
  --green: #2c7a5c;
  --slate: #414a58;
  --steel: #4b7f93;
  --grey: #8d95a1;
  --grey2: #bcc3cc;
  --serif: "Source Serif 4", Georgia, "Times New Roman", serif;
  --mono: "IBM Plex Mono", ui-monospace, "SF Mono", Menlo, monospace;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --ground: #0f1319;
    --surface: #171d26;
    --ink: #e3e8ef;
    --muted: #8e98a6;
    --hair: #263041;
    --hair-soft: #1d2531;
    --accent: #9ba1f5;
    --indigo: #7d84e8;
    --indigo2: #4a51ad;
    --clay: #d98d55;
    --clay2: #9c6134;
    --green: #4fae87;
    --slate: #9aa5b4;
    --steel: #6fb0c6;
    --grey: #6d7686;
    --grey2: #454e5d;
  }
}
:root[data-theme="dark"] {
  --ground: #0f1319;
  --surface: #171d26;
  --ink: #e3e8ef;
  --muted: #8e98a6;
  --hair: #263041;
  --hair-soft: #1d2531;
  --accent: #9ba1f5;
  --indigo: #7d84e8;
  --indigo2: #4a51ad;
  --clay: #d98d55;
  --clay2: #9c6134;
  --green: #4fae87;
  --slate: #9aa5b4;
  --steel: #6fb0c6;
  --grey: #6d7686;
  --grey2: #454e5d;
}

* { box-sizing: border-box; }
body {
  margin: 0;
  background: var(--ground);
  color: var(--ink);
  font-family: var(--serif);
  font-size: 17px;
  line-height: 1.55;
  -webkit-font-smoothing: antialiased;
}
main {
  max-width: 1120px;
  margin: 0 auto;
  padding: 4rem 1.5rem 6rem;
  display: flex;
  flex-direction: column;
  gap: 3.5rem;
}

.masthead { display: flex; flex-direction: column; gap: 1rem; }
.kicker {
  margin: 0;
  font-family: var(--mono);
  font-size: 0.72rem;
  font-weight: 500;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--accent);
}
h1 {
  margin: 0;
  font-size: clamp(2.4rem, 6vw, 3.6rem);
  font-weight: 600;
  letter-spacing: -0.02em;
  line-height: 1.05;
  text-wrap: balance;
}
.lede { margin: 0; max-width: 68ch; color: var(--muted); font-size: 1.05rem; }
.lede code, footer code { font-family: var(--mono); font-size: 0.86em; color: var(--ink); }

.meta {
  margin: 0.5rem 0 0;
  display: flex;
  flex-wrap: wrap;
  gap: 0 2.5rem;
  border-top: 1px solid var(--hair);
  padding-top: 1rem;
}
.meta div { display: flex; flex-direction: column; gap: 0.15rem; padding: 0.4rem 0; }
.meta dt {
  font-family: var(--mono);
  font-size: 0.66rem;
  font-weight: 500;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--muted);
}
.meta dd { margin: 0; font-size: 0.92rem; }

.nav { border-top: 1px solid var(--hair); padding-top: 1.5rem; }
.nav h2 {
  margin: 1.5rem 0 0.75rem;
  font-family: var(--mono);
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--muted);
}
.nav h2:first-child { margin-top: 0; }
.toc {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  gap: 0.15rem 1.5rem;
}
.toc li { display: flex; flex-direction: column; padding: 0.3rem 0; }
.toc a {
  font-family: var(--mono);
  font-size: 0.85rem;
  color: var(--accent);
  text-decoration: none;
  border-bottom: 1px solid transparent;
  align-self: flex-start;
}
.toc a:hover, .toc a:focus-visible { border-bottom-color: currentColor; }
.toc span { font-size: 0.8rem; color: var(--muted); }

section { display: flex; flex-direction: column; gap: 1rem; scroll-margin-top: 1.5rem; }
section h2 {
  margin: 0;
  font-family: var(--mono);
  font-size: 1.1rem;
  font-weight: 600;
  letter-spacing: -0.01em;
}
section h2::before { content: "\00a7\00a0"; color: var(--muted); font-weight: 400; }
.blurb { margin: -0.4rem 0 0; max-width: 72ch; color: var(--muted); font-size: 0.95rem; }

.card {
  background: var(--surface);
  border: 1px solid var(--hair);
  border-radius: 3px;
  padding: 1.4rem 1.5rem 1.2rem;
  display: flex;
  flex-direction: column;
  gap: 0.7rem;
}
.eyebrow {
  font-family: var(--mono);
  font-size: 0.66rem;
  font-weight: 500;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--muted);
}
.card h3 { margin: 0; font-size: 1.15rem; font-weight: 600; text-wrap: balance; }
.units {
  margin: -0.5rem 0 0;
  font-family: var(--mono);
  font-size: 0.75rem;
  color: var(--muted);
}

.chart { width: 100%; height: auto; display: block; margin-top: 0.3rem; }
.chart .grid { stroke: var(--hair-soft); stroke-width: 1; }
.chart .axis { stroke: var(--hair); stroke-width: 1; }
.chart .tick, .chart .xlab, .chart .val, .chart .rowlab {
  font-family: var(--mono);
  fill: var(--muted);
}
.chart .tick { font-size: 10px; }
.chart .xlab { font-size: 11px; fill: var(--ink); }
.chart .val { font-size: 9px; }
.chart .rowlab { font-size: 11px; fill: var(--ink); }
.bar.indigo { fill: var(--indigo); }
.bar.indigo2 { fill: var(--indigo2); }
.bar.clay { fill: var(--clay); }
.bar.clay2 { fill: var(--clay2); }
.bar.green { fill: var(--green); }
.bar.slate { fill: var(--slate); }
.bar.steel { fill: var(--steel); }
.bar.grey { fill: var(--grey); }
.bar.grey2 { fill: var(--grey2); }

.scroll { overflow-x: auto; }
table {
  border-collapse: collapse;
  font-family: var(--mono);
  font-size: 0.8rem;
  font-variant-numeric: tabular-nums;
  width: auto;
}
thead th {
  text-align: right;
  font-weight: 500;
  color: var(--muted);
  padding: 0.3rem 1.2rem;
  border-bottom: 1px solid var(--hair);
  white-space: nowrap;
}
tbody th {
  text-align: left;
  font-weight: 400;
  padding: 0.28rem 0.55rem 0.28rem 0;
  white-space: nowrap;
}
tbody td {
  text-align: right;
  padding: 0.28rem 1.2rem;
  color: var(--muted);
  white-space: nowrap;
}
tbody td.best { color: var(--ink); font-weight: 600; }
tbody tr + tr th, tbody tr + tr td { border-top: 1px solid var(--hair-soft); }
.swatch {
  display: inline-block;
  width: 0.55rem;
  height: 0.55rem;
  border-radius: 1px;
  margin-right: 0.5rem;
  vertical-align: baseline;
}
.swatch.indigo { background: var(--indigo); }
.swatch.indigo2 { background: var(--indigo2); }
.swatch.clay { background: var(--clay); }
.swatch.clay2 { background: var(--clay2); }
.swatch.green { background: var(--green); }
.swatch.slate { background: var(--slate); }
.swatch.steel { background: var(--steel); }
.swatch.grey { background: var(--grey); }
.swatch.grey2 { background: var(--grey2); }

footer {
  border-top: 1px solid var(--hair);
  padding-top: 1.25rem;
  color: var(--muted);
  font-size: 0.9rem;
}
footer p { margin: 0; max-width: 68ch; }

a:focus-visible, [tabindex]:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
@media (max-width: 640px) {
  main { padding: 2.5rem 1rem 4rem; gap: 2.5rem; }
  .card { padding: 1.1rem 1rem 1rem; }
}
</style>
HTML
  )

;; --------------------------------------------------------------------- main

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define in
    (if (pair? args)
        (car args)
        "results.json"))
  (define out
    (if (and (pair? args) (pair? (cdr args)))
        (cadr args)
        "report.html"))
  (define tables (read-tables in))
  (call-with-output-file out #:exists 'truncate (lambda (o) (display (render tables (version)) o)))
  (printf "wrote ~a from ~a tables in ~a\n" out (length tables) in))
