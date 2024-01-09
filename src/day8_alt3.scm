(import
  (rnrs io ports)
  ; (rnrs hashtables) ; r6rs hash tables
  (srfi srfi-1) ; list library
  (srfi srfi-8) ; receive
  (srfi srfi-43) ; vector library
  (srfi srfi-69) ; hash map wrapper
  (srfi srfi-171) ; transducers
  (ice-9 peg)
  (ice-9 pretty-print)
  (ice-9 match))

(define parse-input
  (let ()
    (define-peg-string-patterns
     "root <-- dirs NL* entries !.
      dirs <-- dir*
      dir <-- [LR]
      entries <-- (entry NL*)*
      entry <-- node HS EQ HS OP HS node HS CM HS node HS CP
      node <-- [A-Z0-9]+
      EQ < '='
      OP < '('
      CP < ')'
      CM < ','
      HS < [ \t]*
      NL < '\n'")
    (lambda (text) (peg:tree (match-pattern root text)))))

(define cst->ast
  (let*((dir (match-lambda (('dir d) (string-ref d 0))))
        (node (match-lambda (('node n) n)))
        (entry (match-lambda (('entry . ns) (map node ns)))))
    (match-lambda (('root ('dirs . dirs) ('entries . entries))
      (list (map dir dirs) (map entry entries))))))

(define (extended-euclidean a b) ; all data from the extended euclidean algorithm
  (let loop ((r0 a) (r1 b) (s0 1) (s1 0) (t0 0) (t1 1))
    (if (= r1 0)
      (list r0 s0 t0 t1 s1) ; r0 = gcd a b, s0 a + t0 b = r0, (t1,s1) = +-(a,b)/r0
      (receive (q1 r2) (euclidean/ r0 r1)
        (let ((s2 (- s0 (* q1 s1)))
              (t2 (- t0 (* q1 t1))))
          (loop r1 r2 s1 s2 t1 t2))))))

(define (make-adjlists entries) ; map from name to (name . (left-node . right-node))
  (let ((adjlists (alist->hash-table entries)))
    (hash-table-walk
      adjlists
      (lambda (n l) 
        (set-cdr! l
          (cons (hash-table-ref adjlists (car l))
                (hash-table-ref adjlists (cadr l))))
        (set-car! l n)))        
    adjlists))

(define (walk dirs adjlists init-name terminal-name)
  (let ((terminal-node (hash-table-ref adjlists terminal-name))
        (dirfuns (vector-map (lambda (_ d) (case d ((#\L) cadr) ((#\R) cddr))) dirs)))
    (let loop ((node (hash-table-ref adjlists init-name))
               (i 0))
      (if (eq? node terminal-node)
        i
        (loop ((vector-ref dirfuns (modulo i (vector-length dirs))) node) (1+ i))))))

; (define (walk-map dirs adjlists init-names term?)
;   (let ((dirfuns (vector-map (lambda (_ d) (case d ((#\L) cadr) ((#\R) cddr))) dirs)))
;     (let loop ((nodes (map (lambda (name) (hash-table-ref adjlists name)) init-names))
;                (i 0))
;       (if (= (modulo i (* 271 10000)) 0) (begin (map display (list i ": " (map car nodes))) (newline)))
;       (if (term? nodes)
;         i
;         (loop (map! (vector-ref dirfuns (modulo i (vector-length dirs))) nodes) (1+ i))))))

(define (list->hash-set l) (alist->hash-table (map list l)))

(define vector->hash-set (compose list->hash-set vector->list))

(define (hash-set-union! set0 set1) ; modifies set1
  (for-each
   (lambda (x) (hash-table-set! set1 x '()))
   (hash-table-keys set0))
  set1)

(define (hash-set-intersect! set0 set1) ; modifies set1
  (for-each
   (lambda (x) (if (not (hash-table-exists? set0 x)) (hash-table-delete! set1 x)))
   (hash-table-keys set1))
  set1)

(define unify
  (define (mod-kons n)
    (lambda (a d)
      (hash-table-set! d (modulo a n1) '())
      d))
  (match-lambda (((ppdv0 pdv0 . n0) (ppdl1 pdl1 . n1))
    ; 1. test ppdl1 points 
    ;   1. if point is in ppdv0, keep
    ;   2. if point < car pdv0, drop
    ;   3. else, ((point - car pdv0) % n0 + car pdv0) is in pdv0
    ; 1. test pdl1 points
    ;   1. bucket pdv0 and pdl1 points mod gcd(n0, n1)
    ;   2. make new solutions between buckets and ppdv0???? (TOO MANY)
    (let*((pipt0 (vector-ref pdv0 0)) ; periodic init point
          (ppds0 (vector->hash-set ppdv0))
          (pds0 (vector->hash-set pdv0))
          (pds0modn1
           (vector-fold (mod-kons n1) (make-hash-table) pdv0))
          (ppdl2
           (filter
            (lambda (x)
              (or (hash-table-exists? ppds0 x)
                  (and (>= x pipt0)
                       (hash-table-exists?
                        pds0
                        (+ (modulo (- x pipt0) n0) pipt0)))))
            ppdl1))
          (gcd01 (gcd n0 n1))
          (mod-gcd01-kons (mod-kons gcd01))
          (pds0modgcd01
           (vector-fold (mod-gcd01-kons) (make-hash-table) pdv0))
          (pds1modgcd01
           (vector-fold (mod-gcd01-kons) (make-hash-table) pdl1))
          (pdl2 ; TODO
           ))
      '()))))

(define (walk-table-all dirs-list init-nodes term-nodes)
  (let*((dirs (vector-map (lambda (_ d) (case d ((#\L) cadr) ((#\R) cddr))) dirs-list))
        (termset (list->hash-set term-nodes)) ; set of nodes, abusing a hash table
        (skip-vecs (map (walk-table dirs termset) init-nodes))
        (skip-sets (map (match-lambda ((ppdv pdv . n)
                          (cons* (vector->hash-set ppdv)
                                 (vector->hash-set pdv)
                                 n)))
                        skip-vecs))
        (preperiodic-vecs (map car skip-vecs))
        (preperiodic-sets (map car skip-sets))
        (periodic-vecs (map cdr skip-vecs))
        (periodic-sets (map cdr skip-sets))
        (preperiodic-max ; inclusive cutoff to check
         (fold (lambda (vec n) (max (vector-last vec) n))
               -1
               (filter (compose not vector-empty?) preperiodic-vecs)))
        ; (preperiodic-solutions ; involving at least one preperiodic point
        ;  (if (null? preperiodic-vecs)
        ;    '()
        ;    (hash-table-keys
        ;     (fold (lambda (snew snil))
        ;           (hash-table-copy (car preperiodic-solutions)))))
        ;  (if (null? preperiodic-vecs)
        ;    '()
        ;    (let ((preperiodic-sets
        ;           (map (match-lambda ((ppdv pdv . _)
        ;                  (hash-set-union!
        ;                   (list->hash-set (vector->list ppdv))
        ;                   (list->hash-set
        ;                     (vector->list
        ;                       pdv
        ;                       0
        ;                       (or
        ;                         (vector-skip (lambda (n) (<= n preperiodic-max)) pdv)
        ;                         (vector-length pdv)))))))
        ;                skip-vecs))) ; TODO periodic behavior with modular arithmetic
        ;      (display "preperiodic-max = ") 
        ;      (pretty-print preperiodic-max)
        ;      (pretty-print (map hash-table-keys preperiodic-sets))
        ;      (hash-table-keys (fold hash-set-intersect!
        ;                             (car preperiodic-sets)
        ;                             (cdr preperiodic-sets)))))
        ;  )
        )
    (display "preperiodic-data = ")
    (pretty-print preperiodic-vecs)
    (display "periodic-data = ")
    (pretty-print periodic-vecs)
    ; (if (null? preperiodic-solutions)
    ;   '() ; TODO
    ;   (apply min preperiodic-solutions))
      )) ; TODO incomplete

(define (vector-last vec)
  (vector-ref vec (1- (vector-length vec))))

(define (vector-shift-right! vec dist)
  (vector-move-right! vec 0 (- (vector-length vec) dist) vec dist)
  vec)

(define extract-skip-data ; -> (preperiodic positions . (period . periodic positions))
  (lambda (skiptable init-node term-node-imodn termset)
    (let*((path-from
           (lambda (from-node-imodn)
              (unfold (lambda (v) (equal? (cdr v) term-node-imodn))
                      car
                      (lambda (v) (hash-table-ref skiptable (cdr v)))
                      (hash-table-ref skiptable from-node-imodn)
                      (compose list car))))
          (preperiodic-path (list->vector (path-from (cons init-node 0))))
          (periodic-path (list->vector (path-from term-node-imodn)))
          (cycle-start (vector-last preperiodic-path))
          (cycle-end (vector-last periodic-path))
          (period (- cycle-end cycle-start))
          (preperiodic
           (if (hash-table-exists? termset init-node) ; remove last, not preperiodic
             (begin ; add init-node if needed 
              (vector-shift-right! preperiodic-path 1)
              (vector-set! preperiodic-path 0 0)
              preperiodic-path)
             (vector-copy preperiodic-path 0 (1- (vector-length preperiodic-path)))))
          (periodic ; remove last, not periodic
           (begin
            (vector-shift-right! periodic-path 1)
            (vector-set! periodic-path 0 cycle-start)
            periodic-path)))
      (cons* preperiodic periodic period))))

(define (walk-table dirs termset) ; find distances between nodes
  (let ((skiptable (make-hash-table))
        (n (vector-length dirs))) ; n = length of instruction list
    (lambda (init-node) ; paths are at least 1 long
      (let loop ((node ((vector-ref dirs (modulo 1 n)) init-node)) ; current node
                 (i 1) ; current index in direction instructions
                 (from-node init-node) ; node traveling from
                 (from-imodn 0)) ; direction index of node traveling from
        (let ((imodn (modulo i n)))
          (if (hash-table-exists? termset node) ; termset assumed sparse
            (let ((node-imodn (cons node imodn)))
              ; point from-node to current (index . node)
              (hash-table-set! skiptable (cons from-node from-imodn) (cons i node-imodn))
              (if (hash-table-exists? skiptable node-imodn)
               ; found a loop, we're done. return the end point in the loop
               (extract-skip-data skiptable init-node node-imodn termset)
               (loop ((vector-ref dirs imodn) node) (1+ i) node imodn))) ; new from-node
            ; continue if not in termset
            (loop ((vector-ref dirs imodn) node) (1+ i) from-node from-imodn)))))))

(define (part-1 dirs adjlists) (walk dirs adjlists "AAA" "ZZZ"))

(define (part-2 dirs adjlists)
  (let*((nodes-with-suffix
         (lambda (suf) (filter (lambda (n) (string-suffix? suf (car n)))
                               (hash-table-values adjlists))))
        (res
         (walk-table-all dirs (nodes-with-suffix "A") (nodes-with-suffix "Z"))))
    res)) ; TODO: modular arithmetic

; (define (part-2 dirs adjlists)
;   (walk-map
;     dirs
;     adjlists
;     (filter (lambda (n) (string-suffix? "A" n)) (hash-table-keys adjlists))
;     (let*((nzs (filter (lambda (n) (string-suffix? "Z" n)) (hash-table-keys adjlists)))
;           (zt (alist->hash-table (map list nzs))))
;       (lambda (ns) (every (lambda (n) (hash-table-exists? zt (car n))) ns)))))

(define (file->string path) (call-with-input-file path get-string-all))

(match-let*((cst (parse-input (file->string (cadr (command-line)))))
            ((dirs-list entries) (cst->ast cst))
            (dirs (list->vector dirs-list))
            (adjlists (make-adjlists entries)))
  (begin
    (pretty-print (part-1 dirs adjlists))
    (pretty-print (part-2 dirs adjlists))
    ))
