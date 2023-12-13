(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (srfi srfi-43) ; vectors
  (srfi srfi-69) ; hash tables
  (ice-9 regex)
  (ice-9 match)
  (ice-9 pretty-print))

; ordered map, small generalization allows any type with key-proj to get key
(define* (make-olist #:key (capacity 4) (cmp <=) (key-proj car))
  (list (make-vector capacity) 0 cmp key-proj)) ; vec size cmp key-proj

(define* (alist->olist l #:rest r)
  (fold
    (lambda (e ol) (olist-add! ol e))
    (apply make-olist r)
    l))

; some map operations i want to use
(define olist-size cadr)
(define (olist-ind-ref ol i) (vector-ref (car ol) i)) ; underlying vector access
(define (olist-ref-lower ol t) ; lowermost binary search result
  (olist-ind-ref ol (olist-ind-lower ol t)))
(define (olist-ref-upper ol t) ; uppermost binary search result
  (olist-ind-ref ol (olist-ind-upper ol t)))
(define (olist-ind-lower ol t) ; lowermost binary search result index
  (match-let (((vec size cmp key-proj) ol))
    (let loop ((l 0)
               (r size))
      (cond ((< l r) 
             (let ((m (euclidean-quotient (+ l r) 2)))
               (if (cmp t (key-proj (vector-ref vec m)))
                   (loop l m)
                   (loop (1+ m) r))))
            (else l)))))
(define (olist-ind-upper ol t) ; uppermost binary search result index
  (match-let (((vec size cmp key-proj) ol))
    (let loop ((l 0)
               (r size))
      (cond ((< l r) 
             (let ((m (euclidean-quotient (+ l r) 2)))
               (if (cmp (key-proj (vector-ref vec m)) t)
                   (loop (1+ m) r)
                   (loop l m))))
            (else (1- r))))))

(define (olist-ensure-size! ol) ; resize if at max size
  (match-let (((vec size _ _) ol))
    (begin
      (if (= size (vector-length vec))
        (list-set! ol 0 (vector-copy vec 0 (* 2 size))))
      ol)))

(define (olist-add! ol e) ; add an entry
  (begin
    (olist-ensure-size! ol)
    (match-let* (((vec size _ key-proj) ol)
                (i (1+ (olist-ind-upper ol (key-proj e)))))
      (begin
        (vector-move-right! vec i size vec (1+ i))
        (vector-set! vec i e))
        (list-set! ol 1 (1+ size)))    
    ol))

(define (olist-clear! ol) (begin (list-set! ol 1 0) ol)) ; set size to 0

; unfold each line, concatenate
(define (traverse-with rx f lines)
    (if (null? lines) ; setup invariant: lzd is never null in loop
      '()
      (let loop ((m (regexp-exec rx (car lines)))
                  (lza '())
                  (lzd lines)
                  (lineno 0))
        (cond (m ; process a match
                (cons (f m lza lzd lineno)
                      (loop (regexp-exec rx (car lzd) (match:end m))
                            lza
                            lzd
                            lineno)))
              ((null? (cdr lzd)) '()) ; last match in line, no more lines
              (else ; last match in line, move to next line
                (loop (regexp-exec rx (cadr lzd))
                      (cons (car lzd) lza)
                      (cdr lzd)
                      (1+ lineno)))))))

(define parts-traverser
  (let ((hz-pred
          (lambda (m halfzip)
            (or (null? halfzip)
                (string-every
                  #\.
                  (substring
                    (car halfzip)
                    (max 0 (1- (match:start m)))
                    (min (string-length (car halfzip))
                         (1+ (match:end m))))))))
        (line-pred
          (lambda (nudge index boundary line)
            (or (= index boundary)
                (char=? #\. (string-ref line (nudge index)))))))
    (lambda (m lza lzd lineno)
      (let ((bla (hz-pred m lza))
            (blad (hz-pred m (cdr lzd)))
            (ba (line-pred 1- (match:start m) 0 (car lzd)))
            (bad (line-pred
                  identity
                  (match:end m)
                  (string-length (car lzd))
                  (car lzd))))
        (cons (string->number (match:substring m))
              (and bla blad ba bad))))))

(define (tokens-traverser m lza lzd lineno)
  (list (match:substring m) lineno (match:start m) (match:end m)))

; make a graph out of tokens, only linking between asterisks and numbers
; graph: hashmap from int id to (adjlist of ids, label)
(define parse-into-graph
  (let* ((make-edge ; helper to connect edges
           (lambda (a a-id b b-id) 
             (begin
               (set-car! a (cons b-id (car a)))
               (set-car! b (cons a-id (car b))))))
        (make-edge-with ; helper to connect vertical edges, performing checks first
           (lambda (a a-id bgraph b-as)
             (lambda (b-ai)
               (let ((b-id (cdr (olist-ind-ref b-as b-ai))))
                 (if (and (not (null? (car a))) (= (caar a) b-id))
                   '() ; check we haven't just connected these
                   (let ((b (hash-table-ref bgraph b-id)))
                       (make-edge a a-id b b-id))))))))
    (lambda (tokens)
      (let ((graphs (list (make-hash-table) (make-hash-table))))
        (let loop ((tkns tokens)
                  (cn -1) ; current line number
                  (pass (list (make-olist) (make-olist))) ; previous line anchors
                  (cass (list (make-olist) (make-olist)))) ; current line anchors
          (match tkns
            (() graphs)
            (((label n s e) . ts)
              (if (< cn n) ; new line
                  (begin ; make plas the prev line, make clas empty
                    (for-each olist-clear! pass)
                    (if (< (1+ cn) n) (for-each olist-clear! cass))
                    (loop tkns n cass pass)) ; swap
                  (let* ((ndata (or (string->number label) '())) ; node data
                         (ntype (if (number? ndata) 1 0)) ; node type (color)
                         (ntypec (- 1 ntype)) ; node type, complement
                         (pgraph (list-ref graphs ntypec)) ; previous graph
                         (cgraph (list-ref graphs ntype)) ; current graph
                         (pas (list-ref pass ntypec)) ; previous anchors
                         (cas (list-ref cass ntype)) ; current anchors
                         (node (cons '() ndata)) ; node
                         (nodeid (hash-table-size cgraph))
                         (plli (olist-ind-lower pas (1- s))) ; lower index
                         (plui (olist-ind-upper pas e))) ; upper index
                    (begin
                      (olist-add! cas (cons s nodeid)) ; add start and end to anchors olist
                      (let ((em1 (1- e))) ; add end if it doesn't overlap
                        (if (< s em1) (olist-add! cas (cons em1 nodeid))))
                      (hash-table-set! cgraph nodeid node) ; add node to hash table
                      ; attach to nodes above
                      (for-each
                        (make-edge-with node nodeid pgraph pas)
                        (iota (1+ (- plui plli)) plli))
                      ; attach to node on the left
                      (let* ((b-as (list-ref cass ntypec)))
                        (unless (= (olist-size b-as) 0) ; exists an anchor?
                          (match-let* ((b-ai (1- (olist-size b-as)))
                                       ((b-a . b-id) (olist-ind-ref b-as b-ai))
                                       (b (hash-table-ref pgraph b-id)))
                            (if (= b-a (1- s)) ; last anchor is adjacent
                                  (make-edge node nodeid b b-id)))))
                      (loop ts n pass cass)))))))))))

(define (part-1 lines)
  (fold +
        0
        (map car
             (filter
               (compose not cdr)
               (traverse-with
                 (make-regexp "[0-9]+")
                 parts-traverser
                 lines)))))

(define (part-2 lines)
  (match-let* ((tokens
                 (traverse-with
                   (make-regexp "([0-9]+|\\*)")
                   tokens-traverser
                   lines))
               ((graph-star graph-num) (parse-into-graph tokens)) ; star and number graphs
               (gears
                 (filter (lambda (adjlist)
                           (= (length adjlist) 2))
                         (map car (hash-table-values graph-star))))
               (num-value
                 (lambda (adj-ind)
                    (cdr (hash-table-ref graph-num adj-ind))))
               (gear-value
                 (lambda (adjlist) (fold * 1 (map num-value adjlist)))))
    (fold + 0 (map gear-value gears))))

(define (file->lines path)
  (string-split
    (string-trim-right
      (call-with-input-file path get-string-all)
      #\newline)
    #\newline))

(let ((ls (file->lines (cadr (command-line)))))
  (begin
    (pretty-print (part-1 ls))
    (pretty-print (part-2 ls))))
