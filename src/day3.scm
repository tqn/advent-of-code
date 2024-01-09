(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (srfi srfi-43) ; vectors
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
; graph: list of (adjlist, data)
(define parse-into-graph
  (let* ((make-edge ; helper to connect edges
           (lambda (a b) 
             (begin
               (set-car! a (cons b (car a)))
               (set-car! b (cons a (car b))))))
         (make-edge-with ; helper to connect vertical edges, performing checks first
           (lambda (a bgraph b-as)
             (lambda (b-ai)
               (let ((b (cdr (olist-ind-ref b-as b-ai))))
                 (if (or (null? (car a)) (not (eqv? (caar a) b)))
                    (make-edge a b))))))) ; check we haven't just connected these
    (lambda (tokens)
      (let loop ((tkns tokens)
                (cn -1) ; current line number
                (graphs (list '() '()))
                (pass (list (make-olist) (make-olist))) ; previous line anchors
                (cass (list (make-olist) (make-olist)))) ; current line anchors
        (match tkns
          (() graphs)
          (((label n s e) . ts)
            (if (< cn n) ; new line
                (begin ; make plas the prev line, make clas empty
                  (for-each olist-clear! pass)
                  (if (< (1+ cn) n) (for-each olist-clear! cass))
                  (loop tkns n graphs cass pass)) ; swap
                (let* ((ndata (or (string->number label) '())) ; node data
                       (ntype (if (number? ndata) 1 0)) ; node type (color)
                       (ntypec (- 1 ntype)) ; node type, complement
                       (pgraph (list-ref graphs ntypec)) ; previous graph
                       (cgraph (list-ref graphs ntype)) ; current graph
                       (pas (list-ref pass ntypec)) ; previous anchors
                       (cas (list-ref cass ntype)) ; current anchors
                       (node (cons '() ndata)) ; node
                       (plli (olist-ind-lower pas (1- s))) ; lower index
                       (plui (olist-ind-upper pas e))) ; upper index
                  (begin
                    (olist-add! cas (cons s node)) ; add start and end to anchors olist
                    (let ((em1 (1- e))) ; add end if it doesn't overlap
                      (if (< s em1) (olist-add! cas (cons em1 node))))
                    ; attach to nodes above
                    (for-each
                      (make-edge-with node pgraph pas)
                      (iota (1+ (- plui plli)) plli))
                    ; attach to node on the left
                    (let ((b-as (list-ref cass ntypec)))
                      (unless (= (olist-size b-as) 0) ; exists an anchor?
                        (match-let* ((b-ai (1- (olist-size b-as)))
                                     ((b-a . b) (olist-ind-ref b-as b-ai)))
                          (if (= b-a (1- s)) ; last anchor is adjacent
                                (make-edge node b)))))
                    ; reassemble graph
                    (let* ((cgraph' (cons node cgraph)) ; add node to graph
                           (graphs'
                             (if (= ntype 0)
                                 (list cgraph' pgraph)
                                 (list pgraph cgraph'))))
                      (loop ts n graphs' pass cass)))))))))))

; check length of a list in (min len (length lst)) iterations
(define (length= len lst)
  (and
    (>= len 0)
    (let loop ((n len) (xs lst))
      (let ((nullxs (null? xs))
            (eqn0 (= n 0)))
        (if (or nullxs eqn0)
            (and nullxs eqn0)
            (loop (1- n) (cdr xs)))))))

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
  (let* ((tokens
           (traverse-with
             (make-regexp "([0-9]+|\\*)")
             tokens-traverser
             lines))
         (graph-star (car (parse-into-graph tokens))) ; star graph
         (gears
           (filter (lambda (adjlist)
                     (length= 2 adjlist))
                   (map car graph-star)))
         (gear-value
           (lambda (adjlist) (fold * 1 (map cdr adjlist)))))
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
