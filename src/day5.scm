(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (srfi srfi-43) ; vector library
  (srfi srfi-69) ; hash map wrapper
  (ice-9 peg)
  (ice-9 pretty-print)
  (ice-9 match))

; ordered map, small generalization allows any type with key-proj to get key
(define* (make-olist #:key (capacity 4) (cmp <=) (key-proj car))
  (list (make-vector capacity) 0 cmp key-proj)) ; vec size cmp key-proj

(define* (alist->olist l #:rest r)
  (fold (lambda (e ol) (olist-add! ol e)) (apply make-olist r) l))

; some map operations i want to use
(define olist-size cadr)
(define (olist-ind-ref ol i) (vector-ref (car ol) i)) ; underlying vector access
(define (olist-ref-lower ol t) ; lowermost binary search result
  (olist-ind-ref ol (olist-ind-lower ol t)))
(define (olist-ref-upper ol t) ; uppermost binary search result
  (olist-ind-ref ol (olist-ind-upper ol t)))
(define (olist-ind-lower ol t) ; lowermost binary search result index
  (match-let (((vec size cmp key-proj) ol))
    (let go ((l 0)
               (r size))
      (cond ((< l r) 
             (let ((m (euclidean-quotient (+ l r) 2)))
               (if (cmp t (key-proj (vector-ref vec m)))
                   (go l m)
                   (go (1+ m) r))))
            (else l)))))
(define (olist-ind-upper ol t) ; uppermost binary search result index
  (match-let (((vec size cmp key-proj) ol))
    (let go ((l 0)
               (r size))
      (cond ((< l r) 
             (let ((m (euclidean-quotient (+ l r) 2)))
               (if (cmp (key-proj (vector-ref vec m)) t)
                   (go (1+ m) r)
                   (go l m))))
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

(define parse-input
  (let ()
    (define-peg-string-patterns
     "root <-- seedslist SP almanac* !.
      seedslist <- txtseeds SP seeds
      txtseeds < 'seeds:'
      seeds <-- (dec SP)*
      almanac <-- (imap SP)*
      imap <-- header SP ientries
      ientries <-- (ientry SP)*
      ientry <-- dec SP dec SP dec
      header <-- alpha txtto alpha SP txtmap
      txtto < '-to-'
      txtmap < 'map:'
      entry <-- dec SP dec SP dec
      alpha <-- [A-Za-z]+
      dec <-- [0-9]+
      SP < [ \t\n]*
      NL < '\n'")
    (lambda (text)
      (peg:tree (match-pattern root text)))))

(define cst->ast
  (let*((dec (match-lambda (('dec d) (string->number d))))
        (alpha (match-lambda (('alpha s) s)))
        (ientry (match-lambda (('ientry . is) (map dec is))))
        (imap
          (match-lambda (('imap ('header . labels) ('ientries . ientries))
            (list (map alpha labels) (map ientry ientries))))))
    (match-lambda (('root ('seeds . seeds) ('almanac . imaps))
      (list (map dec seeds) (map imap imaps))))))

; table: olist of (src len dst)
(define parse-table
  (match-lambda (((from to) ientries)
    (list from to (alist->olist ientries #:key-proj cadr)))))

(define (parse-almanac imaps)
  (alist->hash-table (map parse-table imaps)))

(define (number-map table) ; assume ranges don't overlap
  (lambda (i)
    (let ((ind (olist-ind-upper table i)))
      (if (< ind 0)
        i
        (match-let (((dst src len) (olist-ind-ref table ind)))
          (if (or (< i src) (>= i (+ src len)))
              i
              (+ dst (- i src))))))))

(define (interval-map table) ; assume ranges don't overlap
  (lambda (init-interval) ; return list of ranges
    (let ((ind0 (max 0 (1- (olist-ind-lower table (car init-interval)))))
          (ind1 (olist-ind-upper table (cdr init-interval))))
      (let loop ((ti ind0)
                 (interval init-interval))
        (if (> ti ind1)
          (list interval)
          (match-let*(((dst src len) (olist-ind-ref table ti))
                      (d (- dst src))
                      ((i0 . i1) interval)
                      (j0 (min i1 src))
                      (j1 (max i0 (+ src len)))
                      (lsubint (if (< i0 j0) `((,i0 . ,j0)) '()))
                      (rsubint (if (< j1 i1) `((,j1 . ,i1)) '()))
                      (m0 (max i0 j0))
                      (m1 (min i1 j1))
                      (msubint (if (< m0 m1) `((,(+ m0 d) . ,(+ m1 d))) '())))
            (append lsubint
                    msubint
                    (if (null? rsubint) '() (loop (1+ ti) (car rsubint))))))))))

(define (iter-map-with lookup collect almanac init-name terminal-name init-items)
  (let loop ((tablename init-name)
             (items init-items))
    (if (string=? tablename terminal-name)
      items
      (match-let (((to table) (hash-table-ref almanac tablename)))
        (loop to (collect (map (lookup table) items)))))))

(define (exact-chunks xs n) ; errors if n does not divide (length xs)
  (cdr
    (unfold
      (compose null? cdr)
      car
      (lambda (asbs)
        (call-with-values (lambda () (split-at (cdr asbs) n)) cons))
      (cons '() xs))))

(define (seeds-as-intervals seeds)
  (map (lambda (p) (cons (car p) (+ (car p) (cadr p)))) (exact-chunks seeds 2)))

(define (part-1 seeds almanac)
  (apply min (iter-map-with number-map
                            identity
                            almanac
                            "seed"
                            "location"
                            seeds)))

(define (part-2 seeds almanac)
  (apply min (map car (iter-map-with interval-map
                                     concatenate
                                     almanac
                                     "seed"
                                     "location"
                                     (seeds-as-intervals seeds)))))

(define (file->string path)
  (string-trim-right
    (call-with-input-file path get-string-all)
    #\newline))

(match-let* ((contents (file->string (cadr (command-line))))
             ((seeds imaps) (cst->ast (parse-input contents)))
             (almanac (parse-almanac imaps)))
  (begin
    (pretty-print (part-1 seeds almanac))
    (pretty-print (part-2 seeds almanac))))
