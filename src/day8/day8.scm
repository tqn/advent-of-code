(import
  (rnrs io ports)
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

(define (walk dirs adjlists init-name term?)
  (let ((dirfuns (vector-map (lambda (_ d) (case d ((#\L) cadr) ((#\R) cddr))) dirs)))
    (let loop ((node (hash-table-ref adjlists init-name))
               (i 0))
      (if (term? node)
        i
        (loop ((vector-ref dirfuns (modulo i (vector-length dirs))) node) (1+ i))))))

(define (part-1 dirs adjlists)
  (walk dirs adjlists "AAA" (lambda (n) (string=? (car n) "ZZZ"))))

(define (part-2 dirs adjlists)
  (let*((init-names
         (filter (lambda (n) (string-suffix? "A" n)) (hash-table-keys adjlists)))
        (nzs (filter (lambda (n) (string-suffix? "Z" n)) (hash-table-keys adjlists)))
        (zt (alist->hash-table (map list nzs)))
        (term? (lambda (n) (hash-table-exists? zt (car n))))
        (walk-one (lambda (init-name) (walk dirs adjlists init-name term?))))
    (list-transduce (tmap walk-one) lcm init-names) ))

(define (file->string path) (call-with-input-file path get-string-all))

(match-let*((cst (parse-input (file->string (cadr (command-line)))))
            ((dirs-list entries) (cst->ast cst))
            (dirs (list->vector dirs-list))
            (adjlists (make-adjlists entries)))
  (pretty-print (part-1 dirs adjlists))
  (pretty-print (part-2 dirs adjlists)))
