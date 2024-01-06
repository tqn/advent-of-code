(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (srfi srfi-43) ; vector library
  (srfi srfi-69) ; hash map wrapper
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

(define (walk dirs adjlists init-name terminal-name)
  (let ((terminal-node (hash-table-ref adjlists terminal-name)))
    (let loop ((node (hash-table-ref adjlists init-name))
               (i 0))
      (if (eq? node terminal-node)
        i
        (loop
          ((case (vector-ref dirs (modulo i (vector-length dirs)))
             ((#\L) car) ((#\R) cdr))
           (cdr node))
          (1+ i))))))

(define (walk-pred dirs adjlists init-names term?)
  (let ((dirfuns (vector-map (lambda (_ d) (case d ((#\L) cadr) ((#\R) cddr))) dirs)))
    (let loop ((nodes (map (lambda (name) (hash-table-ref adjlists name)) init-names))
               (i 0))
      ; (if (= (modulo i (* 271 1000)) 0) (begin (map display (list i ": " (map car nodes))) (newline)))
      (if (term? nodes)
        i
        (loop (map (vector-ref dirfuns (modulo i (vector-length dirs))) nodes) (1+ i))))))

(define (part-1 dirs adjlists) (walk dirs adjlists "AAA" "ZZZ"))

(define (part-2 dirs adjlists)
  (walk-pred
    dirs
    adjlists
    (filter (lambda (n) (string-suffix? "A" n)) (hash-table-keys adjlists))
    (let*((nzs (filter (lambda (n) (string-suffix? "Z" n)) (hash-table-keys adjlists)))
          (zt (alist->hash-table (map list nzs))))
      (lambda (ns) (every (lambda (n) (hash-table-exists? zt (car n))) ns)))))

(define (file->string path) (call-with-input-file path get-string-all))

(match-let*((cst (parse-input (file->string (cadr (command-line)))))
            ((dirs-list entries) (cst->ast cst))
            (dirs (list->vector dirs-list))
            (adjlists (make-adjlists entries)))
  (begin
    (pretty-print (part-1 dirs adjlists))
    (pretty-print (part-2 dirs adjlists))
    ))
