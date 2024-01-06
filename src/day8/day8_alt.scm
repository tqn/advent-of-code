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
      node <-- [A-Z]+
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

(define (walk dirs-list adjlists init-nodes term?)
  (let ((dirs (list->vector dirs-list)))
    (let loop ((nodes init-nodes)
               (i 0))
      (if (= (modulo i 1000000) 0) (begin (map display (list i ": " nodes)) (newline)))
      (if (term? nodes)
        i
        (loop
          (map (lambda (node)
                 ((case (vector-ref dirs (modulo i (vector-length dirs)))
                   ((#\L) car) ((#\R) cadr))
                 (hash-table-ref adjlists node)))
               nodes)
          (1+ i))))))

(define (part-1 dirs adjlists)
  (walk dirs
        adjlists
        '("AAA")
        (lambda (ns) (every (lambda (n) (string=? n "ZZZ")) ns))))

(define (part-2 dirs adjlists)
  (walk dirs
        adjlists
        (filter (lambda (n) (string-suffix? "A" n)) (hash-table-keys adjlists))
        (lambda (ns) (every (lambda (n) (string-suffix? "Z" n)) ns))))

(define (file->string path) (call-with-input-file path get-string-all))

(match-let*((cst (parse-input (file->string (cadr (command-line)))))
            ((dirs entries) (cst->ast cst))
            (adjlists (alist->hash-table entries)))
  (begin
    ; (pretty-print (part-1 dirs adjlists))
    (pretty-print (part-2 dirs adjlists))))
