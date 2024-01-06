(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (srfi srfi-43) ; vector library
  (ice-9 peg)
  (ice-9 pretty-print)
  (ice-9 match))

(define parse-cards
  (let ()
    (define-peg-string-patterns
     "cards <-- card* !.
      card <-- label wants SP VB SP haves NL*
      label <- labeltext dec CL SP
      labeltext < 'Card' SP
      wants <-- (dec SP)*
      haves <-- (dec SP)*
      dec <-- [0-9]+
      CM < ','
      CL < ':'
      VB < '|'
      SP < [ \t\n]*
      NL < '\n'")
    (lambda (text)
      (peg:tree (match-pattern cards text)))))

(define cst->ast
  (let*((dec (match-lambda (('dec d) (string->number d))))
        (card
          (match-lambda (('card cardnumdec ('wants . ws) ('haves . hs))
            (list (dec cardnumdec) (map dec ws) (map dec hs))))))
    (match-lambda (('cards . cs) (map card cs)))))

(define (card-matchcount-with table)
  (match-lambda ((cardnum wants haves)
    (begin
      (hash-clear! table)
      ; build the hash table, using it as a hash set
      (for-each (lambda (n) (hash-set! table n '())) wants)
      (length (filter (lambda (n) (hash-ref table n)) haves))))))

(define part-1
  (let ((points (lambda (ct) (if (> ct 0) (expt 2 (1- ct)) 0))))
    (lambda (cards)
      (fold + 0 (map (compose points (card-matchcount-with (make-hash-table))) cards)))))

(define (part-2 cards)
  (let*((values (list->vector (map (card-matchcount-with (make-hash-table)) cards)))
        (cts (make-vector (vector-length values) 1))) ; multiplicity of each card
    (vector-fold
      (lambda (i total ct)
        (let ((ct (vector-ref cts i)))
          (begin
            (for-each ; adjust cts of later cards
              (lambda (j) (vector-set! cts j (+ ct (vector-ref cts j))))
              (iota (vector-ref values i) (1+ i)))
            (+ total ct))))
      0
      cts)))

(define (file->string path)
  (string-trim-right
    (call-with-input-file path get-string-all)
    #\newline))

(let* ((contents (file->string (cadr (command-line))))
       (ast (cst->ast (parse-cards contents))))
  (begin
    (pretty-print (part-1 ast))
    (pretty-print (part-2 ast))))
