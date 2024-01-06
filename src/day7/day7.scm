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
     "root <-- (entry NL?)* !.
      entry <-- hand HS dec
      hand <-- card card card card card
      card <-- [2-9TJQKA]
      dec <-- [0-9]+
      HS < [ \t]*
      NL < '\n'")
    (lambda (text) (peg:tree (match-pattern root text)))))

(define cst->ast
  (let*((dec (match-lambda (('dec d) (string->number d))))
        (card (match-lambda (('card c) (string-ref c 0))))
        (entry (match-lambda (('entry ('hand . cards) bid)
                 (list (map card cards) (dec bid))))))
    (match-lambda (('root . entries)
      (map entry entries)))))

(define (card-value c)
  (if (char-set-contains? char-set:digit c)
    (- (char->integer c) (char->integer #\0))
    (case c
      ((#\T) 10)
      ((#\J) 11)
      ((#\Q) 12)
      ((#\K) 13)
      ((#\A) 14))))

(define (card-joker-value c)
  (case c ((#\J) 1)
          (else => card-value)))

(define partition->type
  (let*((types #((1 1 1 1 1)
                 (2 1 1 1)
                 (2 2 1)
                 (3 1 1)
                 (3 2)
                 (4 1)
                 (5)))
        (table (alist->hash-table (vector->list (vector-map xcons types)))))
    (lambda (partition) (hash-table-ref table partition))))

(define (list->multiset xs)
  (let ((table (make-hash-table)))
    (for-each (lambda (c) (hash-table-update!/default table c 1+ 0)) xs)
    table))

(define (hand-type cards)
  (partition->type (sort (hash-table-values (list->multiset cards)) >=)))

(define (hand-joker-type cards)
  (let*((mset (list->multiset cards))
        (jct (hash-table-ref/default mset 1 0))) ; joker count
    (hash-table-delete! mset 1)
    (partition->type
      (match (sort (hash-table-values mset) >=)
        (() (list jct))
        ((p . ps) (cons (+ jct p) ps))))))

(define*(lexcmp #:optional (leq <=) (eq =)) ; lexicographic comparison of lists
  (lambda (x y)
    (let loop ((a x)
               (b y))
      (or (null? a)
          (and (not (null? b))
               (or (not (leq (car b) (car a)))
                   (and (eq (car a) (car b))
                        (loop (cdr a) (cdr b)))))))))

(define (total-winnings-with cval htype)
  (lambda (ast)
    (let ((entries (map (lambda (e) (cons (map cval (car e)) (cdr e))) ast)))
      (vector-fold
        (lambda (_ u v) (+ u v))
        0
        (vector-map
          (lambda (i v) (* (1+ i) (caddr v)))
          (sort (list->vector (map (lambda (e) (cons (htype (car e)) e)) entries))
                (match-lambda* (((at ah . _) (bt bh . _))
                  ((lexcmp) (cons at ah) (cons bt bh))))))))))

(define part-1 (total-winnings-with card-value hand-type))

(define part-2 (total-winnings-with card-joker-value hand-joker-type))

(define (file->string path)
  (string-trim-right
    (call-with-input-file path get-string-all)
    #\newline))

(let ((ast (cst->ast (parse-input (file->string (cadr (command-line)))))))
  (begin
    (pretty-print (part-1 ast)))
    (pretty-print (part-2 ast)))
