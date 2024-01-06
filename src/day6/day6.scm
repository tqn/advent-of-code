(import
  (rnrs io ports)
  (srfi srfi-1) ; list library
  (ice-9 peg)
  (ice-9 pretty-print)
  (ice-9 match))

(define parse-input
  (let ()
    (define-peg-string-patterns
     "root <-- txttimes HS decs NL* txtdistances HS decs !.
      txttimes < 'Time:'
      txtdistances < 'Distance:'
      decs <-- (dec HS)*
      dec <-- [0-9]+
      HS < [ \t]*
      NL < '\n'")
    (lambda (text)
      (peg:tree (match-pattern root text)))))

(define cst->races
  (let ((dec (match-lambda (('dec d) (string->number d)))))
    (match-lambda (('root ('decs . times) ('decs . distances))
      (zip (map dec times) (map dec distances))))))

(define cst->race
  (let ((spaceddec
         (lambda (decs) (string->number (string-concatenate (map cadr decs))))))
    (match-lambda (('root ('decs . times) ('decs . distances))
      (list (spaceddec times) (spaceddec distances))))))

(define count-race-solutions
  (match-lambda ((t d)
    (let ((halfdiscr (/ (sqrt (- (expt t 2) (* 4 d))) 2)))
      (if (real? halfdiscr)
        (if (even? t)
          (1- (* 2 (inexact->exact (ceiling halfdiscr))))
          (* 2 (inexact->exact (ceiling (- halfdiscr 1/2)))))
        0)))))

(define (part-1 races) (fold * 1 (map count-race-solutions races)))

(define part-2 count-race-solutions)

(define (file->string path)
  (string-trim-right
    (call-with-input-file path get-string-all)
    #\newline))

(let*((cst (parse-input (file->string (cadr (command-line)))))
      (races (cst->races cst))
      (race (cst->race cst)))
  (begin
    (pretty-print (part-1 races)))
    (pretty-print (part-2 race)))
