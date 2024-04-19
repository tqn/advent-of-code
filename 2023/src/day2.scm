(import
  (rnrs base)
  (rnrs lists)
  (rnrs io ports)
  (ice-9 peg))

(define parse-games
  (let ()
    (define-peg-string-patterns
     "games <-- game* !.
      game <-- label grabs NL*
      label <- labeltext dec CL SP
      labeltext < 'Game' SP
      grabs <-- (grab (SC SP)?)*
      grab <-- (cube-ct (CM SP)?)*
      cube-ct <-- dec SP color
      color <- ('red')/('green')/('blue')
      dec <-- [0-9]+
      CM < ','
      CL < ':'
      SC < ';'
      SP < [ \t\n]*
      NL < '\n'")
    (lambda (text)
      (peg:tree (match-pattern games text)))))

(define (aupdate-with! f)
  (lambda (l p)
    (assoc-set!
      l
      (car p)
      (cond ((assoc-ref l (car p))
             =>
             (lambda (v) (f v (cdr p))))
            (else (cdr p))))))

(define (amerge! f)
  (lambda (l1 l2)
    (fold-left (aupdate-with! f) l1 l2)))

; whether for each (k, v) in constraints, (f v (assoc-ref l k)) holds
(define (apredall f constraints l)
  (let ((test-pair
          (lambda (p)
            (cond ((assoc-ref l (car p))
                   =>
                   (lambda (v) (f (cdr p) v)))
                  (else #t)))))
    (fold-left
      (lambda (x y) (and x y))
      #t
      (map test-pair constraints))))

(define (proc-ct cube-ct)
  (let ((ct (string->number (cadadr cube-ct)))
        (color (caddr cube-ct)))
    (cons color ct)))

(define (proc-grab grab)
  (map proc-ct (cdr grab)))

(define (proc-game game)
  (let* ((label (cadadr game))
         (grabs (map proc-grab (cdaddr game)))
         (folded (fold-left (amerge! max) '() grabs)))
    (cons label folded)))

(define eval-game-1
  (let ((constraints
          '(("red" . 12) ("green" . 13) ("blue" . 14))))
    (lambda (g)
      (let ((gp (proc-game g)))
        (if (apredall >= constraints (cdr gp))
          (string->number (car gp))
          0)))))

(define (eval-game-2 g)
  (fold-left * 1 (map cdr (cdr (proc-game g)))))

(let* ((contents
         (call-with-input-file
           (list-ref (command-line) 1)
           get-string-all))
       (games-tree
         (keyword-flatten
           '(game grab cube-ct dec)
           (parse-games contents)))
       (games-evaled-1
         (map eval-game-1 (cdr games-tree)))
       (games-evaled-2
         (map eval-game-2 (cdr games-tree))))
  (begin
    (display (fold-left + 0 games-evaled-1))
    (display (fold-left + 0 games-evaled-2))))
