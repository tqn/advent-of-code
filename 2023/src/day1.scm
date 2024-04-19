(import
  (rnrs base)
  (rnrs lists)
  (rnrs io ports)
  (ice-9 regex))

(define name->number
  '(("zero" . 0)
    ("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)))

(define (calibration-value line)
  (let ((f (lambda (g)
             (string-ref line (g line char-numeric?)))))
    (string->number
      (string-append (f string-index) (f string-rindex)))))

(define calibration-value-ext
  (let* ((names (map car name->number))
         (number-abs
           (lambda (f)
             (let ((rx (make-regexp
                         (string-append
                           "([0-9]|"
                           (string-join (map f names) "|")
                           ")"))))
               (lambda (line)
                 (let ((matched
                         (f (match:substring (regexp-exec rx (f line))))))
                   (or (string->number matched)
                       (assoc-ref name->number matched)))))))
         (number-fwd (number-abs identity))
         (number-rev (number-abs string-reverse)))
    (lambda (line)
      (+ (* 10 (number-fwd line)) (number-rev line)))))

(define (file->lines path)
  (string-split
    (string-trim-right
      (call-with-input-file path get-string-all)
      #\newline)
    #\newline))

; part 1
(display
  (fold-left
    +
    0
    (map calibration-value
         (file->lines (list-ref (command-line) 1)))))
(newline)

; part 2
(display
  (fold-left
    +
    0
    (map calibration-value-ext
         (file->lines (list-ref (command-line) 1)))))
