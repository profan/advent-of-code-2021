#lang typed/racket

(require plot)

(struct submarine ([x : Integer] [y : Integer] [aim : Integer]) #:mutable)

(struct up ([n : Integer]))
(struct down ([n : Integer]))
(struct forward ([n : Integer]))
(define-type Command (U up down forward))

(: parse-command (-> String Command))
(define (parse-command line)
  (match (string-split line)
    [(list "up" n) (up (assert (string->number n) exact-integer?))]
    [(list "down" n) (down (assert (string->number n) exact-integer?))]
    [(list "forward" n) (forward (assert (string->number n) exact-integer?))]))

(: in-commands (-> (Sequenceof String) (Sequenceof Command)))
(define (in-commands s) (sequence-map parse-command s))

(: interpret (->* ((Sequenceof Command)) ((U Null (-> submarine Void))) submarine))
(define (interpret commands [fn '()])
  (define sub (submarine 0 0 0))
  (for ([cmd commands])
    (match cmd
      [(up n)
       (set-submarine-aim!
        sub (- (submarine-aim sub) n))]
      [(down n)
       (set-submarine-aim!
        sub (+ (submarine-aim sub) n))]
      [(forward n)
       (set-submarine-x!
        sub (+ (submarine-x sub) n))
       (set-submarine-y!
        sub
        (+ (submarine-y sub)
           (* (submarine-aim sub) n)))])
    (when (not (null? fn))
      (fn sub)))
  sub)

(: submarine-path (Listof (List Real Real)))
(define submarine-path '())

(: push-submarine-position (-> submarine Void))
(define (push-submarine-position sub)
  (set! submarine-path (append submarine-path (list (list (submarine-x sub) (submarine-y sub))))))
  
(define resulting-submarine
  (call-with-input-file
    "day_2_input.txt"
    (lambda ([in : Input-Port])
      (interpret
       (in-commands (in-lines in))
       push-submarine-position))))

(define final-depth (submarine-y resulting-submarine))
(define final-horizontal-position (submarine-x resulting-submarine))
(define final-horizontal-position-multiplied-by-depth (* final-horizontal-position final-depth))

;; we want our plot in a new window
(plot-new-window? #t)

(plot
 (lines
  submarine-path
  #:label "submarine path")
 #:x-label "horizontal position"
 #:y-label "depth")