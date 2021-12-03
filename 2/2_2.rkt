#lang typed/racket

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

(: interpret (-> (Sequenceof Command) submarine))
(define (interpret commands)
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
           (* (submarine-aim sub) n)))]))
  sub)

(define resulting-submarine
  (call-with-input-file
    "day_2_input.txt"
    (lambda ([in : Input-Port])
      (interpret (in-commands (in-lines in))))))

(define final-depth (submarine-y resulting-submarine))
(define final-hozirontal-position (submarine-x resulting-submarine))
(define final-horizontal-position-multiplied-by-depth (* final-hozirontal-position final-depth))

(printf
 "day 2 part 2, horizontal position: ~a, depth: ~a, result: ~a"
 final-hozirontal-position final-depth final-horizontal-position-multiplied-by-depth)