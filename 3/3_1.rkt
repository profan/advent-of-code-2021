#lang typed/racket

(: in-binary-integers (-> (Sequenceof String) (Sequenceof Integer)))
(define (in-binary-integers s) (sequence-map (lambda ([e : String]) (assert (string->number e 2) exact-integer?)) s))

(: integer->string (-> Integer String))
(define (integer->string i)
  (number->string i))

(: bit-column (-> (Listof String) Integer Integer (Vectorof Integer)))
(define (bit-column lines bit-count n)
  (for/vector : (Vectorof Integer)
    ([i (in-binary-integers lines)])
    (bitwise-bit-field i (- bit-count n 1) (- bit-count n))))

(: more-zeroes-than-ones? (-> (Vectorof Integer) Boolean))
(define (more-zeroes-than-ones? bit-column)
  (define zeros (for/sum : Integer ([i bit-column])(if (= i 0) 1 0)))
  (define ones (for/sum : Integer ([i bit-column]) (if (= i 1) 1 0)))
  (> zeros ones))

(: calculate-gamma-rate (-> (Vectorof Integer) Integer))
(define (calculate-gamma-rate bit-column)
  (if (more-zeroes-than-ones? bit-column) 0 1))

(: calculate-epsilon-rate (-> (Vectorof Integer) Integer))
(define (calculate-epsilon-rate bit-column)
  (if (more-zeroes-than-ones? bit-column) 1 0))

(define submarine-power-consumption
  (call-with-input-file
    "day_3_input.txt"
    (lambda ([in : Input-Port])   
      (define all-lines (sequence->list (in-lines in)))
      (define bit-count (string-length (assert (first all-lines) string?)))

      (: calculate-rate (-> (-> (Vectorof Integer) Integer) Integer))
      (define (calculate-rate fn)
        (define gamma-rates
          (for/list : (Listof String) ([n (in-range bit-count)])
            (define column (bit-column all-lines bit-count n))
            (define calculated-rate (fn column))
            (integer->string calculated-rate)))
        (define combined-rate-bit-string (string-join gamma-rates "")) ;; this is kinda ugly.. :D shouldn't need strings really
        (define combined-rate (assert (string->number combined-rate-bit-string 2) exact-integer?))
        combined-rate)
      
      (define gamma-rate (calculate-rate calculate-gamma-rate))
      (define epsilon-rate (calculate-rate calculate-epsilon-rate))
      (define power-consumption (* gamma-rate epsilon-rate))
      power-consumption)))

(printf "day 3, part 1: ~a" submarine-power-consumption)