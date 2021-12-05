#lang typed/racket

(: ~ (-> Integer Integer))
(define (~ n) (bitwise-not n))

(: & (-> Integer Integer Integer))
(define (& n m) (bitwise-and n m))

(: << (-> Integer Integer Integer))
(define (<< n s) (arithmetic-shift n s))

(define-type Rate (U Zero One))

(: in-binary-integers (-> (Vectorof String) (Vectorof Integer)))
(define (in-binary-integers s) (vector-map (lambda ([e : String]) (assert (string->number e 2) exact-integer?)) s))

(: bit-column (-> (Vectorof Integer) Integer Integer (Vectorof Integer)))
(define (bit-column integers bit-count n)
  (for/vector : (Vectorof Integer) ([i integers])
    (bitwise-bit-field i (- bit-count n 1) (- bit-count n))))

(: more-zeroes-than-ones? (-> (Vectorof Integer) Boolean))
(define (more-zeroes-than-ones? bit-column)
  (define zeros (for/sum : Integer ([i bit-column] #:when (= i 0)) 1))
  (define ones (- (vector-length bit-column) zeros))
  (> zeros ones))

(: calculate-gamma-rate (-> (Vectorof Integer) Rate))
(define (calculate-gamma-rate bit-column)
  (if (more-zeroes-than-ones? bit-column) 0 1))

(: calculate-final-epsilon-rate (-> Integer Integer Integer))
(define (calculate-final-epsilon-rate gamma-rate bit-count)
  (& (~ gamma-rate) (- (<< 1 bit-count) 1)))

(: run-timed (All (A) (-> (-> A) (Values A Real))))
(define (run-timed timed-fn)
  (define start-time (current-inexact-milliseconds))
  (define function-result (timed-fn))
  (define total-time (- (current-inexact-milliseconds) start-time))
  (values function-result total-time))

(: calculate-power-consumption (-> (Vectorof Integer) Integer Integer))
(define (calculate-power-consumption all-integers bit-count)
  
  (: calculate-rate (-> (-> (Vectorof Integer) Rate) Integer))
  (define (calculate-rate rate-fn)
    (for/fold : Integer ([result 0]) ([n (in-range bit-count)])
      (define column (bit-column all-integers bit-count n))
      (define new-bitwise-result
        (if (= (rate-fn column) 1)
            (bitwise-ior result (<< 1 (- bit-count n 1)))
            (bitwise-and result (~ (<< 1 (- bit-count n 1))))))
      new-bitwise-result))

  (define gamma-rate (calculate-rate calculate-gamma-rate))
  (define epsilon-rate (calculate-final-epsilon-rate gamma-rate bit-count))
  (define power-consumption (* gamma-rate epsilon-rate))
  power-consumption)

(define-values (submarine-power-consumption total-run-time)
  (run-timed
   (lambda ()
     (call-with-input-file
         "day_3_input.txt"
       (lambda ([in : Input-Port])
         (define all-lines (list->vector (port->lines in)))
         (define all-integers (in-binary-integers all-lines))
         (define bit-count (string-length (vector-ref all-lines 0)))
         (define power-consumption (calculate-power-consumption all-integers bit-count))
         power-consumption)))))

(printf "day 3, part 1: ~a, took: ~a ms" submarine-power-consumption total-run-time)