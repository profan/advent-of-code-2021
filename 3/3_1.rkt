#lang typed/racket

(define-type Rate (U Zero One))

(: sequence->vector (All (A) (-> (Sequenceof A) (Vectorof A))))
(define (sequence->vector s)
  (list->vector (sequence->list s)))

(: in-binary-integers (-> (Vectorof String) (Vectorof Integer)))
(define (in-binary-integers s) (vector-map (lambda ([e : String]) (assert (string->number e 2) exact-integer?)) s))

(: bit-column (-> (Vectorof Integer) Integer Integer (Vectorof Integer)))
(define (bit-column integers bit-count n)
  (for/vector : (Vectorof Integer) ([i integers])
    (bitwise-bit-field i (- bit-count n 1) (- bit-count n))))

(: more-zeroes-than-ones? (-> (Vectorof Integer) Boolean))
(define (more-zeroes-than-ones? bit-column)
  (define zeros (for/sum : Integer ([i bit-column])(if (= i 0) 1 0)))
  (define ones (for/sum : Integer ([i bit-column]) (if (= i 1) 1 0)))
  (> zeros ones))

(: calculate-gamma-rate (-> (Vectorof Integer) Rate))
(define (calculate-gamma-rate bit-column)
  (if (more-zeroes-than-ones? bit-column) 0 1))

(: calculate-epsilon-rate (-> (Vectorof Integer) Rate))
(define (calculate-epsilon-rate bit-column)
  (if (more-zeroes-than-ones? bit-column) 1 0))

(: run-timed (All (A) (-> (-> A) (Values A Real))))
(define (run-timed timed-fn)
  (define start-time (current-inexact-milliseconds))
  (define function-result (timed-fn))
  (define total-time (- (current-inexact-milliseconds) start-time))
  (values function-result total-time))

(: calculate-power-consumption (-> (Vectorof Integer) Integer Integer))
(define (calculate-power-consumption all-integers bit-count)
  
  (: calculate-rate (-> (-> (Vectorof Integer) Integer) Integer))
  (define (calculate-rate fn)
    (for/fold : Integer ([result 0]) ([n (in-range bit-count)])
      (define column (bit-column all-integers bit-count n))
      (define calculated-rate (fn column))
      (define new-bitwise-result
        (if (= calculated-rate 1)
            (bitwise-ior result (arithmetic-shift 1 (- bit-count n 1)))
            (bitwise-and result (bitwise-not (arithmetic-shift 1 (- bit-count n 1))))))
      new-bitwise-result))

  (define gamma-rate (calculate-rate calculate-gamma-rate))
  (define epsilon-rate (calculate-rate calculate-epsilon-rate))
  (define power-consumption (* gamma-rate epsilon-rate))
  power-consumption)

(define-values (submarine-power-consumption total-run-time)
  (run-timed
   (lambda ()
     (call-with-input-file
         "day_3_input.txt"
       (lambda ([in : Input-Port])
         (define all-lines (sequence->vector (in-lines in)))
         (define all-integers (in-binary-integers all-lines))
         (define bit-count (string-length (vector-ref all-lines 0)))
         (define power-consumption (calculate-power-consumption all-integers bit-count))
         power-consumption)))))

(printf "day 3, part 1: ~a, took: ~a ms" submarine-power-consumption total-run-time)