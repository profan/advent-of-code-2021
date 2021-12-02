#lang racket

(define *window-size* 3)
(define (in-numbers s) (sequence-map string->number s))
(define (window-sum v) (for/sum ([e v]) e))

(define (window-sum-with v new)
  (define drop-idx (if (>= (vector-length v) 3) (min (vector-length v) 1) 0))
  (define new-window (vector-append (vector-drop v drop-idx) (vector new)))
  (values new-window (window-sum new-window)))

(define-values (larger-count last-window-sum last-window)
  (call-with-input-file "day_1_input.txt"
    (lambda (in)
      (for/fold
       ([sum 0] [last-window-sum +inf.0] [last-window #()])
       ([(current-value i) (in-indexed (in-numbers (in-lines in)))])
        (define-values (current-window current-window-sum) (window-sum-with last-window current-value))
        (define is-increasing (and (> current-window-sum last-window-sum) (> i (- *window-size*))))
        (values
         (+ sum (if is-increasing 1 0))
         current-window-sum
         current-window)))))

(printf "day 1, part 2: ~a" larger-count)