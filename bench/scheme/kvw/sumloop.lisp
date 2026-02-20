;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.

(define sum 0)

(define tail-rec-aux
  (fn (i n)
    (if (< i n)
      (do (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum)))

(define tail-rec-loop
  (fn (n)
    (set! sum 0)
    (tail-rec-aux 0 n)
    sum))

(define do-loop
  (fn (n)
    (set! sum 0)
    (induct ((i 0 (+ i 1)))
            ((>= i n) sum)
      (set! sum (+ sum 1)))))

(define main
  (fn args
    (run-benchmark
      "sumloop"
      10
      (fn (result) (= result 100000000))
      (fn (n) (fn () (do-loop n)))
      100000000)))
