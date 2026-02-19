;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(define run
  (fn (n)
    (recur loop ((i n) (sum 0.))
      (if (< i 0.)
        sum
        (loop (- i 1.) (+ i sum))))))

(define main
  (fn args
    (run-benchmark
      "sumfp"
      20000
      (fn (result) (= result 50005000.))
      (fn (n) (fn () (run n)))
      10000.)))
