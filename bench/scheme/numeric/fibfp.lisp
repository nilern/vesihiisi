;;; FIBFP -- Computes fib(35) using floating point

(define fibfp
  (fn (n)
    (if (< n 2.)
      n
      (+ (fibfp (- n 1.))
         (fibfp (- n 2.))))))

(define main
  (fn args
    (run-benchmark
      "fibfp"
      2
      (fn (result) (= result 9227465.))
      (fn (n) (fn () (fibfp n)))
      35.)))
