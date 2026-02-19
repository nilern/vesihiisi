;;; FIB -- A classic benchmark, computes fib(35) inefficiently.

(define fib
  (fn (n)
    (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2))))))

(define main
  (fn args
    (run-benchmark
      "fib"
      5
      (fn (result) (= result 9227465))
      (fn (n) (fn () (fib n)))
      35)))
