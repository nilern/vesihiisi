;;; SUM1 -- One of the Kernighan and Van Wyk benchmarks.

(define sumport
  (fn (port sum-so-far)
    (let ((x (read port)))
      (if (identical? x end)
        sum-so-far
        (sumport port (+ x sum-so-far))))))

(define sum
  (fn (port)
    (sumport port 0.0)))

(define go
  (fn (input)
    (call-with-input-file input sum)))

(define main
  (fn args
    (run-benchmark
      "sum1"
      10
      (fn (result) (and (>= result 15794.974999999)
                        (<= result 15794.975000001)))
      (fn (input) (fn () (go input)))
      "kvw/sum1.data")))
