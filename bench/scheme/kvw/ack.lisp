;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(define ack
  (fn (m n)
    (cond ((= m 0) (+ n 1))
          ((= n 0) (ack (- m 1) 1))
          (else (ack (- m 1) (ack m (- n 1)))))))

(define main
  (fn args
    (run-benchmark
      "ack"
      10
      (fn (result) (= result 4093))
      (fn (m n) (fn () (ack m n)))
      3
      9)))
