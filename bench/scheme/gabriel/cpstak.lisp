;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define cpstak
  (fn (x y z)
    (letfn (((tak x y z k)
              (if (not (< y x))
                (k z)
                (tak (- x 1)
                     y
                     z
                     (fn (v1)
                       (tak (- y 1)
                            z
                            x
                            (fn (v2)
                              (tak (- z 1)
                                   x
                                   y
                                   (fn (v3)
                                     (tak v1 v2 v3 k))))))))))
      (tak x y z (fn (a) a)))))

(define main
  (fn args
    (run-benchmark
      "cpstak"
      1000
      (fn (result) (= result 7))
      (fn (x y z) (fn () (cpstak x y z)))
      18
      12
      6)))
