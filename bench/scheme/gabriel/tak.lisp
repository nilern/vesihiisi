;;; TAK -- A vanilla version of the TAKeuchi function.

(define tak
  (fn (x y z)
    (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y)))))

(define main
  (fn args
    (run-benchmark
      "tak"
      1
      (fn (result) (= result 12))
      (fn (x y z) (fn () (tak x y z)))
      40
      20
      11)))
