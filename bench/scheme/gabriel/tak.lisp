;;; TAK -- A vanilla version of the TAKeuchi function.

(def tak
  (fn (x y z)
    (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y)))))

(def main
  (fn args
    (run-benchmark
      "tak"
      2000
      (fn (result) (= result 7))
      (fn (x y z) (fn () (tak x y z)))
      18
      12
      6)))
