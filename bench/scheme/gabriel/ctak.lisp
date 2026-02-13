;;; CTAK -- A version of the TAK procedure that uses continuations.

(define ctak
  (fn (x y z)
    (call-with-current-continuation
      (fn (k) (ctak-aux k x y z)))))

(define ctak-aux
  (fn (k x y z)
    (if (not (< y x))
      (continue k z)
      (call-with-current-continuation
        (fn (k)
          (ctak-aux
            k
            (call-with-current-continuation
              (fn (k) (ctak-aux k (- x 1) y z)))
            (call-with-current-continuation
              (fn (k) (ctak-aux k (- y 1) z x)))
            (call-with-current-continuation
              (fn (k) (ctak-aux k (- z 1) x y)))))))))

(define main
  (fn args
    (run-benchmark
      "ctak"
      160
      (fn (result) (= result 7))
      (fn (x y z) (fn () (ctak x y z)))
      18
      12
      6)))
