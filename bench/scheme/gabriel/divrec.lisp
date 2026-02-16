;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.

(define create-n
  (fn (n)
    (induct ((n n (- n 1))
             (a () (cons () a)))
            ((= n 0) a))))

(define recursive-div2
  (fn (l)
    (if (identical? l ())
      ()
      (cons (car l) (recursive-div2 (cddr l))))))

(define main
  (fn args
    (run-benchmark
      "divrec"
      1000000
      (fn (result) (= (count result) 100))
      (fn (l) (fn () (recursive-div2 l)))
      (create-n 200))))
