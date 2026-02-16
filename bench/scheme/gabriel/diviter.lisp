;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(define create-n
  (fn (n)
    (induct ((n n (- n 1))
             (a () (cons () a)))
            ((= n 0) a))))

(define iterative-div2
  (fn (l)
    (induct ((l l (cddr l))
             (a () (cons (car l) a)))
            ((identical? l ()) a))))

(define main
  (fn args
    (run-benchmark
      "diviter"
      1000000
      (fn (result) (= (count result) 100))
      (fn (l) (fn () (iterative-div2 l)))
      (create-n 200))))
