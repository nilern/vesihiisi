;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(define create-n
  (fn (n)
    (letfn (((loop n xs)
              (if (= n 0)
                xs
                (loop (- n 1) (cons () xs)))))
      (loop n ()))))

(define iterative-div2
  (fn (l)
    (letfn (((loop l a)
              (if (identical? l ())
                a
                (loop (cddr l) (cons (car l) a)))))
      (loop l ()))))

(define main
  (fn args
    (run-benchmark
      "diviter"
      1000000
      (fn (result) (= (count result) 100))
      (fn (l) (fn () (iterative-div2 l)))
      (create-n 200))))
