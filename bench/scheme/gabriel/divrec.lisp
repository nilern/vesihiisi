;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.

(define create-n
  (fn (n)
    (letfn (((loop n xs)
              (if (= n 0)
                xs
                (loop (- n 1) (cons () xs)))))
      (loop n ()))))

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
      (fn (result) (= (count result) 500))
      (fn (l) (fn () (recursive-div2 l)))
      (create-n 1000))))
