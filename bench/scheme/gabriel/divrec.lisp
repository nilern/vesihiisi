;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.

(def create-n
  (fn (n)
    (letfn (((loop n xs)
              (if (= n 0)
                xs
                (loop (- n 1) (cons () xs))))) ; FIXME: Something very strange happens here
      (loop n ()))))

(def *ll* (create-n 200))

(def recursive-div2
  (fn (l)
    (if (identical? l ())
      ()
      (cons (car l) (recursive-div2 (cddr l))))))

(def main
  (fn args
    (run-benchmark
      "divrec"
      1000000
      (fn (result)
        (= result
           (quote (() () () () () () () () () () () () () () () () () () () ()
                   () () () () () () () () () () () () () () () () () () () ()
                   () () () () () () () () () () () () () () () () () () () ()
                   () () () () () () () () () () () () () () () () () () () ()
                   () () () () () () () () () () () () () () () () () () () ()))))
      (fn (l) (fn () (recursive-div2 l)))
     *ll*)))
