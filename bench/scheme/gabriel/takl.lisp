;;; TAKL -- The TAKeuchi function using lists as counters.

(define listn
  (fn (n)
    (if (= n 0)
      ()
      (cons n (listn (- n 1))))))

(define l18 (listn 18))
(define l12 (listn 12))
(define l6 (listn 6))

(define mas
  (fn (x y z)
    (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y)))))

(define shorterp
  (fn (x y)
    (if (not (identical? y ()))
      (if (identical? x ())
        #t
        (shorterp (cdr x)
                  (cdr y)))
      #f)))

(define main
  (fn args
    (run-benchmark
      "takl"
      300
      (fn (result) (= result '(7 6 5 4 3 2 1)))
      (fn (x y z) (fn () (mas x y z)))
      l18
      l12
      l6)))
