(def even?
  (letfn (((even? n)
             (if (identical? n 0)
               #t
               (odd? (fx- n 1))))
          ((odd? n)
             (if (identical? n 0)
               #f
               (even? (fx- n 1)))))
    even?))
