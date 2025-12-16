(def grec-fact
  (fn (n)
    (if (identical? n 0)
      1
      (fx* n (grec-fact (fx- n 1))))))

(def lrec-fact
  (letfn ((fact (n)
            (if (identical? n 0)
              1
              (fx* n (fact (fx- n 1))))))
    fact))

(def trec-fact
  (letfn ((fact (acc n)
            (if (identical? n 0)
              acc
              (fact (fx* acc n) (fx- n 1)))))
    (fn (n) (fact 1 n))))
