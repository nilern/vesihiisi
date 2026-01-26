(try-yield-at* 'foo
                (fn () (+ 1 (try-yield-at* 'bar
                                           (fn () (+ 1 (yield-to 'foo 1)))
                                           ;; (fn () (+ 1 (yield-to 'bar 1)))
                                           (fn (p v k)
                                             (write 'bar) (write-char #" ") (write (k v)) (newline)
                                             (k v))
                                           (fn (v) v))))
                (fn (p v k)
                  (write 'foo) (write-char #" ") (write (k v)) (newline)
                  (k v))
                (fn (v) v))
