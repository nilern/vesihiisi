(call-with-prompt 'foo
                  (fn () (+ 1 (call-with-prompt 'bar
                                                ;; (fn () (+ 1 (yield-to 'foo 1)))
                                                (fn () (+ 1 (yield-to 'bar 1)))
                                                (fn (k v)
                                                  (write 'bar) (write-char #" ")
                                                  (write (call-delimited-continuation k v))
                                                  (newline)
                                                  v))))
                  (fn (k v)
                    (write 'foo) (write-char #" ") (write (call-delimited-continuation k v))
                    (newline)
                    v))
