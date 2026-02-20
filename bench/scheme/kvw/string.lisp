;;; STRING -- One of the Kernighan and Van Wyk benchmarks.

(define s "abcdef")

(define grow
  (fn ()
    (set! s (string-concat "123" s "456" s "789"))
    (set! s (string-concat
              (substring s (quotient (string-count s) 2) (string-count s))
              (substring s 0 (+ 1 (quotient (string-count s) 2)))))
    s))

(define trial
  (fn (n)
    (induct ((i 0 (+ i 1)))
            ((> (string-count s) n) (string-count s))
      (grow))))

(define my-try
  (fn (n)
    (induct ((i 0 (+ i 1)))
            ((>= i 10) (string-count s))
      (set! s "abcdef")
      (trial n))))

(define main
  (fn args
    (run-benchmark
      "string"
      10
      (fn (result) (= result 524278))
      (fn (n) (fn () (my-try n)))
      500000)))
