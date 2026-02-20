;;; WC -- One of the Kernighan and Van Wyk benchmarks.
;;; Based on Will Clinger's more idiomatic (and correct!) Scheme version.

(define wcport
  (fn (port)
    (recur loop ((nl 0) (nw 0) (nc 0) (inword? #f))
      (let ((x (read-char port)))
        (cond ((identical? x end)
               (list nl nw nc))
              ((identical? x #" ")
               (loop nl nw (+ nc 1) #f))
              ((identical? x #"\n")
               (loop (+ nl 1) nw (+ nc 1) #f))
              (else
               (loop nl (if inword? nw (+ nw 1)) (+ nc 1) #t)))))))

(define go
  (fn (x)
    (call-with-input-file x wcport)))

(define main
  (fn args
    (run-benchmark
      "wc"
      1
      (fn (result) (= result '(31102 851820 4460056)))
      (fn (x) (fn () (go x)))
      "kvw/bib")))
