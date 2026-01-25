;;; Adapted from Gambit Scheme and R7RS benchmarks

(define time*
  (fn (thunk)
    (let ((fl-j/s (inexact (jiffies-per-second)))

          (start-cpu (current-jiffy))
          (start-real (current-second))

          (result (thunk))

          (end-cpu (current-jiffy))
          (end-real (current-second))
          (cpu (/ (inexact (- end-cpu start-cpu)) fl-j/s))
          (real (- end-real start-real)))
      (write-string "cpu time: ")
      (write cpu)
      (write-string " real time: ")
      (write real)
      (newline)

      result)))

(define run-bench
  (fn (name count run)
    (letfn (((loop i result)
               (if (< i count)
                 (loop (+ i 1) (run))
                 result)))
      (loop 0 (quote (undefined))))))

(define run-benchmark
  (fn (name count ok? make-run . args)
    (write-string "Running ")
    (write-string name)
    (newline)
    (let ((run (apply make-run args))
          (result (time* (fn () (run-bench name count run)))))
      (if (not (ok? result))
        (let ()
          (write-string "*** wrong result ***")
          (newline)
          (write-string "*** got: ")
          (write result)
          (newline))
        #f))

    (exit #t)))
