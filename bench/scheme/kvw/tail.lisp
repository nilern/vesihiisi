;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.
;;;
;;; The key idea of this benchmark is that, for each iteration,
;;; the entire input is read line by line before any output
;;; is produced, and the lines are then written to the output
;;; in the reverse of the order in which they were read.

(define tail-r-aux
  (fn (port file-so-far)
    (let ((x (read-line port)))
      (if (identical? x end)
        file-so-far
        (tail-r-aux port (cons x file-so-far))))))

(define echo-lines-in-reverse-order
  (fn (in out)
    (for-each (fn (line) (write-string line out) (newline out))
              (tail-r-aux in '()))))

(define go
  (fn (input output)
    (call-with-input-file
        input
      (fn (in)
        (when (file-exists? output)
          (delete-file output))
        (call-with-output-file
            output
          (fn (out)
            (echo-lines-in-reverse-order in out)))))))

(define main
  (fn (args)
    (run-benchmark
      "tail"
      1
      (fn (result) #t)
      (fn (input output) (fn () (go input output))
      "kvw/bib"
      "foo"))))
