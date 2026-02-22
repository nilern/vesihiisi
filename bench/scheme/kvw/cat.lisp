;;; CAT -- One of the Kernighan and Van Wyk benchmarks.
;;; Based on Will Clinger's more idiomatic Scheme version.

(define catport
  (fn (in out)
    (let ((x (read-char in)))
      (unless (eof-object? x)
        (write-char x out)
        (catport in out)))))

(define go
  (fn (input-file output-file)
    (when (file-exists? output-file)
      (delete-file output-file))
    (call-with-input-file
        input-file
      (fn (in)
        (call-with-output-file
            output-file
          (fn (out)
            (catport in out)))))))

(define main
  (fn args
    (run-benchmark
      "cat"
      1
      (fn (result) #t)
      (fn (input-file output-file) (fn () (go input-file output-file)))
      "kvw/bib"
      "foo")))
