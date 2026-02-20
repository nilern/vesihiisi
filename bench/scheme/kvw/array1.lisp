;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

;;; FIXME: OOM

(define create-x
  (fn (n)
    (let ((result (make-array! n)))
      (induct ((i 0 (+ i 1)))
              ((>= i n) result)
        (array!-set! result i i)))))

(define create-y
  (fn (x)
    (let ((n (array!-count x))
          (result (make-array! n)))
      (induct ((i (- n 1) (- i 1)))
              ((< i 0) result)
        (array!-set! result i (array!-get x i))))))

(define my-try
  (fn (n)
    (array!-count (create-y (create-x n)))))

(define go
  (fn (m n)
    (recur loop ((repeat m)
                 (result ()))
      (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result))))

(define main
  (fn args
    (run-benchmark
      "array1"
      1
      (fn (result) (= result 200000))
      (fn (m n) (fn () (go m n)))
      100
      200000)))
