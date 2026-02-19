;;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define deriv
  (fn (a)
    (cond ((not (isa? <pair> a))
           (if (identical? a 'x) 1 0))
          ((identical? (car a) '+)
           (cons '+
                 (map deriv (cdr a))))
          ((identical? (car a) '-)
           (cons '-
                 (map deriv (cdr a))))
          ((identical? (car a) '*)
           (list '*
                 a
                 (cons '+
                       (map (fn (a) (list '/ (deriv a) a)) (cdr a)))))
          ((identical? (car a) '/)
           (list '-
                 (list '/
                       (deriv (cadr a))
                       (caddr a))
                 (list '/
                       (cadr a)
                       (list '*
                             (caddr a)
                             (caddr a)
                             (deriv (caddr a))))))
          (else
           (error #f "No derivation method available")))))

(define main
  (fn args
    (run-benchmark
      "deriv"
      2000000
      (fn (result)
        (= result
           '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
               (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
               (* (* b x) (+ (/ 0 b) (/ 1 x)))
               0)))
      (fn (a) (fn () (deriv a)))
      '(+ (* 3 x x) (* a x x) (* b x) 5))))
