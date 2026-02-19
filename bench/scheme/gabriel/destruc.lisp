;;; DESTRUC -- Destructive operation benchmark.

(define append-to-tail!
  (fn (x y)
    (if (identical? x ())
      y
      (letfn (((loop a b)
                 (if (identical? b ())
                   (do
                     (set-cdr! a y)
                     x)
                   (loop b (cdr b)))))
        (loop x (cdr x))))))

(define destructive
  (fn (n m)
    (let ((l (induct ((i 10 (- i 1)) (a () (cons () a)))
                     ((= i 0) a))))
      (induct ((i n (- i 1)))
              ((= i 0) l)
        (cond ((identical? (car l) ())
               (induct ((l l (cdr l)))
                       ((identical? l ()))
                 (when (identical? (car l) ())
                   (set-car! l (cons () ())))
                 (append-to-tail! (car l)
                                  (induct ((j m (- j 1)) (a () (cons () a)))
                                          ((= j 0) a)))))
              (else
               (induct ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                       ((identical? l2 ()))
                 (set-cdr! (induct ((j (quotient (count (car l2)) 2) (- j 1))
                                   (a (car l2) (cdr a)))
                               ((= j 0) a)
                             (set-car! a i))
                           (let ((n (quotient (count (car l1)) 2)))
                             (cond ((= n 0)
                                    (set-car! l1 ())
                                    (car l1))
                                   (else
                                    (induct ((j n (- j 1)) (a (car l1) (cdr a)))
                                            ((= j 1)
                                      (let ((x (cdr a)))
                                        (set-cdr! a ())
                                        x))
                                      (set-car! a i)))))))))))))

(define main
  (fn args
    (run-benchmark
      "destruc"
      500
      (fn (result)
        (= result
           '((1 1 2)
             (1 1 1)
             (1 1 1 2)
             (1 1 1 1)
             (1 1 1 1 2)
             (1 1 1 1 2)
             (1 1 1 1 2)
             (1 1 1 1 2)
             (1 1 1 1 2)
             (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3))))
      (fn (n m) (fn () (destructive n m)))
      600
      50)))
