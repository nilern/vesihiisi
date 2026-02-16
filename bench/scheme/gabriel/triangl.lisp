;;; TRIANGL -- Board game benchmark.

(define *board*
  (list->array! '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->array! '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->array! '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->array! '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->array! '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* ())

(define attempt
  (fn (i depth)
    (cond ((= depth 14)
           (set! *answer*
                 (cons (cdr (array!->list *sequence*)) *answer*))
           #t)
          ((and (= 1 (array!-get *board* (array!-get *a* i)))
                (= 1 (array!-get *board* (array!-get *b* i)))
                (= 0 (array!-get *board* (array!-get *c* i))))
           (array!-set! *board* (array!-get *a* i) 0)
           (array!-set! *board* (array!-get *b* i) 0)
           (array!-set! *board* (array!-get *c* i) 1)
           (array!-set! *sequence* depth i)
           (induct ((j 0 (+ j 1))
                    (depth (+ depth 1)))
                   ((or (= j 36) (attempt j depth)) #f))
           (array!-set! *board* (array!-get *a* i) 1)
           (array!-set! *board* (array!-get *b* i) 1)
           (array!-set! *board* (array!-get *c* i) 0) #f)
          (else #f))))

(define test
  (fn (i depth)
    (set! *answer* ())
    (attempt i depth)
    (car *answer*)))

(define main
  (fn args
    (run-benchmark
      "triangl"
      10
      (fn (result) (= result '(22 34 31 15 7 1 20 17 25 6 5 13 32)))
      (fn (i depth) (fn () (test i depth)))
      22
      1)))
