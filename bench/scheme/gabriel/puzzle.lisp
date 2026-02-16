;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(define my-iota
  (fn (n)
    (induct ((n n (- n 1))
             (list () (cons (- n 1) list)))
            ((= n 0) list))))

(define *size* 511)
(define classmax 3)
(define typemax 12)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-array! (+ classmax 1) 0))
(define *class* (make-array! (+ typemax 1) 0))
(define *piecemax* (make-array! (+ typemax 1) 0))
(define *puzzle* (make-array! (+ *size* 1)))
(define *p* (make-array! (+ typemax 1)))

(define fit
  (fn (i j)
    (let ((end (array!-get *piecemax* i)))
      (induct ((k 0 (+ k 1)))
              ((or (> k end)
                   (and (array!-get (array!-get *p* i) k)
                        (array!-get *puzzle* (+ j k))))
        (if (> k end) #t #f))))))

(define place
  (fn (i j)
    (let ((end (array!-get *piecemax* i)))
      (induct ((k 0 (+ k 1)))
              ((> k end))
        (cond ((array!-get (array!-get *p* i) k)
               (array!-set! *puzzle* (+ j k) #t)
               #t)))
      (array!-set! *piececount*
                   (array!-get *class* i)
                   (- (array!-get *piececount* (array!-get *class* i)) 1))
      (induct ((k j (+ k 1)))
              ((or (> k *size*) (not (array!-get *puzzle* k)))
               (if (> k *size*) 0 k))))))

(define puzzle-remove
  (fn (i j)
    (let ((end (array!-get *piecemax* i)))
      (induct ((k 0 (+ k 1)))
              ((> k end))
        (cond ((array!-get (array!-get *p* i) k)
               (array!-set! *puzzle* (+ j k) #f)
              #f)))
      (array!-set! *piececount*
                  (array!-get *class* i)
                  (+ (array!-get *piececount* (array!-get *class* i)) 1)))))

(define trial
  (fn (j)
    (let ((k (box 0)))
      (call-with-current-continuation
        (fn (return)
          (induct ((i 0 (+ i 1)))
                  ((> i typemax) (set! *kount* (+ *kount* 1)) #f)
            (cond
              ((not
                (= (array!-get *piececount* (array!-get *class* i)) 0))
               (cond
                 ((fit i j)
                  (box-set! k (place i j))
                 (cond
                   ((or (trial (box-get k)) (= (box-get k) 0))
                    (set! *kount* (+ *kount* 1))
                    (continue return #t))
                   (else (puzzle-remove i j)))))))))))))

(define definePiece
  (fn (iclass ii jj kk)
    (let ((index (box 0)))
      (induct ((i 0 (+ i 1)))
              ((> i ii))
        (induct ((j 0 (+ j 1)))
                ((> j jj))
          (induct ((k 0 (+ k 1)))
                  ((> k kk))
            (box-set! index (+ i (* *d* (+ j (* *d* k)))))
            (array!-set! (array!-get *p* *iii*) (box-get index) #t))))
      (array!-set! *class* *iii* iclass)
      (array!-set! *piecemax* *iii* (box-get index))
      (cond ((not (= *iii* typemax))
             (set! *iii* (+ *iii* 1)))))))

(define start
  (fn (size)
    (set! *kount* 0)
    (induct ((m 0 (+ m 1)))
            ((> m size))
      (array!-set! *puzzle* m #t))
    (induct ((i 1 (+ i 1)))
            ((> i 5))
      (induct ((j 1 (+ j 1)))
              ((> j 5))
        (induct ((k 1 (+ k 1)))
                ((> k 5))
          (array!-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f))))
    (induct ((i 0 (+ i 1)))
            ((> i typemax))
      (induct ((m 0 (+ m 1)))
              ((> m size))
        (array!-set! (array!-get *p* i) m #f)))
    (set! *iii* 0)
    (definePiece 0 3 1 0)
    (definePiece 0 1 0 3)
    (definePiece 0 0 3 1)
    (definePiece 0 1 3 0)
    (definePiece 0 3 0 1)
    (definePiece 0 0 1 3)

    (definePiece 1 2 0 0)
    (definePiece 1 0 2 0)
    (definePiece 1 0 0 2)

    (definePiece 2 1 1 0)
    (definePiece 2 1 0 1)
    (definePiece 2 0 1 1)

    (definePiece 3 1 1 1)

    (array!-set! *piececount* 0 13)
    (array!-set! *piececount* 1 3)
    (array!-set! *piececount* 2 1)
    (array!-set! *piececount* 3 1)

    (let ((m (+ (* *d* (+ *d* 1)) 1))
          (n (box 0)))
      (cond ((fit 0 m) (box-set! n (place 0 m)))
            (else (do (newline) (display "Error."))))
      (if (trial (box-get n))
        *kount*
        #f))))

(for-each (fn (i) (array!-set! *p* i (make-array! (+ *size* 1))))
          (my-iota (+ typemax 1)))

(define main
  (fn args
    (run-benchmark
      "puzzle"
      100
      (fn (result) (= result 2005))
      (fn () (fn () (start *size*))))))
