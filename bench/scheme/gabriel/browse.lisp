;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define lookup
  (fn (key table)
    (letfn (((loop x)
               (if (identical? x ())
                 #f
                 (let ((pair (car x)))
                   (if (identical? (car pair) key)
                     pair
                     (loop (cdr x)))))))
      (loop table))))

(define *properties* ())

(define get
  (fn (key1 key2)
    (let ((x (lookup key1 *properties*)))
      (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
            (cdr y)
            #f))
        #f))))

(define put
  (fn (key1 key2 val)
    (let ((x (lookup key1 *properties*)))
      (if x
        (let ((y (lookup key2 (cdr x))))
          (if y
            (set-cdr! y val)
            (set-cdr! x (cons (cons key2 val) (cdr x)))))
        (set! *properties*
              (cons (list key1 (cons key2 val)) *properties*))))))

(define *current-gensym* 0)

(define generate-symbol
  (fn ()
    (set! *current-gensym* (+ *current-gensym* 1))
    (string->symbol (number->string *current-gensym*))))

(define concat-to-tail!
  (fn (x y)
    (if (identical? x ())
      y
      (induct ((a x b)
               (b (cdr x) (cdr b)))
              ((identical? b ())
               (set-cdr! a y)
               x)))))

(define tree-copy
  (fn (x)
    (if (not (isa? <pair> x))
      x
      (cons (tree-copy (car x))
            (tree-copy (cdr x))))))

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(define *rand* 21)

(define init
  (fn (n m npats ipats)
    (let ((ipats (tree-copy ipats)))
      (induct ((p ipats (cdr p)))
              ((identical? (cdr p) ()) (set-cdr! p ipats)))
      (induct ((n n (- n 1))
               (i m (cond ((= i 0) m)
                          (else (- i 1))))
               (name (generate-symbol) (generate-symbol))
               (a (box ())))
              ((= n 0) (box-get a))
        (box-set! a (cons name (box-get a)))
        (induct ((i i (- i 1)))
                ((= i 0))
          (put name (generate-symbol) #f))
        (put name
             'pattern
             (induct ((i npats (- i 1))
                      (ipats ipats (cdr ipats))
                      (a (box ())))
                     ((= i 0) (box-get a))
               (box-set! a (cons (car ipats) (box-get a)))))
        (induct ((j (- m i) (- j 1)))
                ((= j 0))
          (put name (generate-symbol) #f))))))

(define browse-random
  (fn ()
    (set! *rand* (remainder (* *rand* 17) 251))
    *rand*))

(define randomize
  (fn (l)
    (let ((l (box l)))
      (induct ((a (box ())))
              ((identical? (box-get l) ()) (box-get a))
        (let ((n (remainder (browse-random) (count (box-get l)))))
          (cond ((= n 0)
                (box-set! a (cons (car (box-get l)) (box-get a)))
                (box-set! l (cdr (box-get l)))
                (box-get l))
                (else
                (induct ((n n (- n 1))
                         (x (box-get l) (cdr x)))
                        ((= n 1)
                          (box-set! a (cons (cadr x) (box-get a)))
                          (set-cdr! x (cddr x))
                          x)))))))))

(define my-match
  (fn (pat dat alist)
    (cond ((identical? pat ())
           (identical? dat ()))
          ((identical? dat ()) ())
           ((or (identical? (car pat) '?)
                (identical? (car pat)
                     (car dat)))
           (my-match (cdr pat) (cdr dat) alist))
          ((identical? (car pat) '*)
           (or (my-match (cdr pat) dat alist)
               (my-match (cdr pat) (cdr dat) alist)
               (my-match pat (cdr dat) alist)))
          (else (cond ((not (isa? <pair> (car pat)))
                       (cond ((identical? (peek (iter (symbol->string (car pat))))
                                  #"?")
                              (let ((val (assq (car pat) alist)))
                                (cond (val (my-match (cons (cdr val)
                                                          (cdr pat))
                                                    dat alist))
                                      (else (my-match (cdr pat)
                                                      (cdr dat)
                                                      (cons (cons (car pat)
                                                                  (car dat))
                                                            alist))))))
                             ((identical? (peek (iter (symbol->string (car pat))))
                                   #"*")
                              (let ((val (assq (car pat) alist)))
                                (cond (val (my-match (concat (cdr val)
                                                             (cdr pat))
                                                     dat alist))
                                      (else
                                       (induct ((l ()
                                                (concat-to-tail!
                                                 l
                                                 (cons (if (identical? d ())
                                                         ()
                                                         (car d))
                                                      ())))
                                            (e (cons () dat) (cdr e))
                                            (d dat (if (identical? d ()) () (cdr d))))
                                          ((or (identical? e ())
                                               (my-match (cdr pat)
                                                         d
                                                         (cons
                                                          (cons (car pat) l)
                                                          alist)))
                                           (if (identical? e ()) #f #t)))))))

                             ;; fix suggested by Manuel Serrano
                             ;; (cond did not have an else clause);
                             ;; this changes the run time quite a bit

                             (else #f)))
                      (else (and
                             (isa? <pair> (car dat))
                             (my-match (car pat)
                                       (car dat) alist)
                             (my-match (cdr pat)
                                       (cdr dat) alist))))))))

(define database
  (randomize
   (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
                    (a a b b b b a a
                       (a a)(b b))
                    (a a a b (b a) b a b a)))))

(define browse
  (fn (pats)
    (investigate
    database
    pats)
    (map string->number (map symbol->string database))))

(define investigate
  (fn (units pats)
    (induct ((units units (cdr units)))
            ((identical? units ()))
      (induct ((pats pats (cdr pats)))
              ((identical? pats ()))
        (induct ((p (get (car units) 'pattern)
                    (cdr p)))
                ((identical? p ()))
          (my-match (car pats) (car p) ()))))))

(define main
  (fn args
    (run-benchmark
      "browse"
      600
      (fn (result)
        (= result
           '(837 177
              1090
              617
              661
              749
              628
              56
              826
              408
              1035
              474
              320
              452
              672
              991
              155
              122
              793
              221
              716
              727
              848
              309
              144
              936
              100
              881
              287
              430
              23
              771
              232
              804
              958
              650
              1068
              1057
              463
              276
              1046
              1002
              199
              34
              738
              210
              540
              397
              342
              364
              782
              683
              89
              375
              166
              595
              892
              705
              507
              639
              331
              188
              243
              441
              1013
              1079
              67
              298
              386
              573
              859
              133
              760
              12
              529
              815
              111
              496
              45
              265
              925
              903
              254
              78
              551
              606
              485
              518
              419
              870
              562
              1
              353
              980
              694
              914
              969
              947
              584
              1024)))
      (fn (pats) (fn () (browse pats)))
      '((*a ?b *b ?b a *a a *b *a)
        (*a *b *b *a (*a) (*b))
        (? ? * (b a) * ? ?)))))
