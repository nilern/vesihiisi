(def not (fn (x) (if x #f #t)))

(def fx> (fn (x y) (fx< y x))) ; OPTIMIZE: separate primop?

(def cons (fn (x xs) (make <pair> x xs)))

(def car (fn ((: xs <pair>)) (slot-get xs 0)))
(def cdr (fn ((: xs <pair>)) (slot-get xs 1)))

(def fold-left
  (fn (f acc xs)
    (if (identical? xs ())
      acc
      (fold-left f (f (car xs) acc) (cdr xs)))))

(def fold-right
  (fn (f xs acc)
    (if (identical? xs ())
      acc
      (f (car xs) (fold-right f (cdr xs) acc)))))

(def map
  (fn (f xs)
    (fold-right (fn (x acc) (cons (f x) acc))
                xs ())))

(def filter
  (fn (pred? xs)
    (fold-right (fn (x acc) (if (pred? x) (cons x acc) acc))
                xs ())))

(def count (fn (xs) (fold-left (fn (x acc) (fx+ 1 acc)) 0 xs)))

(def reverse (fn (xs) (fold-left cons () xs)))

(def drop
  (fn (n xs)
    (if (identical? n 0)
      xs
      (drop (fx- n 1) (cdr xs)))))

(def nth (fn (xs i) (car (drop i xs))))

(def array!-count (fn ((: xs <array!>)) (flex-count xs)))

(def array!-get (fn ((: xs <array!>) i) (flex-get xs i)))

(def array!-fold-left
  (fn (f acc xs)
    (let ((len (array!-count xs)))
      (letfn (((loop acc i)
                 (if (fx< i len)
                   (loop (f (array!-get xs i) acc) (fx+ i 1))
                   acc)))
        (loop acc 0)))))

(def array!-fold-right
  (fn (f acc xs)
    (letfn (((loop acc i)
               (if (fx> i 0)
                 (let ((i (fx- i 1)))
                   (loop (f (array!-get xs i) acc) i))
                 acc)))
      (loop acc (array!-count xs)))))

(def array!->list (fn (arr) (array!-fold-right cons () arr)))

(def array!->array
  (fn (arr)
    (let ((len (array!-count arr))
          (imm-arr (make-flex <array> len)))
      (flex-copy! imm-arr 0 arr 0 len))))

(def error
  (fn (name . irritants)
    (abort (cons name (array!->list irritants))))) ; HACK: Using a list as error

(def fn-method (fn ((: f <fn>)) (slot-get f 0)))

(def has-vararg? (fn ((: method <method>)) (slot-get method 3)))

(def min-arity
  (fn ((: method <method>) has-vararg)
    (let ((arity (flex-count method)))
      (if (not has-vararg)
        arity
        (fx- arity 1)))))

(def meet
  (fn (type1 type2)
    (if (not (identical? type1 <any>))
      (if (not (identical? type2 <any>))
        (if (identical? type1 type2)
          type1
          #f)
        type1)
      type2)))

(def meet-domains
  (let ((meet-domains*
         (fn (method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)
           (call-with-current-continuation
             (fn (return)
               (letfn (((meet-domain-tails i)
                          (if (fx< i min-arity1) ; In fixed params of `method1`
                            (let ((param-type1 (flex-get method1 i))
                                  (param-type2 (flex-get method2
                                                         (if (fx< i min-arity2)
                                                           i              ; Fixed param
                                                           min-arity2)))) ; `method2` vararg
                              (let ((met-param (meet param-type1 param-type2)))
                                (if (not met-param)
                                  (continue return #f)
                                  (cons met-param (meet-domain-tails (fx+ i 1))))))
                            (if (not has-vararg1)
                              ()
                              (let ((param-type1 (flex-get method1 min-arity1)))
                                (if (fx< i min-arity2) ; In fixed params of `method2`
                                  (let ((param-type2 (flex-get method2 i))
                                        (met-param (meet param-type1 param-type2)))
                                    (if (not met-param)
                                      (continue return #f)
                                      (cons met-param (meet-domain-tails (fx+ i 1)))))
                                  (if (not has-vararg2)
                                    ()
                                    (let ((param-type2 (flex-get method2 min-arity2))
                                          (met-tail (meet param-type1 param-type2)))
                                      (if (not met-tail)
                                        (continue return #f)
                                        (meet param-type1 (flex-get method2 min-arity2)))))))))))
                 (meet-domain-tails 0)))))))
    (fn (f1 f2)
      (let ((method1 (fn-method f1))
            (has-vararg1 (has-vararg? method1))
            (min-arity1 (min-arity method1 has-vararg1)) ; OPTIMIZE: not needed on all paths

            (method2 (fn-method f2))
            (has-vararg2 (has-vararg? method2))
            (min-arity2 (min-arity method2 has-vararg2))) ; OPTIMIZE: not needed on all paths
        ;; If arities are compatible call `meet-domains*` to try and produce meet list:
        (if (not has-vararg1)
          (if (not has-vararg2)
            (if (identical? min-arity1 min-arity2)
              (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)
              #f) ; Different fixed arities
            (if (fx< min-arity1 min-arity2)
              #f ; Only `method2` is varargs and always requires more args than `method1`
              (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)))
          (if (not has-vararg2)
            (if (fx> min-arity1 min-arity2)
              #f ; Only `method1` is varargs and always requires more args than `method2`
              (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2))
            (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)))))))

(def make-multimethod
  (fn methods
    (let ((ensure-unambiguous
           (fn (f i)
             (array!-fold-left
               (fn (f* j)
                 (if (not (identical? j i))
                   (let ((met-domain (meet-domains f f*)))
                     (if (not met-domain)
                       (fx+ j 1)
                       (error (quote ambiguous-methods) f f* met-domain)))
                   (fx+ j 1)))
               0 methods))))
      (array!-fold-left
        (fn ((: f <fn>) i)
          (ensure-unambiguous f i)
          (fx+ i 1))
        0 methods)

      (make <multimethod> (array!->array methods)))))

(def +
  (make-multimethod
    (fn () 0) ; Additive identity
    (fn ((: x <fixnum>)) x)
    fx+
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (+ acc v)) (+ (+ x y) z) ns))))

(def -
  (make-multimethod
    (fn ((: x <fixnum>)) (fx- 0 x)) ; OPTIMIZE: `fx-neg`?
    fx-
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (- acc v)) (- (- x y) z) ns))))

(def *
  (make-multimethod
    (fn () 1) ; Multiplicative identity
    (fn ((: x <fixnum>)) x)
    fx*
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (* acc v)) (* (* x y) z) ns))))
