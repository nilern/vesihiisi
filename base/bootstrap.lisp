(def not (fn (x) (if x #f #t)))

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

(def meet
  (fn (type1 type2)
    (if (not (identical? type1 <any>))
      (if (not (identical? type2 <any>))
        #f
        type1)
      type2)))

(def has-vararg? (fn ((: method <method>)) (slot-get method 3)))

(def min-arity
  (fn ((: method <method>) has-vararg)
    (let ((arity (flex-count method)))
      (if (not has-vararg)
        arity
        (fx- arity 1)))))

(def meet-domains
  (let ((meet-domains*
         (fn (method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)
           (call-with-current-continuation
             (fn (return)
               (letfn (((meet-domain-tails i)
                          (if (fx< i min-arity1)
                            (let ((param-type1 (flex-get method1 i))
                                  (param-type2 (flex-get method2
                                                         (if (fx< i min-arity2) i min-arity2))))
                              (let ((met-param (meet param-type1 param-type2)))
                                (if (not met-param)
                                  (return #f)
                                  (cons met-param (meet-domain-tails (fx+ i 1))))))
                            (let ((param-type1 (flex-get method1 min-arity1)))
                              (if (fx< i min-arity2)
                                (let ((met-param (meet param-type1 (flex-get method2 i))))
                                  (if (not met-param)
                                    (return #f)
                                    (cons met-param (meet-domain-tails (fx+ i 1)))))
                                (if (if (not has-vararg1) #t (not has-vararg2))
                                  ()
                                  (let ((met-tail (meet param-type1 (flex-get method2 min-arity2))))
                                    (if (not met-tail)
                                      (return #f)
                                      (meet param-type1 (flex-get method2 min-arity2))))))))))
                 (meet-domain-tails 0)))))))
  (fn (f1 f2)
    (let ((method1 (fn-method f1))
          (has-vararg1 (has-vararg? method1))
          (min-arity1 (min-arity method1 has-vararg1))

          (method2 (fn-method f2))
          (has-vararg2 (has-vararg? method2))
          (min-arity2 (min-arity method2 has-vararg2)))
      (if (not has-vararg1)
        (if (not has-vararg2)
          (if (identical? min-arity1 min-arity2)
            (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)
            #f)
          (if (fx< min-arity1 min-arity2)
            #f
            (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)))
        (if (not has-vararg2)
          (if (fx> min-arity1 min-arity2)
            #f
            (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2))
          (meet-domains* method1 min-arity1 has-vararg1 method2 min-arity2 has-vararg2)))))))

(def make-multimethod
  (fn methods
    (let ((methods (array!->array methods))

          (ensure-unambiguous
           (fn (f i)
             (array-fold-left
               (fn (f* j)
                 (if (not (identical? j i))
                   (let ((met-domain (meet-domains f f*)))
                     (if (not met-domain)
                       (fx+ i 1)
                       (error (quote ambiguous-methods) f f* met-domain)))
                   (fx+ i 1)))
               0 methods))))
      (array-fold-left
        (fn (f i)
          (ensure-unambiguous f i)
          (fx+ i 1))
        0 methods)

      (make <multimethod> methods))))
