(def not (fn (x) (if x #f #t)))

(def cons (fn (x xs) (make <pair> x xs)))

(def car (fn (xs) (field-get <pair> xs 0)))
(def cdr (fn (xs) (field-get <pair> xs 1)))

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
