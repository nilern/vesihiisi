(def isa?
  (fn (t v)
    (if (identical? t <any>)
      #t
      (identical? (type-of v) t))))

(def not (fn (x) (if x #f #t)))

;; OPTIMIZE: separate primops?:
(def fx> (fn (x y) (fx< y x)))
(def fx<= (fn (x y) (not (fx< y x))))
(def fx>= (fn (x y) (not (fx< x y))))

;; OPTIMIZE: separate primops?:
(def char> (fn (x y) (char< y x)))
(def char<= (fn (x y) (not (char< y x))))
(def char>= (fn (x y) (not (char< x y))))

(def symbol->string
  (fn ((: sym <symbol>))
    (let ((len (flex-count sym))
          (str (make-flex <string> len)))
      (flex-copy! str 0 sym 0 len))))

(def cons (fn (x xs) (make <pair> x xs 0.)))

(def car (fn ((: xs <pair>)) (slot-get xs 0)))
(def set-car! (fn ((: xs <pair>) v) (slot-set! xs 0 v)))
(def cdr (fn ((: xs <pair>)) (slot-get xs 1)))
(def set-cdr! (fn ((: xs <pair>) v) (slot-set! xs 1 v)))

(def caar (fn (xs) (car (car xs))))
(def cadr (fn (xs) (car (cdr xs))))
(def cdar (fn (xs) (cdr (car xs))))
(def cddr (fn (xs) (cdr (cdr xs))))

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
  (fn (f xs acc)
    (letfn (((loop acc i)
               (if (fx> i 0)
                 (let ((i (fx- i 1)))
                   (loop (f (array!-get xs i) acc) i))
                 acc)))
      (loop acc (array!-count xs)))))

(def array!->array
  (fn (arr)
    (let ((len (array!-count arr))
          (imm-arr (make-flex <array> len)))
      (flex-copy! imm-arr 0 arr 0 len))))

(def array!->list (fn (arr) (array!-fold-right cons arr ())))

(def list (fn xs (array!->list xs)))

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
  (fn (name . methods) ; FIXME: `((: name <symbol>) . methods)` (breaks due to compiler bug)
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

      (make <multimethod> (array!->array methods) name))))

;; TODO: Support `(apply f x y zs)` (needs to be done in native code to be efficient for
;; `apply-array(!)`:
(def apply
  (make-multimethod 'apply
    apply-array
    apply-array!
    (fn (f (: xs <pair>)) (apply-list f xs))
    (fn (f (: xs <empty-list>)) (f))))

(def +
  (make-multimethod '+
    (fn () 0) ; Additive identity
    (fn ((: x <fixnum>)) x)
    (fn ((: x <flonum>)) x)
    fx+
    fl+
    (fn ((: x <flonum>) (: y <fixnum>)) (fl+ x (fixnum->flonum y)))
    (fn ((: x <fixnum>) (: y <flonum>)) (fl+ (fixnum->flonum x) y))
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (+ acc v)) (+ (+ x y) z) ns))))

(def -
  (make-multimethod '-
    (fn ((: x <fixnum>)) (fx- 0 x)) ; OPTIMIZE: `fx-neg`?
    (fn ((: x <flonum>)) (fl- 0.0 x)) ; OPTIMIZE: `fl-neg`?
    fx-
    fl-
    (fn ((: x <flonum>) (: y <fixnum>)) (fl- x (fixnum->flonum y)))
    (fn ((: x <fixnum>) (: y <flonum>)) (fl- (fixnum->flonum x) y))
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (- acc v)) (- (- x y) z) ns))))

(def *
  (make-multimethod '*
    (fn () 1) ; Multiplicative identity
    (fn ((: x <fixnum>)) x)
    (fn ((: x <flonum>)) x)
    fx*
    fl*
    (fn ((: x <flonum>) (: y <fixnum>)) (fl* x (fixnum->flonum y)))
    (fn ((: x <fixnum>) (: y <flonum>)) (fl* (fixnum->flonum x) y))
    (fn (x y z . ns) (array!-fold-left (fn (v acc) (* acc v)) (* (* x y) z) ns))))

(def /
  (make-multimethod '/
    (fn ((: x <flonum>)) (fl/ 1.0 x))
    fl/))

(def <
  (make-multimethod '<
    (fn ((: x <fixnum>) (: y <fixnum>)) (fx< x y))
    (fn ((: x <char>) (: y <char>)) (char< x y))))

(def >
  (make-multimethod '>
    (fn ((: x <fixnum>) (: y <fixnum>)) (fx> x y))
    (fn ((: x <char>) (: y <char>)) (char> x y))))

(def <=
  (make-multimethod '<=
    (fn ((: x <fixnum>) (: y <fixnum>)) (fx<= x y))
    (fn ((: x <char>) (: y <char>)) (char<= x y))))

(def >=
  (make-multimethod '>=
    (fn ((: x <fixnum>) (: y <fixnum>)) (fx>= x y))
    (fn ((: x <char>) (: y <char>)) (char>= x y))))

(def > (fn (x y) (< y x)))

(def inexact
  (make-multimethod 'inexact
    (fn ((: n <fixnum>)) (fixnum->flonum n))
    (fn ((: n <flonum>)) n)))

(def concat
  (make-multimethod 'concat
    (fn () ())
    (fn (xs) xs)
    (fn (xs ys) (fold-right cons xs ys))
    (letfn (((concat-nonempty-array! xss)
               (let ((last-idx (fx- (array!-count xss) 1)))
                 (letfn (((loop i acc)
                            (if (fx> i 0)
                              (let ((i (fx- i 1)))
                                (loop i (concat (array!-get xss i) acc)))
                              acc)))
                   (loop last-idx (array!-get xss last-idx))))))
      (fn (xs ys zs . xss)
        (if (identical? (array!-count xss) 0)
          (concat xs (concat ys zs))
          (concat xs (concat ys (concat zs (concat-nonempty-array! xss)))))))))

(def =-impl
  (make-multimethod '=-impl
    (fn ((: x <fixnum>) (: y <fixnum>)) (identical? x y))
    (fn ((: x <char>) (: y <char>)) (identical? x y))

    (fn ((: x <empty-list>) (: y <empty-list>)) #t)
    (fn ((: x <pair>) (: y <pair>))
      (if (= (car x) (car y))
        (= (cdr x) (cdr y))
        #f))))

(def =
  (fn (x y)
    (if (identical? x y)
      #t
      (if (identical? (type-of x) (type-of y))
        (=-impl x y)
        #f))))

(def <string-builder> (make-slots-type '<string-builder> 2 #f))
(def string-builder (fn () (make <string-builder> (make-flex <array!> 2) 0)))

(def string-builder-push!
   (fn ((: builder <string-builder>) v)
     (let ((vs (slot-get builder 0))
           (len (slot-get builder 1))
           (cap (array!-count vs))
           (vs (if (= len cap)
                 (let ((cap* (+ cap (fx-quot cap 2)))
                       (vs* (make-flex <array!> cap*)))
                   (flex-copy! vs* 0 vs 0 len)
                   (slot-set! builder 0 vs*))
                 vs)))
       (flex-set! vs len v)
       (slot-set! builder 1 (+ len 1))
       v)))

(def build-string
  (fn ((: builder <string-builder>))
    (let ((cs (flex-copy (slot-get builder 0) 0 (slot-get builder 1))))
      (array!->string cs))))

(def push!
  (make-multimethod 'push!
    string-builder-push!))

(def newline (fn () (write-char #"\n")))

(def <box> (make-slots-type (quote <box>) 1 #f))
(def box (fn (v) (make <box> v)))
(def box-get (fn (bx) (slot-get bx 0)))
(def box-set! (fn (bx v) (slot-set! bx 0 v)))

(def next!
  (make-multimethod 'next!
    string-iterator-next!
    read-char))

(def peek
  (make-multimethod 'peek
    string-iterator-peek
    peek-char))

(def iter-fold!
  (fn (f acc it)
    (letfn (((loop acc)
               (let ((miv (next! it)))
                 (if (not (identical? miv end))
                   (loop (f miv acc))
                   acc))))
      (loop acc))))

(def string-fold (fn (f acc (: s <string>)) (iter-fold! f acc (make <string-iterator> s 0))))

(def fold
  (make-multimethod 'fold
    string-fold))

(def index-of
  (fn (vs v)
    (call-with-current-continuation
      (fn (return)
        (fold (fn (itv i)
                (if (= itv v)
                  (continue return i)
                  (fx+ i 1)))
              0 vs)
        #f))))

(def source-location-filename (fn ((: loc <source-location>)) (slot-get loc 0)))
(def source-location-byte-index (fn ((: loc <source-location>)) (slot-get loc 1)))

;;; Self-Hosting Reader
;;; ================================================================================================

(def build-symbol (fn (builder) (string->symbol (build-string builder))))

;;; Let over Lambda to the max!:

;;; TODO: Fix indentation when code stabilizes:
;;; First, an embedded LL(1) parser combinator library:
(let ((<parser> (make-slots-type (quote <parser>) 3 #f))
        (parser-parse (fn ((: p <parser>)) (slot-get p 0)))
        (parser-look-ahead (fn ((: p <parser>)) (slot-get p 1)))
        (parser-nullable (fn ((: p <parser>)) (slot-get p 2)))

        (parse (make-multimethod 'parse
                 (fn ((: p <parser>) input byte-idx) ((parser-parse p) input byte-idx))

                 (fn ((: p-box <box>) input byte-idx)
                   ((parser-parse (box-get p-box)) input byte-idx))

                 (fn ((: c <char>) input byte-idx)
                   (let ((ic (next! input)))
                     (if (= ic c)
                       (let ()
                         (box-set! byte-idx (+ (box-get byte-idx) 1))
                         ic)
                       (error (quote read-error) ic c))))))

        (look-ahead? (make-multimethod 'look-ahead?
                       (fn ((: p <parser>) c) ((parser-look-ahead p) c))
                       (fn ((: p-box <box>) c) ((parser-look-ahead (box-get p-box)) c))
                       (fn ((: c <char>) ic) (= ic c))))

        (nullable? (make-multimethod 'nullable?
                     parser-nullable
                     (fn ((: p-box <box>)) (parser-nullable (box-get p-box)))
                     (fn ((: c <char>)) #f)))

        (sat (fn (acceptable? description)
               (make <parser> (fn (input byte-idx)
                                (let ((c (next! input)))
                                  (if (acceptable? c)
                                    (let ()
                                      (box-set! byte-idx (+ (box-get byte-idx) 1))
                                      c)
                                    (error (quote read-error) c description))))
                              acceptable?
                              #f)))

        (epsilon (make <parser> (fn (input byte-idx) #f)
                                (fn (c) #f) ; Theoretically off but makes this work in e.g. `seq->`
                                #t))

        (parser-placeholder (fn (nullable)
                              (make <parser>
                                    (fn (input byte-idx)
                                      (error (quote uninitialized-parser) input byte-idx))
                                    (fn (c) (error (quote uninitialized-parser) c))
                                    nullable)))

        ;; Could use varargs but the reader is so fundamental that hand-unrolling is warranted:
        ;; p q etc.
        (seq-> (make-multimethod 'seq->
                 (fn (p f)
                   (make <parser> (fn (input byte-idx) (f (parse p input byte-idx)))
                                  (fn (c) (look-ahead? p c))
                                  (nullable? p)))

                 (fn (p q f)
                   (make <parser> (fn (input byte-idx)
                                    (let ((x (parse p input byte-idx))
                                          (y (parse q input byte-idx)))
                                      (f x y)))
                                  (fn (c)
                                    (if (look-ahead? p c)
                                      #t
                                      (if (nullable? p)
                                        (look-ahead? q c)
                                        #f)))
                                  (if (nullable? p) (nullable? q) #f)))

                 (fn (p q r f)
                   (make <parser> (fn (input byte-idx)
                                    (let ((x (parse p input byte-idx))
                                          (y (parse q input byte-idx))
                                          (z (parse r input byte-idx)))
                                      (f x y z)))
                                  (fn (c)
                                    (if (look-ahead? p c)
                                      #t
                                      (if (nullable? p)
                                        (if (look-ahead? q c)
                                          #t
                                          (if (nullable? q)
                                            (look-ahead? r c)
                                            #f))
                                        #f)))
                                  (if (nullable? p)
                                    (if (nullable? q)
                                      (nullable? r)
                                      #f)
                                    #f)))))

        ;; Could use varargs but the reader is so fundamental that hand-unrolling is warranted:
        ;; Does not support nullable alternatives but we do not need them:
        ;; p | q etc.
        (alt (make-multimethod 'alt
               (fn (expected p q)
                 (make <parser> (fn (input byte-idx)
                                  (let ((c (peek input)))
                                    (if (look-ahead? p c)
                                      (parse p input byte-idx)
                                      (if (look-ahead? q c)
                                        (parse q input byte-idx)
                                        (error 'read-error c expected)))))
                                (fn (c) (if (look-ahead? p c) #t (look-ahead? q c)))
                                (if (nullable? p) (nullable? q) #f)))
               (fn (expected p q r)
                 (make <parser> (fn (input byte-idx)
                                  (let ((c (peek input)))
                                    (if (look-ahead? p c)
                                      (parse p input byte-idx)
                                      (if (look-ahead? q c)
                                        (parse q input byte-idx)
                                        (if (look-ahead? r c)
                                          (parse r input byte-idx)
                                          (error 'read-error c expected))))))
                                (fn (c)
                                  (if (look-ahead? p c)
                                    #t
                                    (if (look-ahead? q c)
                                      #t
                                      (look-ahead? r c))))
                                (if (nullable? p)
                                  (if (nullable? q)
                                    (nullable? r)
                                    #f)
                                  #f)))
               (fn (expected p q r s)
                 (make <parser> (fn (input byte-idx)
                                  (let ((c (peek input)))
                                    (if (look-ahead? p c)
                                      (parse p input byte-idx)
                                      (if (look-ahead? q c)
                                        (parse q input byte-idx)
                                        (if (look-ahead? r c)
                                          (parse r input byte-idx)
                                          (if (look-ahead? s c)
                                            (parse s input byte-idx)
                                            (error 'read-error c expected)))))))
                                 (fn (c)
                                   (if (look-ahead? p c)
                                     #t
                                     (if (look-ahead? q c)
                                       #t
                                       (if (look-ahead? r c)
                                         #t
                                         (look-ahead? s c)))))
                                 (if (nullable? p)
                                   (if (nullable? q)
                                     (if (nullable? r)
                                       (nullable? s)
                                       #f)
                                     #f)
                                   #f)))
                (fn (expected p q r s t)
                  (make <parser> (fn (input byte-idx)
                                   (let ((c (peek input)))
                                     (if (look-ahead? p c)
                                       (parse p input byte-idx)
                                       (if (look-ahead? q c)
                                         (parse q input byte-idx)
                                         (if (look-ahead? r c)
                                           (parse r input byte-idx)
                                           (if (look-ahead? s c)
                                             (parse s input byte-idx)
                                             (if (look-ahead? t c)
                                               (parse t input byte-idx)
                                               (error 'read-error c expected))))))))
                                 (fn (c)
                                   (if (look-ahead? p c)
                                     #t
                                     (if (look-ahead? q c)
                                       #t
                                       (if (look-ahead? r c)
                                         #t
                                         (if (look-ahead? s c)
                                           #t
                                           (look-ahead? t c))))))
                                 (if (nullable? p)
                                   (if (nullable? q)
                                     (if (nullable? r)
                                       (if (nullable? s)
                                         (nullable? t)
                                         #f)
                                       #f)
                                     #f)
                                   #f)))
                (fn (expected p q r s t u)
                  (make <parser> (fn (input byte-idx)
                                   (let ((c (peek input)))
                                     (if (look-ahead? p c)
                                       (parse p input byte-idx)
                                       (if (look-ahead? q c)
                                         (parse q input byte-idx)
                                         (if (look-ahead? r c)
                                           (parse r input byte-idx)
                                           (if (look-ahead? s c)
                                             (parse s input byte-idx)
                                             (if (look-ahead? t c)
                                               (parse t input byte-idx)
                                               (if (look-ahead? u c)
                                                 (parse u input byte-idx)
                                                 (error 'read-error c expected)))))))))
                                 (fn (c)
                                   (if (look-ahead? p c)
                                     #t
                                     (if (look-ahead? q c)
                                       #t
                                       (if (look-ahead? r c)
                                         #t
                                         (if (look-ahead? s c)
                                           #t
                                           (if (look-ahead? t c)
                                             #t
                                             (look-ahead? u c)))))))
                                 (if (nullable? p)
                                   (if (nullable? q)
                                     (if (nullable? r)
                                       (if (nullable? s)
                                         (if (nullable? t)
                                           (nullable? u)
                                           #f)
                                         #f)
                                       #f)
                                     #f)
                                   #f)))))

        ;; Would not work for a bool-valued parser but we do not need that:
        (opt (fn (p)
               (make <parser> (fn (input byte-idx)
                                (if (look-ahead? p (peek input))
                                  (parse p input byte-idx)
                                  #f))
                              (fn (c) (look-ahead? p c))
                              #t)))

        ;; accp p*
        (mfoldl (fn (f accp p)
                  (make <parser> (fn (input byte-idx)
                                   (let ((acc (parse accp input byte-idx)))
                                     (letfn (((parse-ps acc)
                                               (if (look-ahead? p (peek input))
                                                 (let ((v (parse p input byte-idx)))
                                                   (parse-ps (f v acc)))
                                                 acc)))
                                       (parse-ps acc))))
                                 (fn (c)
                                   (if (look-ahead? accp c)
                                     #t
                                     (if (nullable? accp)
                                       (look-ahead? p c)
                                       #f)))
                                 (nullable? accp))))

        ;;; Now for the actual concrete S-expression parsing:

        (expr-box (box (parser-placeholder #f)))

        ;; FIXME:
        ;; \s+|;[^\n]*(\n|$)
        (ws-seg (let ((whitespace-char (sat char-whitespace? "whitespace char (\\s)"))
                      (online (sat (fn (c) (not (= c #"\n"))) "not newline (#\"\\n\")")))
                   (alt "\\s|;"
                        (mfoldl (fn (_ acc) acc)
                                (seq-> whitespace-char (fn (_) #f))
                                whitespace-char)
                        (seq-> #";"
                               (mfoldl (fn (_ acc) acc) epsilon online)
                               (alt "\n|$" #"\n" end)
                               (fn (_ __ ___) #f)))))
        ;; ws-seg*
        (ws (mfoldl (fn (_ acc) acc) epsilon ws-seg))

        (ws-expr-ws (seq-> ws expr-box ws (fn (_ v __) v)))

        ;; ')'
        (proper-list-terminator (seq-> #")" (fn (_) ())))
        ;; TODO: Source locations
        ;; (ws expr ws)+ (')' | '.' ws expr ws ')')
        (non-empty-list-tail (seq-> (mfoldl (fn (v acc)
                                              (let ((pair (cons v #f)))
                                                (set-cdr! (cdr acc) pair)
                                                (set-cdr! acc pair))
                                              acc)
                                            (seq-> ws-expr-ws
                                                   (fn (v)
                                                     (let ((first-pair (cons v #f)))
                                                       (cons first-pair first-pair))))
                                            ws-expr-ws)
                                    (alt "[).]"
                                         proper-list-terminator
                                         (seq-> #"." ws-expr-ws #")" (fn (_ v __) v)))
                                    (fn (acc tail) (set-cdr! (cdr acc) tail) (car acc))))

        ;; '(' ws (non-empty-list-tail proper-list-terminator)
        (list (seq-> #"("
                     ws
                     (alt "S-expr or #\")\""
                          non-empty-list-tail
                          proper-list-terminator)
                     (fn (_ __ ls) ls)))

        (initial? (fn (c)
                    (if (isa? <char> c)
                      (if (char-alphabetic? c)
                        #t
                        (index-of "_:!?+-*/=<>&|" c))
                      #f)))
        (initial (sat initial? "symbol initial ([\\p{L}_:!?+\\-*/=<>&|])"))
        (subsequent? (fn (c)
                       (if (initial? c)
                         #t
                         (if (isa? <char> c)
                           (char-numeric? c)
                           #f))))
        (subsequent (sat subsequent? "symbol subsequent ([\\p{L}_:!?+\\-*/=<>]\\p{Nd})"))
        ;; initial subsequent*
        (symbol (seq-> (mfoldl (fn (c builder) (push! builder c) builder)
                               (seq-> initial
                                      (fn (c)
                                        (let ((builder (string-builder)))
                                          (push! builder c)
                                          builder)))
                               subsequent)
                       (fn (builder) (build-symbol builder))))

        ;; [^"\\] | '\\' [abtnr\\]
        (normal-string-char? (fn (c) (if (= c #"\"") #f (not (= c #"\\")))))
        (string-char (alt "[^\"]"
                          (sat normal-string-char? "normal string char ([^\"\\\\])")
                          (seq-> #"\\"
                                 (alt "[abtnr\\\\]"
                                      (seq-> #"a" (fn (_) #"\a"))
                                      (seq-> #"b" (fn (_) #"\b"))
                                      (seq-> #"t" (fn (_) #"\t"))
                                      (seq-> #"n" (fn (_) #"\n"))
                                      (seq-> #"r" (fn (_) #"\r"))
                                      (seq-> #"\\" (fn (_) #"\\")))
                                      ;; TODO: \"
                                 (fn (_ c) c))))

        ;; '"' string-char* '"'
        (string (seq-> (mfoldl (fn (c builder) (push! builder c) builder)
                               (seq-> #"\"" (fn (_) (string-builder)))
                               string-char)
                       #"\""
                       (fn (builder _) (build-string builder))))

        (decimal-digit? (fn (c) (if (isa? <char> c) (if (>= c #"0") (<= c #"9") #f) #f)))
        (decimal-digit-value (fn (c)
                               (if (isa? <char> c)
                                 (if (>= c #"0")
                                   (if (<= c #"9")
                                     (- (char->integer c) (char->integer #"0"))
                                     #f)
                                   #f)
                                 #f)))
        (hex-digit? (fn (c) ;; Yuck, but we haven't bootstrapped `and` and `or` yet:
                      (if (decimal-digit? c)
                        #t
                        (if (isa? <char> c)
                          (if (>= c #"A")
                            (if (<= c #"F")
                              #t
                              (if (>= c #"a")
                                (if (<= c #"f")
                                  #t
                                  #f)
                                #f))
                            #f)
                          #f))))
        (hex-digit-value (fn (c)
                           (let ((d (decimal-digit-value c)))
                             (if d
                               d
                               (if (isa? <char> c)
                                 (if (>= c #"A")
                                   (if (<= c #"F")
                                     (- (char->integer c) (char->integer #"A"))
                                     (if (>= c #"a")
                                       (if (<= c #"f")
                                         (- (char->integer c) (char->integer #"a"))
                                         #f)
                                       #f))
                                   #f)
                                 #f)))))
        (digit-in (fn (radix)
                    (let ((acceptable? (if (= radix 10)
                                         decimal-digit?
                                         (if (= radix 16)
                                           hex-digit?
                                           (error (quote unimplemented-radix) radix))))
                          (description (if (= radix 10)
                                         "decimal digit ([0-9])"
                                         (if (= radix 16)
                                           "hexadecimal digit ([0-9A-Fa-f])"
                                           (error (quote unimplemented-radix) radix))))
                          (digit-value (if (= radix 10)
                                         decimal-digit-value
                                         (if (= radix 16)
                                           hex-digit-value
                                           (error (quote unimplemented-radix) radix)))))
                      (seq-> (sat acceptable? description)
                             (fn (c) (if c (digit-value c) c))))))
        ;; TODO: Improve flonum precision:
        ;; <digit radix>+ ('.' <digit radix>*)?
        (number (fn (radix)
                  (let ((fl-radix (inexact radix))
                        (digit (digit-in radix)))
                    (seq-> (mfoldl (fn (d n) (+ (* radix n) d)) digit digit)
                           (opt (mfoldl (fn (d acc)
                                          (let ((c-frac (/ (inexact d) (cdr acc))))
                                            ;; OPTIMIZE: `set-c(a|d)r!` instead of `cons`:
                                            (cons (+ (car acc) c-frac)
                                                  (* (cdr acc) fl-radix))))
                                        (seq-> #"." (fn (_) (cons 0. fl-radix)))
                                        digit))
                           (fn (integral acc)
                             (if (not acc)
                               integral
                               (+ (inexact integral) (car acc))))))))
        (decimal-number (number 10))
        (hex-number (number 16))

        ;; '#' ('t' | 'f' | '"' string-char '"' | 'x' hex-number)
        (crunchy (seq-> #"#"
                        (alt "[tf\\\\x]"
                             (seq-> #"t" (fn (_) #t))
                             (seq-> #"f" (fn (_) #f))
                             (seq-> #"\"" string-char #"\"" (fn (_ c __) c))
                             (seq-> #"x" hex-number (fn (_ n) n)))
                        (fn (_ v) v)))

        (expr (alt "S-expr" list symbol string decimal-number crunchy))

        (ws-expr (seq-> ws expr (fn (_ v) v))))
    (box-set! expr-box expr)

  (def do-read*
    (fn (input loc)
      (let ((byte-idx (box (source-location-byte-index loc)))
            (v (parse ws-expr input byte-idx)))
        (cons v (make <source-location> (source-location-filename loc) (box-get byte-idx))))))

  (def skip-whitespace
    (fn (input)
      (let ((byte-idx (box 0)))
        (parse ws input byte-idx)
        (box-get byte-idx)))))

;; FIXME: Need to provide start location as well as the end:
(def read*
  (make-multimethod 'read*
    (fn (input (: loc <source-location>)) (do-read* input loc))
    (fn (input (: filename <string>)) (read* input (make <source-location> filename 0)))))

(def read
  (make-multimethod 'read
    (fn (input) (car (read* input "???")))
    (fn () (read standard-input))))

;; FIXME: Provide start location:
(def read*-all
  (make-multimethod 'read*-all
    (fn (input (: loc <source-location>))
      (let ((ws-count (skip-whitespace input)))
        (if (not (= (peek input) end))
          (let ((filename (source-location-filename loc))
                (byte-idx (+ (source-location-byte-index loc) ws-count))
                (v&loc (read* input (make <source-location> filename byte-idx)))
                (pair (cons (car v&loc) ()))
                (sexprs pair)
                (byte-idx (source-location-byte-index (cdr v&loc))))
            (letfn (((read*-remaining pair byte-idx)
                       (if (not (= (peek input) end))
                         (let ((v&loc (read* input (make <source-location> filename byte-idx)))
                               (pair* (cons  (car v&loc) ()))
                               (_ (set-cdr! pair pair*))
                               (byte-idx (source-location-byte-index (cdr v&loc)))
                               (ws-count (skip-whitespace input)))
                           (read*-remaining pair* (+ byte-idx ws-count)))
                         sexprs)))
             (read*-remaining pair byte-idx)))
          ())))))
