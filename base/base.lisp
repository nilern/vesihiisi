;;; Macros for Defining Macros

(define define-macro
  (fn (_ form)
    (let ((interface (cadr form))
          (name (car interface))
          (params (cdr interface))
          (body (cddr form)))
      ;; TODO: Use the quasiquote reader macros when they become available:
      `(let ()
         (define ,name  (fn ,params ,@body))
         (var-set-macro-category! (resolve (quote ,name)) 'fn-macro)))))
(var-set-macro-category! (resolve 'define-macro) 'fn-macro)

(define-macro (define-symbol-macro _ form)
  (let ((interface (cadr form))
        (name (car interface))
        (params (cdr interface))
        (body (cddr form)))
    ;; TODO: Use the quasiquote reader macros when they become available:
    `(let ()
       (define ,name (fn ,params ,@body))
       (var-set-macro-category! (resolve (quote ,name)) 'symbol-macro))))

;;; Utility Macros
;;; ================================================================================================

(define-macro (do _ form)
  (let ((stmts (cdr form)))
    `(let () ,@stmts)))

(define-macro (when _ form)
  (let ((condition (cadr form))
        (body (cddr form)))
    `(if ,condition (do ,@body) #f)))

(define-macro (cond _ form)
  (let ((cases (cdr form)))
    (if (identical? cases ())
      #f
      (let ((case (car cases)))
        (if (identical? (car case) 'else)
          `(do ,@(cdr case))
          `(if ,(car case)
             (do ,@(cdr case))
             (cond ,@(cdr cases))))))))

(define-macro (and _ form)
  (let ((args (cdr form)))
    (if (identical? args ())
      #t
      (let ((tmp-val (car args))
            (args (cdr args)))
        (if (identical? args ())
          tmp-val
          `(if ,tmp-val
             (and ,@args)
             #f))))))

(define-macro (or _ form)
  (let ((args (cdr form)))
    (if (identical? args ())
      #f
      (let ((tmp-val (car args))
            (args (cdr args)))
        (if (identical? args ())
          tmp-val
          (let ((tmp (gensym)))
            `(let ((,tmp ,tmp-val))
               (if ,tmp
                 ,tmp
                 (or ,@args)))))))))

(define-macro (induct _ form)
  (let ((args (cdr form))
        (inductions (cadr form))
        (ctrl (caddr form))
        (body (cdddr form))

        (inductions (map (fn (induction)
                           (if (= (count induction) 2)
                             `(,@induction ,(car induction))
                             induction))
                         inductions))
        (loop (gensym)))
    `(letfn (((,loop ,@(map car inductions))
               (if ,(car ctrl)
                 (do ,@(cdr ctrl))
                 (do ,@body
                     (,loop ,@(map caddr inductions))))))
       (,loop ,@(map cadr inductions)))))

;;; Scalars
;;; ================================================================================================

(define fixnum-width
  ;; Thunkify:
  (let ((width fixnum-width))
    (fn () width)))

(define least-fixnum
  ;; Thunkify:
  (let ((min least-fixnum))
    (fn () min)))

(define greatest-fixnum
  ;; Thunkify:
  (let ((max greatest-fixnum))
    (fn () max)))

(define quotient
  (make-multimethod 'quotient
    fx-quot))

(define remainder
  (make-multimethod 'remainder
    fx-rem))

(define expt
  (make-multimethod 'expt
    (fn ((: base <fixnum>) (: power <fixnum>))
      (when (fx< power 0) (error 'TODO))
      (induct ((n 1 (fx* n base))
               (power power (fx- power 1)))
              ((identical? power 0) n)))))

;; FIXME: Negative inputs result in garbage outputs:
(define fx-ilog2 (fn (n) (fx- (fx- (fixnum-width) 1) (fx-nlz n))))

;; FIXME: Negative inputs result in garbage outputs:
(define fx-ilog10
  ;; Hacker's Delight Figure 11-13:
  (let ((maxes (array 0 9 99 999 9999
                      99999 999999 9999999 99999999 999999999
                      9999999999 99999999999 999999999999 9999999999999 99999999999999)))
    (fn (n)
      (let ((approx (fx>> (fx* 19 (fx-ilog2 n)) 6)) ; 19 / 64 * ilog2(n)
            (correction (fx>>> (fx- (array-get maxes (fx+ approx 1)) n)
                               (fx- (fixnum-width) 1)))) ; [0, 1)
        (fx+ approx correction)))))

(define fixnum->string
  (fn ((: n <fixnum>))
    (if (identical? n 0)
      "0"
      (if (identical? n (least-fixnum))
        "-140737488355328"
        (let ((a (fx-abs n))
              (max-divisor-power (fx-ilog10 a))
              (max-divisor (expt 10 max-divisor-power))
              (len (if (fx< n 0) (fx+ max-divisor-power 2) (fx+ max-divisor-power 1)))
              (str (make-flex <string> len)))
          (letfn (((loop i n divisor)
                    (if (fx< i len)
                      (do (flex-u8-set! str i (fx+ 48 (fx-quot n divisor))) ; #"0" = 48
                          (loop (fx+ i 1) (fx-rem n divisor) (fx-quot divisor 10)))
                      str)))
            (if (>= n 0)
              (loop 0 a max-divisor)
              (do (flex-u8-set! str 0 (char->integer #"-"))
                  (loop 1 a max-divisor)))))))))

;; TODO: Make extensible:
(define number? (fn (v) (or (isa? <fixnum> v) (isa? <flonum> v))))

(define number->string
  (make-multimethod 'number->string
    fixnum->string))

;; TODO: Sign, non-decimal radices, flonums:
(define string->number
  (fn (s)
    (let ((it (string-iterator s))
          (?c (string-iterator-next! it)))
      ;; TODO: DRY wrt. following loop:
      (if (identical? ?c end)
        #f
        (if (and (>= ?c #"0") (<= ?c #"9"))
          (letfn (((loop n)
                    (let ((?c (string-iterator-next! it)))
                      (if (identical? ?c end)
                        n
                        (if (and (>= ?c #"0") (<= ?c #"9"))
                          (loop (+ (* n 10) (fx- (char->integer ?c) (char->integer #"0"))))
                          #f)))))
            (loop (fx- (char->integer ?c) (char->integer #"0"))))
          #f)))))

;;; Collections
;;; ================================================================================================

(define for-each (fn (f vs) (fold (fn (v _) (f v)) vs vs)))

(define make-array!
  (make-multimethod 'make-array!
    (fn (len) (make-flex <array!> len))
    (fn (len v)
      (let ((arr (make-array! len)))
        (induct ((i 0 (fx+ i 1)))
                ((identical? i len) arr)
          (array!-set! arr i v))))))

(define array!-set! (fn ((: xs <array!>) i v) (flex-set! xs i v)))

(define list->array! (fn (xs) (apply array! xs)))

(define string-iterator (fn ((: s <string>)) (make <string-iterator> s 0)))

(define iter
  (make-multimethod 'iter
    string-iterator))

;;; I/O
;;; ================================================================================================

(define char-utf8-width
  (fn (c)
    (let ((codepoint (char->integer c)))
      (if (fx< codepoint #x80)
        1
        (if (fx< codepoint #x800)
          2
          (if (fx< codepoint #x10000)
            3
            4))))))

(define call-with-port
  (fn (port f)
    (let ((res (f port)))
      (close-port port)
      res)))

(define call-with-input-file (fn (filename f) (call-with-port (open-input-file filename) f)))

;;; Debug Info
;;; ================================================================================================

(define continuation-method (fn ((: continuation <continuation>)) (slot-get continuation 0)))

(define method-name (fn ((: method <method>)) (slot-get method 5)))

(define <file-coordinates> (make-slots-type '<file-coordinates> 2 #f))
(define file-coordinates (fn (line column) (make <file-coordinates> line column)))
(define file-coordinates-line (fn ((: coords <file-coordinates>)) (slot-get coords 0)))
(define file-coordinates-column (fn ((: coords <file-coordinates>)) (slot-get coords 1)))

(define display-file-coordinates
  (fn (coords)
    (write (file-coordinates-line coords))
    (write-char #":") (write (file-coordinates-column coords))))

;; OPTIMIZE: Very slow:
(define try-source-location-coords
  (fn (loc)
    (let ((filename (source-location-filename loc))
          (byte-idx (source-location-byte-index loc)))
      (if (file-exists? filename)
        (call-with-input-file filename
          (fn (file)
            (letfn (((loop line col i)
                      (if (fx< i byte-idx)
                        (let ((c (read-char file)))
                          (if (not (identical? c end))
                            (let ((i* (fx+ i (char-utf8-width c))))
                              (if (identical? c #"\n")
                                (loop (fx+ line 1) 1 i*)
                                (loop line (fx+ col 1) i*)))
                            (file-coordinates line col)))
                        (file-coordinates line col))))
              (loop 1 1 0))))
        byte-idx))))

(define continulet-trace
  (fn (k)
    (letfn (((trace k)
               (let ((method (continuation-method k)))
                 (if (isa? <method> method)
                   (let ((name (let ((name (method-name method)))
                                 (if (isa? <symbol> name) name #f)))
                         (loc (continuation-call-loc k))
                         (entry (if loc
                                  (array name
                                         (source-location-filename loc)
                                         (try-source-location-coords loc))
                                  (array name #f #f))))
                     (cons entry (trace (flex-get k 0))))
                   ()))))
      (trace k))))

(define display-trace-entry
  (fn (entry)
    (write-string "in ")
    (let ((fn-name (array-get entry 0)))
      (if fn-name
        (write fn-name)
        (write-string "#<fn>")))
    (write-string " at ")
    (let ((filename (array-get entry 1)))
      (if filename
        (write filename)
        (write-string "???")))
    (let ((coords (array-get entry 2)))
      (when coords
        (if (isa? <file-coordinates> coords)
          (do (write-char #":")
              (display-file-coordinates coords))
          (do (write-string " byte ")
              (write coords)))))))

(define display-continulet-trace
  (fn (trace)
    (for-each (fn (entry) (display-trace-entry entry) (newline))
              trace)
    #t))

;;; Delimited Continuations
;;; ================================================================================================

(define <delimited-continuation> (make-slots-type '<delimited-continuation> 4 #f))
(define delimited-continuation
  (fn (continulet stash-capture prompt on-yield)
    (make <delimited-continuation> continulet stash-capture prompt on-yield)))

(let ((<prompt-frame> (make-slots-type '<prompt-frame> 4 #f))
      (prompt-frame (fn (next continulet prompt on-yield)
                      (make <prompt-frame> next continulet prompt on-yield)))
      (prompt-frame-next (fn ((: frame <prompt-frame>)) (slot-get frame 0)))
      (prompt-frame-continulet (fn ((: frame <prompt-frame>)) (slot-get frame 1)))
      (prompt-frame-prompt (fn ((: frame <prompt-frame>)) (slot-get frame 2)))
      (prompt-frame-on-yield (fn ((: frame <prompt-frame>)) (slot-get frame 3)))

      (<wind-frame> (make-slots-type '<wind-frame> 4 #f))
      (wind-frame (fn (next continulet initially finally)
                    (make <wind-frame> next continulet initially finally)))
      (wind-frame-next (fn ((: frame <wind-frame>)) (slot-get frame 0)))
      (wind-frame-continulet (fn ((: frame <wind-frame>)) (slot-get frame 1)))
      (wind-frame-initially (fn ((: frame <wind-frame>)) (slot-get frame 2)))
      (wind-frame-finally (fn ((: frame <wind-frame>)) (slot-get frame 3)))

      (frame-next (make-multimethod 'frame-next prompt-frame-next wind-frame-next))
      (frame-continulet (make-multimethod 'frame-continulet
                                          prompt-frame-continulet
                                          wind-frame-continulet))

      (stash (box #f))
      (trampoline (box #f))
      (maybe-thunk (call-with-current-continuation (fn (k) (box-set! trampoline k) #f)))
      (_ (when maybe-thunk
           (let ((v (maybe-thunk))
                 (ks (box-get stash)))
             (box-set! stash (frame-next ks))
             (continue (frame-continulet ks) v))))

      (unwind-to (fn (stash prompt)
                   (letfn (((unwind-to frame capture)
                              (if (isa? <prompt-frame> frame)
                                (if (identical? (prompt-frame-prompt frame) prompt)
                                  (array frame capture)
                                  (unwind-to (prompt-frame-next frame)
                                             (prompt-frame capture
                                                           (prompt-frame-continulet frame)
                                                           (prompt-frame-prompt frame)
                                                           (prompt-frame-on-yield frame))))
                                (if (isa? <wind-frame> frame)
                                  (let ((finally (wind-frame-finally frame)))
                                    (finally)
                                    (unwind-to (wind-frame-next frame)
                                               (wind-frame capture
                                                           (wind-frame-continulet frame)
                                                           (wind-frame-initially frame)
                                                           finally)))
                                  (error 'prompt-not-set prompt)))))
                     (unwind-to stash #f))))
      (unwind-abort-to (fn (stash prompt)
                         (letfn (((unwind-abort-to frame)
                                    (if (isa? <prompt-frame> frame)
                                      (if (identical? (prompt-frame-prompt frame) prompt)
                                        frame
                                        (unwind-abort-to (prompt-frame-next frame)))
                                      (if (isa? <wind-frame> frame)
                                        (unwind-abort-to (wind-frame-next frame))
                                        (error 'prompt-not-set prompt)))))
                           (unwind-abort-to stash))))
      (take-subcont (fn (prompt f)
                      (call-with-current-continuation
                        (fn (continulet)
                          (let ((stash&stash-capture (unwind-to (box-get stash) prompt))
                                (stash* (array-get stash&stash-capture 0))
                                (on-yield (prompt-frame-on-yield stash*))
                                (_ (box-set! stash stash*))
                                (k (delimited-continuation continulet
                                                           (array-get stash&stash-capture 1)
                                                           prompt
                                                           on-yield)))
                            (continue (box-get trampoline) (f k)))))))

      (rewind (fn (stash capture)
                (letfn (((rewind stash capture)
                           (if (isa? <prompt-frame> capture)
                             (rewind (prompt-frame stash
                                                   (prompt-frame-continulet capture)
                                                   (prompt-frame-prompt capture)
                                                   (prompt-frame-on-yield capture))
                                     (prompt-frame-next capture))
                             (if (isa? <wind-frame> capture)
                               (let ((initially (wind-frame-initially capture)))
                                 (initially)
                                 (rewind (wind-frame stash
                                                     (wind-frame-continulet capture)
                                                     initially
                                                     (wind-frame-finally capture))
                                         (wind-frame-next capture)))
                               stash))))
                  (rewind stash capture))))
      (push-subcont (fn (k thunk)
                      (call-with-current-continuation
                        (fn (continulet)
                          (let ((continulet* (slot-get k 0))
                                (stash-capture (slot-get k 1)))
                                ;; Prompt and handler of `k` ignored in favor of placeholders
                            (box-set! stash (prompt-frame (box-get stash)
                                                          continulet
                                                          (prompt)       ; placeholder
                                                          (fn (k v) v))) ; placeholder
                            (box-set! stash (rewind (box-get stash) stash-capture))
                            (continue continulet* thunk))))))
      (push-delim-subcont (fn (k thunk)
                            (call-with-current-continuation
                              (fn (k)
                                (let ((continulet (slot-get k 0))
                                      (stash-capture (slot-get k 1))
                                      (prompt (slot-get k 2))
                                      (on-yield (slot-get k 3)))
                                  (box-set! stash (prompt-frame (box-get stash)
                                                                continulet
                                                                prompt
                                                                on-yield))
                                  (box-set! stash (rewind (box-get stash) stash-capture))
                                  (continue continulet thunk)))))))
  (define call-delimited-continuation (fn (k v) (push-subcont k (fn () v))))

  (define <prompt> (make-slots-type '<prompt> 0 #f))
  (define prompt (fn () (make <prompt>)))
  (define default-prompt 'default-prompt)

  (define call-with-prompt
    (fn (prompt thunk on-yield)
      (call-with-current-continuation
        (fn (continulet)
          (box-set! stash (prompt-frame (box-get stash) continulet prompt on-yield))
          (continue (box-get trampoline) thunk)))))

  (define dynamic-wind
    (fn (initially thunk finally)
      (call-with-current-continuation
        (fn (continulet)
          (box-set! stash (wind-frame (box-get stash) continulet initially finally))
          (continue (box-get trampoline)
                    (fn ()
                      (initially)
                      (let ((res (thunk)))
                        (finally)
                        res)))))))

  (define call-at-prompt
    (fn (prompt thunk)
      (let ((stash* (unwind-abort-to (box-get stash) prompt))
            (continulet (prompt-frame-continulet stash*)))
        (box-set! stash (prompt-frame-next stash*))
        (continue continulet thunk))))

  (define yield-to
    (fn (prompt v)
      (take-subcont prompt
                    (fn (k)
                      (fn ()
                        (let ((on-yield (slot-get k 3)))
                          (on-yield k v)))))))
  (define yield (fn (v) (yield-to default-prompt v)))

  ;; TODO: Prune `trampoline` from traces:
  (define stash-trace
    (fn (stash)
      (letfn (((trace frame)
                 (if frame
                   (let ((t (cons (continulet-trace (frame-continulet frame))
                                  (trace (frame-next frame)))))
                     (if (isa? <prompt-frame> frame)
                       (cons (prompt-frame-prompt frame) t)
                       t))
                   ())))
        (trace stash))))
  (define continuation-trace
    (fn (k)
      (cons (continulet-trace (slot-get k 0))
            (reverse (stash-trace (slot-get k 1))))))
  (define current-continuation-trace
    (fn ()
      (call-with-current-continuation
        (fn (continulet)
          (cons (continulet-trace continulet)
                (stash-trace (box-get stash)))))))
  (define display-continuation-trace
    (fn (trace)
      (for-each (fn (entry)
                  (if (if (isa? <pair> entry) #t (identical? entry ()))
                    (display-continulet-trace entry)
                    (do (write-string "on prompt ") (write entry) (newline))))
                trace)
      #t)))

;;; Parameters
;;; ================================================================================================

(define <parameter> (make-slots-type '<parameter> 1 #f))
(define parameter (fn (v) (make <parameter> v)))
(define parameter-get (fn ((: parameter <parameter>)) (slot-get parameter 0)))
(define parameter-set! (fn ((: parameter <parameter>) v) (slot-set! parameter 0 v)))

(define-macro (parameterize _ form)
  (let ((bindings (cadr form))
        (body (cddr form)))
    (letfn (((emit bindings)
               (if (isa? <pair> bindings)
                 (let ((binding (car bindings))
                       (param-name (gensym))
                       (old-value-name (gensym))
                       (value-name (gensym)))
                   `(let ((,param-name ,(car binding))
                          (,old-value-name (parameter-get ,param-name))
                          (,value-name ,(cadr binding)))
                       (dynamic-wind (fn () (parameter-set! ,param-name ,value-name))
                                     (fn () ,(emit (cdr bindings)))
                                     (fn () (parameter-set! ,param-name ,old-value-name)))))
                 `(do ,@body))))
      (emit bindings))))

;;; Exception Handling
;;; ================================================================================================

(define continuable? (fn (exn) #f)) ; TODO: Continuable

(define *exception-handlers* (parameter ()))

(define with-exception-handler
  (fn (thunk handle-exception)
    (parameterize ((*exception-handlers* (cons handle-exception
                                               (parameter-get *exception-handlers*))))
      (thunk))))

(define throw
  (fn (exn)
    (let ((exception-handlers (parameter-get *exception-handlers*))
          (handle-exception (car exception-handlers)))
      (parameterize ((*exception-handlers* (cdr exception-handlers))) ; HACK?
        (if (continuable? exn)
          (handle-exception exn)
          (do (handle-exception exn)
              (throw exn)))))))

(define try*
  (fn (thunk handle-exception)
    (let ((p (prompt)))
      (call-with-prompt p
                        (fn ()
                          (with-exception-handler
                            thunk
                            (fn (exn)
                              (let ((v (handle-exception exn)))
                                (call-at-prompt p (fn () v))))))
                        (fn (k v) v))))) ; Unreachable

(set! *error-handler* throw)

;;; Self-Hosting REPL
;;; ================================================================================================

(define repl
  (fn (input debug)
    (let ((prompt "vesihiisi> "))
      (letfn (((loop)
                 (write-string prompt)
                 (flush-output-port)
                 (let ((line (read-line input)))
                   (if (not (identical? line end))
                     (do
                       (try*
                         (fn ()
                           (let ((loc&expr (read* (make <string-iterator> line 0)
                                                  (make <source-location> "REPL" 0)))
                                 (v (eval (array-get loc&expr 1) (array-get loc&expr 0) debug)))
                             (write v) (newline)))
                         (fn (exn)
                           (write-string "Uncaught exception: ") (write exn) (newline)
                           (display-continuation-trace (current-continuation-trace))))
                       (loop))
                     end))))
        (loop)))))
