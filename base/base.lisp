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

;;; Delimited Continuations
;;; ================================================================================================

;;; OPTIMIZE

(let ((stash (box ()))
      (trampoline (box #f))
      (maybe-thunk (call-with-current-continuation (fn (k) (box-set! trampoline k) #f)))
      (_ (when maybe-thunk
           (let ((v (maybe-thunk))
                 (ks (box-get stash)))
             (box-set! stash (cdr ks)) ; FIXME: What is `ks` is `()`?
             (continue (car ks) v)))))
  (define reset*
    (fn (thunk)
      (call-with-current-continuation
        (fn (k)
          (box-set! stash (cons k (box-get stash)))
          (continue (box-get trampoline) thunk)))))

  (define shift*
    (fn (f)
      (call-with-current-continuation
        (fn (k)
          (continue (box-get trampoline)
                    (fn ()
                      (f (fn (v)
                           (call-with-current-continuation
                             (fn (k*)
                               (box-set! stash (cons k* (box-get stash)))
                               (continue k v)))))))))))

  (define <yield> (make-slots-type '<yield> 3 #f))
  (define yield-prompt (fn ((: y <yield>)) (slot-get y 0)))
  (define yield-value (fn ((: y <yield>)) (slot-get y 1)))
  (define yield-continuation (fn ((: y <yield>)) (slot-get y 2)))

  (define yield-to (fn (prompt v) (shift* (fn (k) (make <yield> prompt v k)))))
  (define yield (fn (v) (yield-to (box #f) v)))

  (define try-yield*
    (fn (thunk on-yield finish)
      (let ((v (thunk)))
        (if (isa? <yield> v)
          (on-yield (yield-prompt v) (yield-value v) (yield-continuation v))
          (finish v)))))

  (define try-yield-at*
    (fn (prompt thunk on-yield finish)
      (letfn (((loop thunk)
                 (let ((v (thunk)))
                   (try-yield* (fn () v)
                               (fn (yield-prompt v k)
                                 (if (identical? yield-prompt prompt)
                                   (on-yield yield-prompt v k)
                                   (let ((v* (yield-to yield-prompt v)))
                                     (loop (fn () (k v*))))))
                               (fn (v) v)))))
        (loop (fn () (reset* thunk))))))

  (define dynamic-wind
    (fn (initially thunk finally)
      (letfn (((loop thunk)
                 (initially)
                 (let ((v (thunk)))
                   (finally)
                   (try-yield* (fn () v)
                               (fn (p v k)
                                 (let ((v* (yield-to p v)))
                                   (loop (fn () (k v*)))))
                               (fn (v) v)))))
        (loop (fn () (reset* thunk)))))))

(define <prompt> (make-slots-type '<prompt> 0 #f))
(define prompt (fn () (make <prompt>)))

(define continuable? (fn (exn) #f)) ; TODO: Continuable

(define with-exception-handler
  (fn (thunk handle-exception)
    (try-yield-at* 'catch
                   thunk
                   (fn (p exn k)
                     (if (continuable? exn)
                       (k (handle-exception exn))
                       (do (handle-exception exn)
                           (yield-to p exn))))
                   (fn (v) v))))

(define try*
  (fn (thunk handle-exception)
    (let ((p (prompt)))
      (try-yield-at* p
                     (fn ()
                       (with-exception-handler
                         thunk
                         (fn (exn) (yield-to p (handle-exception exn))))) ; OPTIMIZE: `abort-to`
                     (fn (_ v __) v)
                     (fn (v) v)))))

(define throw (fn (exn) (yield-to 'catch exn)))

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
                         (fn (exn) (write-string "Uncaught exception: ") (write exn) (newline)))
                       (loop))
                     end))))
        (loop)))))
