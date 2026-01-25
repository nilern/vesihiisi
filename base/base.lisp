;;; Macros for Defining Macros
;;; ================================================================================================

(define define-macro
  (fn (_ form)
    (let ((interface (cadr form))
          (name (car interface))
          (params (cdr interface))
          (body (cddr form)))
      ;; TODO: Use the quasiquote reader macros when they become available:
      (quasiquote
        (let ()
          (define (unquote name)
            (fn (unquote params)
              (unquote-splicing body)))
          (var-set-macro-category! (resolve (quote (unquote name))) 'fn-macro))))))
(var-set-macro-category! (resolve 'define-macro) 'fn-macro)

(define-macro (define-symbol-macro _ form)
  (let ((interface (cadr form))
        (name (car interface))
        (params (cdr interface))
        (body (cddr form)))
    ;; TODO: Use the quasiquote reader macros when they become available:
    (quasiquote
      (let ()
        (define (unquote name)
          (fn (unquote params)
            (unquote-splicing body)))
        (var-set-macro-category! (resolve (quote (unquote name))) 'symbol-macro)))))

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
                     (let ((loc&expr (read* (make <string-iterator> line 0)
                                            (make <source-location> "REPL" 0)))
                           (v (eval (array-get loc&expr 1) (array-get loc&expr 0) debug)))
                       (write v)
                       (newline)
                       (loop))
                     end))))
        (loop)))))
