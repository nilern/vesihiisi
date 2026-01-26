;;; Adapted from https://okmij.org/ftp/continuations/implementations.html#dynamic-wind

(define current-line-width 80)

(define with-new-line-width-yl
  (fn (new-lw thunk)
    (let ((old (box #f)))
      (dynamic-wind
        (fn ()
          (box-set! old current-line-width)
          (set! current-line-width new-lw))
        thunk
        (fn () (set! current-line-width (box-get old)))))))

(define task
  (fn (title)
    (write-string title)
    (write-string "Current line width: ") (write current-line-width) (newline)))

(define for-loop
  (fn (generator body)
    (letfn (((loop thr)
               (try-yield* (fn () thr)
                           (fn (v k)
                             (body v)
                             (loop (k #f))))))
      (loop (reset* (fn () (generator)))))))

(define ex7
  (fn ()
    (task "Begin. ")
    (for-loop (fn ()
                (with-new-line-width-yl 120
                  (fn ()
                    (task "Inner1. ")
                    (yield 1)
                    (task "Inner2. ")
                    (yield 2)
                    (task "Inner3. "))))
              (fn (v)
                (newline)
                (write-string "Yielded value: ") (write v) (newline)
                (task "Loop body. ")))
    (task "End. ")))

(ex7)
