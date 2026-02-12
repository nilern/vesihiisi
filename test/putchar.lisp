(define libc (open-foreign-library #f))

(define putchar
  (let ((putchar (get-foreign libc "putchar")))
    (fn ((: c <char>))
      (call-foreign <fixnum> putchar c))))

(define main
  (fn ()
    (let ((res (putchar #"c")))
      (close-foreign-library libc)
      res)))

(main)
