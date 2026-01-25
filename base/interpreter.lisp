(define <config> (make-slots-type '<config> 6 #f))
(define config-name (fn ((: config <config>)) (slot-get config 0)))
(define config-from-stdin (fn ((: config <config>)) (slot-get config 1)))
(define set-config-from-stdin! (fn ((: config <config>) v) (slot-set! config 1 v)))
(define config-filename (fn ((: config <config>)) (slot-get config 2)))
(define set-config-filename! (fn ((: config <config>) v) (slot-set! config 2 v)))
(define config-debug (fn ((: config <config>)) (slot-get config 3)))
(define set-config-debug! (fn ((: config <config>) v) (slot-set! config 3 v)))
(define config-help (fn ((: config <config>)) (slot-get config 4)))
(define set-config-help! (fn ((: config <config>) v) (slot-set! config 4 v)))
(define config-force-interactive (fn ((: config <config>)) (slot-get config 5)))
(define set-config-force-interactive! (fn ((: config <config>) v) (slot-set! config 5 v)))

(define long-flag-names (array "debug" "help" "interactive"))
(define short-flag-names (array #"d" #"h" #"i"))
(define flag-descriptions
  (array "turn on debug output (for developing the VM)"
         "display this help and exit"
         "force interactive i.e. if FILE is given launch repl after loading it"))
(define flag-inits
  (array (fn (config) (set-config-debug! config #t))
         (fn (config) (set-config-help! config #t))
         (fn (config) (set-config-force-interactive! config #t))))


(define parse-argv
  (fn ()
    (let ((config (make <config>
                        (array-get *command-line* 0)
                        #f
                        #f
                        #f
                        #f
                        #f)))
      (letfn (((parse-flags i)
                (if (fx< i (array-count *command-line*))
                  (let ((arg (array-get *command-line* i))
                        (arg-it (make <string-iterator> arg 0)))
                    (if (identical? (next! arg-it) #"-") ; Flag?
                      (let ()
                        (if (identical? (peek arg-it) #"-") ; Long flag?
                          (let ((arg-name (flex-copy arg 2 (flex-count arg))) ; FIXME: `substring`
                                (flag-idx (index-of long-flag-names arg-name)))
                            (if flag-idx
                              ((array-get flag-inits flag-idx) config)
                              (error 'invalid-cli-flag arg-name)))
                          (if (not (identical? (peek arg-it) end)) ; Short flags?
                            (letfn (((parse-short-flags it)
                                      (let ((c (next! it)))
                                        (if (not (identical? c end))
                                          (let ((flag-idx (index-of short-flag-names c)))
                                            (if flag-idx
                                              (let ((_ ((array-get flag-inits flag-idx) config)))
                                                (parse-short-flags it))
                                              (error 'invalid-cli-arg c)))
                                          #f))))
                              (parse-short-flags arg-it))
                            (set-config-from-stdin! config #t))) ; Special flag `-`
                        (parse-flags (+ i 1)))
                      i))
                  i)))
        (let ((i (parse-flags 1))
              (i (if (fx< i (array-count *command-line*))
                   (let ()
                     (if (config-from-stdin config)
                       (error 'inconsistent-cli-args *command-line*)
                       #f)
                     (set-config-filename! config (array-get *command-line* i))
                     (fx+ i 1))
                   i)))
          (if (fx< i (array-count *command-line*))
            (error 'extra-cli-args *command-line*)
            config))))))

(define print-help
  (fn (config)
    (write-string "Usage: ")
    (write-string (config-name config)) (write-string " [OPTION]... [FILE]\n")
    (write-string "An uncommon Lisp. Runs FILE if given, else launches a REPL.\n")

    (newline)

    (fold (fn (long-name i)
            (write-string "  -") (write-char (array-get short-flag-names i))
            (write-string ", --") (write-string long-name)
            (write-char #"\t") (write-string (array-get flag-descriptions i))
            (newline)
            (fx+ i 1))
          0 long-flag-names)

    (write-string "  -\tread program from stdin (as if from FILE)\n")
    #f))

(define main
  (fn ()
    (let ((config (parse-argv))
          (filename (config-filename config)))
      (if (config-help config)
        (let ()
          (print-help config)
          (exit #t))
        (let ()
          (if filename
            (load filename (config-debug config))
            (if (config-from-stdin config)
              (load standard-input "STDIN" (config-debug config))
              #f))

          (if (if (not (config-filename config))
                (if (not (config-from-stdin config))
                  #t
                  (config-force-interactive config))
                (config-force-interactive config))
            (let ()
              (repl standard-input (config-debug config))
              (newline))
            #f)))

      (exit #t))))

(main)
