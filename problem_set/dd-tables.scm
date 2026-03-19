;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECTANGULAR Fundamental
;; ('rectangular  make-complex-rectangular)
;; ('rectangular  is-complex-rectangular?)
;; ('rectangular  get-type-tag-rectangular)
;; ('rectangular  get-real-part-rectangular)
;; ('rectangular  get-imag-part-rectangular)
;; ('rectangular  get-conjugate-rectangular)
;; ('rectangular  add-rectangular-complex)
;; ('rectangular  subtract-rectangular-complex)
;; ('rectangular  multiply-rectangular-complex)
;; ('rectangular  is-zero-rectangular?)
;; ('rectangular  is-pure-real-rectangular?)
;; ('rectangular  is-pure-imaginary-rectangular?)
;; POLAR Fundamental
;; ('polar  make-complex-polar)
;; ('polar  is-complex-polar?)
;; ('polar  get-magnitude-polar)
;; ('polar  get-angle-polar)
;; ('polar  get-conjugate-polar)
;; ('polar  add-polar-complex)
;; ('polar  subtract-polar-complex)
;; ('polar  multiply-polar-complex)
;; ('polar  is-zero-polar?)
;; ('polar  is-pure-real-polar?)
;; ('polar  is-pure-imaginary-polar?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECTANGULAR Fundamental
;; (make-complex-rectangular  is-complex-rectangular?    get-type-tag-rectangular
;;  get-real-part-rectangular get-imag-part-rectangular  get-conjugate-rectangular
;;  add-rectangular-complex  subtract-rectangular-complex  multiply-rectangular-complex
;;  is-zero-rectangular?  is-pure-real-rectangular?  is-pure-imaginary-rectangular?)
;; POLAR Fundamental
;; (make-complex-polar  is-complex-polar?  get-magnitude-polar  get-angle-polar
;;  get-conjugate-polar  add-polar-complex  subtract-polar-complex
;;  multiply-polar-complex  is-zero-polar?  is-pure-real-polar?  is-pure-imaginary-polar?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "non-data-directed-complex.scm")

(define list-of-rectangular-ops 
  (list make-complex-rectangular
        is-complex-rectangular?
        get-type-tag-rectangular
        get-real-part-rectangular
        get-imag-part-rectangular
        get-conjugate-rectangular
        add-rectangular-complex
        subtract-rectangular-complex
        multiply-rectangular-complex
        get-magnitude-rectangular
        get-angle-rectangular
        is-zero-rectangular?
        is-pure-real-rectangular?
        is-pure-imaginary-rectangular?
        ))

(define list-of-polar-ops 
  (list make-complex-polar
        is-complex-polar?
        get-type-tag-polar
        is-zero-polar?
        is-pure-real-polar?
        is-pure-imaginary-polar?
        get-magnitude-polar
        get-angle-polar
        get-real-part-polar
        get-imag-part-polar
        get-conjugate-polar
        add-polar-complex
        subtract-polar-complex
        multiply-polar-complex
        ))

(define list-of-generic-ops 
  (list 'make-complex
        'is-complex?
        'get-type-tag
        'is-zero?
        'is-pure-real?
        'is-pure-imaginary?
        'get-magnitude
        'get-angle
        'get-real-part
        'get-imag-part
        'get-conjugate
        'add-complex
        'subtract-complex
        'multiply-complex
        ))


(define list-of-rep-symbols
  (list 'rectangular 'polar))

(define make-sym-list
  (lambda (list-name-sym list-of-syms)
    (cons list-name-sym list-of-syms)))

(define make-key-proc-record            ; i.e. a pair if key is a symbol and proc is a procedure, #f otherwise
  (lambda (key proc)
    (if (and (symbol?    key)
             (procedure? proc))
        (cons key proc)
        #f)))

(define init-key-proc-col (lambda () (cons '() '()))) ; Procedure with no arguments.

(define install-key-proc-in-col!
  (lambda (key-proc-col key proc)
    (if (null? (car key-proc-col))
        (set-car! key-proc-col (make-key-proc-record key proc))
        (if (null? (cdr key-proc-col))
            (set-cdr! key-proc-col (cons (make-key-proc-record key proc) '()))
            (install-key-proc-in-col! (cdr key-proc-col) key proc)))))

(define make-key-proc-col
  (lambda (rep-sym key-proc-col logo locro)
    (if (and (eq? (length logo) (length locro))
             (and (null?  logo) (null?  locro)))
        (list rep-sym key-proc-col)
        (let ((gen-op        (car logo))
              (the-proc      (car locro))
              (rest-ops      (cdr logo))
              (rest-procs    (cdr locro)))
          (begin
            (install-key-proc-in-col! key-proc-col  gen-op  the-proc)
            (make-key-proc-col  rep-sym  key-proc-col  (cdr logo)  (cdr locro)))))))

(define rectangular-ops (init-key-proc-col))
(set! rectangular-ops (make-key-proc-col 'rectangular rectangular-ops list-of-generic-ops list-of-rectangular-ops))
(define polar-ops       (init-key-proc-col))
(set! polar-ops       (make-key-proc-col 'polar       polar-ops       list-of-generic-ops list-of-polar-ops))

(define complex-ops-table (list rectangular-ops polar-ops))

(define get-ops-list
  (lambda (rep-sym ops-tab)
    (if (null? ops-tab)
        #f
        (let ((ops-sym    (caar  ops-tab))
              (ops-list   (cadar ops-tab))
              (rest-tab   (cdr   ops-tab)))
          (if (eq? rep-sym ops-sym)
              ops-list
              (get-ops-list rep-sym rest-tab))))))

(define get-ops-for-gen-ops
  (lambda (gen-op-sym  ops-list)
    (if (null? ops-list)
        ops-list
        (let   ((ops-pair    (car ops-list))
                (rest-ops    (cdr ops-list)))
          (let   ((op-sym    (car ops-pair))
                  (op-proc   (cdr ops-pair)))
            (if (eq? gen-op-sym  op-sym)
                op-proc
                (get-ops-for-gen-ops   gen-op-sym   rest-ops)))))))

(define lookup-proc-for-gen-op
  (lambda (key-proc-col proc-key)
    (if (null? key-proc-col)
        #f
        (let ((the-pair   (car key-proc-col))
              (rest-pairs (cdr key-proc-col)))
          (let ((gen-op-key   (car the-pair))
                (the-op       (cdr the-pair)))
            (if (eq? gen-op-key  proc-key)
                the-op
                (lookup-proc-for-gen-op rest-pairs proc-key)))))))

