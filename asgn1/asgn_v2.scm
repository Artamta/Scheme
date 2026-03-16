;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbolic Differentiation System
;; CTS Assignment I
;; Ayush Raj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define var 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? x)
  (symbol? x))

(define (same-variable? x y)
  (and (symbol? x) (symbol? y) (eq? x y)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMPLIFIER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))

(define (make-power base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '^ base exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION ACCESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operator exp) (car exp))
(define (operand1 exp) (cadr exp))
(define (operand2 exp) (caddr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA-DIRECTED DERIVATIVE TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define deriv-table '())

(define (put op proc)
  (set! deriv-table
        (cons (cons op proc) deriv-table)))

(define (get op)
  (let ((record (assoc op deriv-table)))
    (if record
        (cdr record)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN DERIVATIVE FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv exp)

  (cond

   ((number? exp) 0)

   ((variable? exp)
    (if (same-variable? exp var) 1 0))

   (else
    (let ((proc (get (operator exp))))
      (if proc
          (proc exp)
          (error "Unknown operator" (operator exp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL DERIVATIVE RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; addition
(put '+ 
     (lambda (exp)
       (make-sum
        (deriv (operand1 exp))
        (deriv (operand2 exp)))))

;; multiplication
(put '*
     (lambda (exp)
       (make-sum
        (make-product
         (operand1 exp)
         (deriv (operand2 exp)))
        (make-product
         (operand2 exp)
         (deriv (operand1 exp))))))

;; power
(put '^
     (lambda (exp)
       (let ((base (operand1 exp))
             (power (operand2 exp)))
         (make-product
          power
          (make-product
           (make-power base (- power 1))
           (deriv base))))))

;; sin
(put 'sin
     (lambda (exp)
       (make-product
        (list 'cos (operand1 exp))
        (deriv (operand1 exp)))))

;; cos
(put 'cos
     (lambda (exp)
       (make-product
        -1
        (make-product
         (list 'sin (operand1 exp))
         (deriv (operand1 exp))))))

;; exp
(put 'exp
     (lambda (exp)
       (make-product
        (list 'exp (operand1 exp))
        (deriv (operand1 exp)))))

;; log
(put 'log
     (lambda (exp)
       (make-product
        (make-power (operand1 exp) -1)
        (deriv (operand1 exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (differentiate expr)
  (deriv expr))
