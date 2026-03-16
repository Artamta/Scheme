;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbolic Differentiation System
;; CTS Assignment I
;; Ayush Raj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variable to differentiate with respect to
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
        ((and (number? a) (number? b))
         (+ a b))
        (else (list '+ a b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b))
         (* a b))
        (else (list '* a b))))

(define (make-power base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '^ base exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION ACCESSORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operator exp) (car exp))
(define (operand1 exp) (cadr exp))
(define (operand2 exp) (caddr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIFFERENTIATION RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv exp)

  (cond

    ;; constant
    ((number? exp) 0)

    ;; variable
    ((variable? exp)
     (if (same-variable? exp var) 1 0))

    ;; sum rule
    ((eq? (operator exp) '+)
     (make-sum
      (deriv (operand1 exp))
      (deriv (operand2 exp))))

    ;; product rule
    ((eq? (operator exp) '*)
     (make-sum
      (make-product
       (operand1 exp)
       (deriv (operand2 exp)))
      (make-product
       (operand2 exp)
       (deriv (operand1 exp)))))

    ;; power rule (x^n)
    ((eq? (operator exp) '^)
     (let ((base (operand1 exp))
           (power (operand2 exp)))
       (make-product
        power
        (make-product
         (make-power base (- power 1))
         (deriv base)))))

    ;; sin
    ((eq? (operator exp) 'sin)
     (make-product
      (list 'cos (operand1 exp))
      (deriv (operand1 exp))))

    ;; cos
    ((eq? (operator exp) 'cos)
     (make-product
      -1
      (make-product
       (list 'sin (operand1 exp))
       (deriv (operand1 exp)))))

    ;; exp
    ((eq? (operator exp) 'exp)
     (make-product
      (list 'exp (operand1 exp))
      (deriv (operand1 exp))))

    ;; log
    ((eq? (operator exp) 'log)
     (make-product
      (make-power (operand1 exp) -1)
      (deriv (operand1 exp))))

    (else
     (error "Unknown expression type -- DERIV" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (differentiate expr)
  (deriv expr))
