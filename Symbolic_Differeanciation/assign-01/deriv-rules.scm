;;; ============================================================
;;;  deriv-rules.scm  –  Differentiation rules (data-directed)
;;; ============================================================

;;; helper: differentiate + simplify
(define (ds expr var) (simplify (deriv expr var)))

;;; ──────────────────────────────────────────────────────────
;;;  ARITHMETIC
;;; ──────────────────────────────────────────────────────────

;;; d/dx(u + v) = u' + v'
(put-deriv! '+
  (lambda (expr var)
    (simplify (make-sum (ds (operand1 expr) var)
                        (ds (operand2 expr) var)))))

;;; d/dx(u - v) = u' - v'   |   d/dx(-u) = -u'
(put-deriv! '-
  (lambda (expr var)
    (if (unary? expr)
        (simplify (make-negation (ds (operand1 expr) var)))
        (simplify (make-difference (ds (operand1 expr) var)
                                   (ds (operand2 expr) var))))))

;;; d/dx(u * v) = u*v' + v*u'
(put-deriv! '*
  (lambda (expr var)
    (let ((u (operand1 expr)) (v (operand2 expr)))
      (simplify (make-sum (make-product u (ds v var))
                          (make-product v (ds u var)))))))

;;; d/dx(u / v) = (v*u' - u*v') / v^2
(put-deriv! '/
  (lambda (expr var)
    (let ((u (operand1 expr)) (v (operand2 expr)))
      (simplify
        (make-quotient
          (make-difference (make-product v (ds u var))
                           (make-product u (ds v var)))
          (make-power v 2))))))

;;; Power rule (three cases)
;;;   Case 1: u^n  (n numeric)  →  n * u^(n-1) * u'
;;;   Case 2: c^v  (c numeric)  →  c^v * log(c) * v'
;;;   Case 3: u^v  (symbolic)   →  u^v * (v/u*u' + log(u)*v')
(put-deriv! '**
  (lambda (expr var)
    (let ((u (operand1 expr)) (v (operand2 expr)))
      (cond
        ((number? v)
         (simplify (make-product (make-product v (make-power u (- v 1)))
                                 (ds u var))))
        ((number? u)
         (simplify (make-product (make-product expr (make-app 'log u))
                                 (ds v var))))
        (else
         (simplify
           (make-product expr
             (make-sum (make-product (make-quotient v u) (ds u var))
                       (make-product (make-app 'log u) (ds v var))))))))))

;;; ──────────────────────────────────────────────────────────
;;;  TRIGONOMETRIC  (no inverse trig per spec)
;;; ──────────────────────────────────────────────────────────

;;; d/dx(sin u) = cos(u) * u'
(put-deriv! 'sin
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-app 'cos u) (ds u var))))))

;;; d/dx(cos u) = -sin(u) * u'
(put-deriv! 'cos
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-negation (make-app 'sin u))
                              (ds u var))))))

;;; d/dx(tan u) = 1/cos²(u) * u'
(put-deriv! 'tan
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-quotient 1 (make-power (make-app 'cos u) 2))
                              (ds u var))))))

;;; d/dx(cot u) = -1/sin²(u) * u'
(put-deriv! 'cot
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-negation
                                (make-quotient 1 (make-power (make-app 'sin u) 2)))
                              (ds u var))))))

;;; d/dx(sec u) = sec(u)*tan(u) * u'
(put-deriv! 'sec
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-product expr (make-app 'tan u))
                              (ds u var))))))

;;; d/dx(csc u) = -csc(u)*cot(u) * u'
(put-deriv! 'csc
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product (make-negation
                                (make-product expr (make-app 'cot u)))
                              (ds u var))))))

;;; ──────────────────────────────────────────────────────────
;;;  EXPONENTIAL  &  LOGARITHMIC
;;; ──────────────────────────────────────────────────────────

;;; d/dx(exp u) = exp(u) * u'
(put-deriv! 'exp
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-product expr (ds u var))))))

;;; d/dx(log u) = u' / u   (natural log)
(put-deriv! 'log
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-quotient (ds u var) u)))))

;;; d/dx(log2 u) = u' / (u * log(2))
(put-deriv! 'log2
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-quotient (ds u var)
                               (make-product u (make-app 'log 2)))))))

;;; d/dx(log10 u) = u' / (u * log(10))
(put-deriv! 'log10
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-quotient (ds u var)
                               (make-product u (make-app 'log 10)))))))

;;; d/dx(sqrt u) = u' / (2 * sqrt(u))
(put-deriv! 'sqrt
  (lambda (expr var)
    (let ((u (operand1 expr)))
      (simplify (make-quotient (ds u var)
                               (make-product 2 expr))))))
