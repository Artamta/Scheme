;;; deriv.scm
;;; Symbolic differentiation for single variable expressions.
;;; Expressions are in prefix notation like Scheme uses.
;;; e.g.  (+ x (* 3 (** x 2)))  means  x + 3x^2
;;;
;;; Load this file in guile:  (load "deriv.scm")
;;; Then try:  (deriv '(** x 3) 'x)

;;; -----------------------------------------------------------
;;; Part 1 - a simple table for data-directed dispatch
;;; -----------------------------------------------------------
;;; We store rules as a list of (operator . procedure) pairs.
;;; put-rule! adds a new rule, get-rule looks one up.

(define *table* '())

(define (put-rule! op proc)
  (set! *table* (cons (cons op proc) *table*)))

(define (get-rule op)
  (let ((entry (assq op *table*)))
    (if entry
        (cdr entry)
        #f)))

;;; -----------------------------------------------------------
;;; Part 2 - helpers for building and reading expressions
;;; -----------------------------------------------------------

(define (variable? e)   (symbol? e))
(define (number? e)     (number? e))   ; already in scheme but nice to name
(define (same-var? v w) (and (variable? v) (variable? w) (eq? v w)))

;; selectors
(define (op e)    (car e))
(define (arg1 e)  (cadr e))
(define (arg2 e)  (caddr e))

;; constructors  -- we call simplify on the way out so results stay clean
(define (make+ a b)   (simplify (list '+ a b)))
(define (make- a b)   (simplify (list '- a b)))
(define (make-neg a)  (simplify (list '- a)))
(define (make* a b)   (simplify (list '* a b)))
(define (make/ a b)   (simplify (list '/ a b)))
(define (make** b e)  (simplify (list '** b e)))
(define (make-fn f u) (list f u))     ; e.g. (make-fn 'sin 'x) -> (sin x)

;;; -----------------------------------------------------------
;;; Part 3 - the simplifier
;;; -----------------------------------------------------------
;;; We keep this simple but cover the most important cases.
;;; It works bottom-up: simplify children before the parent.

(define (simplify e)
  (if (not (pair? e))
      e
      (let ((e2 (cons (car e) (map simplify (cdr e)))))
        (cond
          ((eq? (car e2) '+)  (simp+ (cadr e2) (caddr e2)))
          ((eq? (car e2) '-)
           (if (= (length e2) 2)
               (simp-neg (cadr e2))
               (simp- (cadr e2) (caddr e2))))
          ((eq? (car e2) '*)  (simp* (cadr e2) (caddr e2)))
          ((eq? (car e2) '/)  (simp/ (cadr e2) (caddr e2)))
          ((eq? (car e2) '**) (simp** (cadr e2) (caddr e2)))
          (else e2)))))

(define (num? x) (number? x))

(define (simp+ a b)
  (cond
    ((and (num? a) (num? b)) (+ a b))
    ((and (num? a) (= a 0))  b)
    ((and (num? b) (= b 0))  a)
    ((equal? a b)            (simp* 2 a))
    (else (list '+ a b))))

(define (simp- a b)
  (cond
    ((and (num? a) (num? b)) (- a b))
    ((and (num? b) (= b 0))  a)
    ((and (num? a) (= a 0))  (simp-neg b))
    ((equal? a b)            0)
    (else (list '- a b))))

(define (simp-neg a)
  (cond
    ((num? a) (- a))
    ;; -(-x) = x
    ((and (pair? a) (eq? (car a) '-) (= (length a) 2))
     (cadr a))
    (else (list '- a))))

(define (simp* a b)
  (cond
    ((and (num? a) (num? b)) (* a b))
    ((and (num? a) (= a 0))  0)
    ((and (num? b) (= b 0))  0)
    ((and (num? a) (= a 1))  b)
    ((and (num? b) (= b 1))  a)
    ((and (num? a) (= a -1)) (simp-neg b))
    ((and (num? b) (= b -1)) (simp-neg a))
    ;; put the number in front: x*3 -> 3*x
    ((and (num? b) (not (num? a))) (list '* b a))
    (else (list '* a b))))

(define (simp/ a b)
  (cond
    ((and (num? a) (num? b) (not (= b 0)))
     ;; keep as fraction if not whole
     (if (= (remainder a b) 0) (/ a b) (list '/ a b)))
    ((and (num? a) (= a 0))  0)
    ((and (num? b) (= b 1))  a)
    ((equal? a b)            1)
    ;; (/ (* c u) (** u 2)) -> (/ c u)  handy for log derivatives
    ((and (pair? a) (eq? (car a) '*)
          (pair? b) (eq? (car b) '**)
          (equal? (caddr a) (cadr b))
          (num? (caddr b)))
     (simp/ (cadr a) (simp** (cadr b) (- (caddr b) 1))))
    ;; (/ n (* n u)) -> (/ 1 u)   handy for log(2x) type results
    ((and (num? a) (pair? b) (eq? (car b) '*)
          (num? (cadr b)) (= a (cadr b)))
     (simp/ 1 (caddr b)))
    (else (list '/ a b))))

(define (simp** base exp)
  (cond
    ((and (num? base) (num? exp)) (expt base exp))
    ((and (num? exp) (= exp 0))  1)
    ((and (num? exp) (= exp 1))  base)
    ((and (num? base) (= base 1)) 1)
    (else (list '** base exp))))

;;; -----------------------------------------------------------
;;; Part 4 - the main differentiation procedure
;;; -----------------------------------------------------------
;;; deriv looks up the operator in our table and calls the rule.

(define (deriv e var)
  (cond
    ((num? e)      0)
    ((variable? e) (if (same-var? e var) 1 0))
    ((pair? e)
     (let ((rule (get-rule (op e))))
       (if rule
           (rule e var)
           (error "deriv: don't know how to differentiate" (op e)))))
    (else (error "deriv: bad expression" e))))

;;; -----------------------------------------------------------
;;; Part 5 - differentiation rules, registered into the table
;;; -----------------------------------------------------------
;;; Each rule takes the full expression and the variable.
;;; Adding a new function only means adding one put-rule! here.

;; d/dx (u + v) = u' + v'
(put-rule! '+
  (lambda (e var)
    (make+ (deriv (arg1 e) var)
           (deriv (arg2 e) var))))

;; d/dx (u - v) = u' - v'    and    d/dx (-u) = -u'
(put-rule! '-
  (lambda (e var)
    (if (= (length e) 2)
        (make-neg (deriv (arg1 e) var))
        (make- (deriv (arg1 e) var)
               (deriv (arg2 e) var)))))

;; d/dx (u * v) = u*v' + v*u'   (product rule)
(put-rule! '*
  (lambda (e var)
    (let ((u (arg1 e))
          (v (arg2 e)))
      (make+ (make* u (deriv v var))
             (make* v (deriv u var))))))

;; d/dx (u / v) = (v*u' - u*v') / v^2   (quotient rule)
(put-rule! '/
  (lambda (e var)
    (let ((u (arg1 e))
          (v (arg2 e)))
      (make/ (make- (make* v (deriv u var))
                    (make* u (deriv v var)))
             (make** v 2)))))

;; d/dx (u^n) = n * u^(n-1) * u'   when n is a number
;; d/dx (a^v) = a^v * ln(a) * v'   when a is a number
;; d/dx (u^v) = u^v * (v/u*u' + ln(u)*v')   general case
(put-rule! '**
  (lambda (e var)
    (let ((u (arg1 e))
          (v (arg2 e)))
      (cond
        ((num? v)
         (make* (make* v (make** u (- v 1)))
                (deriv u var)))
        ((num? u)
         (make* (make* e (make-fn 'log u))
                (deriv v var)))
        (else
         (make* e
                (make+ (make* (make/ v u) (deriv u var))
                       (make* (make-fn 'log u) (deriv v var)))))))))

;; -- trig --
;; d/dx sin(u) = cos(u) * u'
(put-rule! 'sin
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make-fn 'cos u)
             (deriv u var)))))

;; d/dx cos(u) = -sin(u) * u'
(put-rule! 'cos
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make-neg (make-fn 'sin u))
             (deriv u var)))))

;; d/dx tan(u) = 1/cos^2(u) * u'
(put-rule! 'tan
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make/ 1 (make** (make-fn 'cos u) 2))
             (deriv u var)))))

;; d/dx cot(u) = -1/sin^2(u) * u'
(put-rule! 'cot
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make-neg (make/ 1 (make** (make-fn 'sin u) 2)))
             (deriv u var)))))

;; d/dx sec(u) = sec(u)*tan(u) * u'
(put-rule! 'sec
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make* e (make-fn 'tan u))
             (deriv u var)))))

;; d/dx csc(u) = -csc(u)*cot(u) * u'
(put-rule! 'csc
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* (make-neg (make* e (make-fn 'cot u)))
             (deriv u var)))))

;; -- exponential and log --
;; d/dx exp(u) = exp(u) * u'
(put-rule! 'exp
  (lambda (e var)
    (let ((u (arg1 e)))
      (make* e (deriv u var)))))

;; d/dx log(u) = u' / u   (natural log)
(put-rule! 'log
  (lambda (e var)
    (let ((u (arg1 e)))
      (make/ (deriv u var) u))))

;; d/dx sqrt(u) = u' / (2 * sqrt(u))
(put-rule! 'sqrt
  (lambda (e var)
    (let ((u (arg1 e)))
      (make/ (deriv u var)
             (make* 2 e)))))

;;; -----------------------------------------------------------
;;; Some small helpers for convenience
;;; -----------------------------------------------------------

;; second derivative
(define (deriv2 e var)
  (deriv (deriv e var) var))

;; show the derivative nicely
(define (show e var)
  (display "d/d") (display var)
  (display " ") (display e)
  (display "  =  ")
  (display (deriv e var))
  (newline))

(display "deriv.scm loaded.  Try: (deriv '(** x 3) 'x)")
(newline)
