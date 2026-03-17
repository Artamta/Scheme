;;; ============================================================
;;;  simplify.scm  –  Algebraic simplifier
;;; ============================================================

;;; ----------------------------------------------------------
;;; Helper predicates
;;; ----------------------------------------------------------

(define (=number? expr n) (and (number? expr) (= expr n)))
(define (numeric-args? a b) (and (number? a) (number? b)))

;;; ----------------------------------------------------------
;;; Top-level simplifier  (bottom-up, one pass)
;;; ----------------------------------------------------------

(define (simplify expr)
  (cond
    ((not (pair? expr)) expr)
    (else
     (let* ((op   (car expr))
            (args (map simplify (cdr expr))))
       (case op
         ((+)   (apply simplify-sum        args))
         ((-)   (if (= (length args) 1)
                    (simplify-negation (car args))
                    (apply simplify-difference args)))
         ((*)   (apply simplify-product    args))
         ((/)   (apply simplify-quotient   args))
         ((**)  (apply simplify-power      args))
         (else  (cons op args)))))))

;;; ----------------------------------------------------------
;;; Addition
;;; ----------------------------------------------------------

(define (simplify-sum a b)
  (cond
    ((numeric-args? a b)       (+ a b))
    ((=number? a 0)            b)
    ((=number? b 0)            a)
    ((equal? a b)              (simplify-product 2 a))
    ;; (n*u)+u → (n+1)*u
    ((and (pair? a) (eq? (car a) '*)
          (number? (cadr a)) (equal? (caddr a) b))
     (simplify-product (+ (cadr a) 1) b))
    ;; u+(n*u) → (n+1)*u
    ((and (pair? b) (eq? (car b) '*)
          (number? (cadr b)) (equal? (caddr b) a))
     (simplify-product (+ (cadr b) 1) a))
    (else (list '+ a b))))

;;; ----------------------------------------------------------
;;; Subtraction
;;; ----------------------------------------------------------

(define (simplify-difference a b)
  (cond
    ((numeric-args? a b) (- a b))
    ((=number? b 0)      a)
    ((=number? a 0)      (simplify-negation b))
    ((equal? a b)        0)
    (else (list '- a b))))

;;; ----------------------------------------------------------
;;; Negation  (unary minus)
;;; ----------------------------------------------------------

(define (simplify-negation a)
  (cond
    ((number? a)                        (- a))
    ;; -(-u) → u
    ((and (pair? a) (eq? (car a) '-) (= (length a) 2))
     (cadr a))
    ;; -(a-b) → b-a
    ((and (pair? a) (eq? (car a) '-) (= (length a) 3))
     (simplify-difference (caddr a) (cadr a)))
    (else (list '- a))))

;;; ----------------------------------------------------------
;;; Multiplication
;;; ----------------------------------------------------------

(define (simplify-product a b)
  (cond
    ((numeric-args? a b)  (* a b))
    ((=number? a 0)       0)
    ((=number? b 0)       0)
    ((=number? a 1)       b)
    ((=number? b 1)       a)
    ((=number? a -1)      (simplify-negation b))
    ((=number? b -1)      (simplify-negation a))
    ;; canonical: put numeric factor first  u*n → n*u
    ((and (number? b) (not (number? a)))
     (list '* b a))
    ;; u * u → u^2
    ((equal? a b)         (simplify-power a 2))
    ;; u^m * u^n → u^(m+n)
    ((and (pair? a) (eq? (car a) '**)
          (pair? b) (eq? (car b) '**)
          (equal? (cadr a) (cadr b))
          (number? (caddr a)) (number? (caddr b)))
     (simplify-power (cadr a) (+ (caddr a) (caddr b))))
    ;; u^n * u → u^(n+1)
    ((and (pair? a) (eq? (car a) '**) (equal? (cadr a) b) (number? (caddr a)))
     (simplify-power b (+ (caddr a) 1)))
    ;; u * u^n → u^(n+1)
    ((and (pair? b) (eq? (car b) '**) (equal? (cadr b) a) (number? (caddr b)))
     (simplify-power a (+ (caddr b) 1)))
    ;; constant folding: (n1 * e) * n2 → (n1*n2) * e
    ((and (pair? a) (eq? (car a) '*) (number? (cadr a)) (number? b))
     (simplify-product (* (cadr a) b) (caddr a)))
    ;; n1 * (n2 * e) → (n1*n2) * e
    ((and (pair? b) (eq? (car b) '*) (number? (cadr b)) (number? a))
     (simplify-product (* a (cadr b)) (caddr b)))
    (else (list '* a b))))

;;; ----------------------------------------------------------
;;; Division  (with algebraic cancellation)
;;; ----------------------------------------------------------

(define (simplify-quotient a b)
  (cond
    ((=number? b 0)       (error "simplify: division by zero"))
    ((numeric-args? a b)  (let ((r (/ a b)))
                            (if (integer? r) r (list '/ a b))))
    ((=number? a 0)       0)
    ((=number? b 1)       a)
    ((equal? a b)         1)

    ;; (/ (* n u) (** u 2)) → (/ n u)   e.g. (/ (* 2 x) (** x 2)) → (/ 2 x)
    ((and (pair? a) (eq? (car a) '*)
          (pair? b) (eq? (car b) '**)
          (equal? (caddr a) (cadr b))
          (number? (caddr b)))
     (simplify-quotient (cadr a)
                        (simplify-power (cadr b) (- (caddr b) 1))))

    ;; (/ (* u n) (** u 2)) → (/ n u)   e.g. numerator has sym first
    ((and (pair? a) (eq? (car a) '*)
          (pair? b) (eq? (car b) '**)
          (equal? (cadr a) (cadr b))
          (number? (caddr b)))
     (simplify-quotient (caddr a)
                        (simplify-power (cadr b) (- (caddr b) 1))))

    ;; (/ n (* n u)) → (/ 1 u)     e.g. (/ 2 (* 2 x)) → (/ 1 x)
    ((and (number? a)
          (pair? b) (eq? (car b) '*)
          (number? (cadr b))
          (= a (cadr b)))
     (simplify-quotient 1 (caddr b)))

    ;; (/ (* n u) (* n v)) → (/ u v)
    ((and (pair? a) (eq? (car a) '*)
          (pair? b) (eq? (car b) '*)
          (number? (cadr a)) (number? (cadr b))
          (= (cadr a) (cadr b)))
     (simplify-quotient (caddr a) (caddr b)))

    ;; (/ (* c u) u) → c    e.g. (/ (* 2 x) x) → 2
    ((and (pair? a) (eq? (car a) '*) (equal? (caddr a) b))
     (cadr a))
    ((and (pair? a) (eq? (car a) '*) (equal? (cadr a) b))
     (caddr a))

    ;; (/ u/v) / w  → u/(v*w)
    ((and (pair? a) (eq? (car a) '/))
     (simplify-quotient (cadr a)
                        (simplify-product (caddr a) b)))
    (else (list '/ a b))))

;;; ----------------------------------------------------------
;;; Exponentiation
;;; ----------------------------------------------------------

(define (simplify-power base exp)
  (cond
    ((and (number? base) (number? exp)) (expt base exp))
    ((=number? exp 0)   1)
    ((=number? exp 1)   base)
    ((=number? base 0)  0)
    ((=number? base 1)  1)
    ;; (u^m)^n → u^(m*n)
    ((and (pair? base) (eq? (car base) '**)
          (number? (caddr base)) (number? exp))
     (simplify-power (cadr base) (* (caddr base) exp)))
    (else (list '** base exp))))
