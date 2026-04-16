;;; simplify.scm
;;; Simplifies symbolic arithmetic expressions.
;;; No dependencies - load this first.

(define (simplify e)
  (if (not (pair? e))
      e
      ;; simplify children first, then the top node
      (let ((a (if (>= (length e) 2) (simplify (cadr e)) #f))
            (b (if (>= (length e) 3) (simplify (caddr e)) #f))
            (op (car e)))
        (cond
          ((eq? op '+)  (s+ a b))
          ((eq? op '-)  (if (= (length e) 2) (sneg a) (s- a b)))
          ((eq? op '*)  (s* a b))
          ((eq? op '/)  (s/ a b))
          ((eq? op '**)  (s** a b))
          (else e)))))

(define (num? x) (number? x))

;;; addition
(define (s+ a b)
  (cond
    ((and (num? a) (num? b))  (+ a b))
    ((and (num? a) (= a 0))   b)
    ((and (num? b) (= b 0))   a)
    ((equal? a b)             (s* 2 a))
    (else (list '+ a b))))

;;; subtraction
(define (s- a b)
  (cond
    ((and (num? a) (num? b))  (- a b))
    ((and (num? b) (= b 0))   a)
    ((and (num? a) (= a 0))   (sneg b))
    ((equal? a b)             0)
    (else (list '- a b))))

;;; negation  -a
(define (sneg a)
  (cond
    ((num? a)   (- a))
    ;; -(-x) = x
    ((and (pair? a) (eq? (car a) '-) (= (length a) 2))  (cadr a))
    (else (list '- a))))

;;; multiplication
(define (s* a b)
  (cond
    ((and (num? a) (num? b))  (* a b))
    ((and (num? a) (= a 0))   0)
    ((and (num? b) (= b 0))   0)
    ((and (num? a) (= a 1))   b)
    ((and (num? b) (= b 1))   a)
    ((and (num? a) (= a -1))  (sneg b))
    ((and (num? b) (= b -1))  (sneg a))
    ;; n * (n2 * u) -> (n*n2) * u   e.g. (* 3 (* 2 x)) -> (* 6 x)
    ((and (num? a) (pair? b) (eq? (car b) '*) (num? (cadr b)))
     (s* (* a (cadr b)) (caddr b)))
    ;; always write number first:  x*3 -> 3*x
    ((and (num? b) (not (num? a)))  (list '* b a))
    (else (list '* a b))))

;;; division
(define (s/ a b)
  (cond
    ((and (num? a) (num? b))  (/ a b))
    ((and (num? a) (= a 0))   0)
    ((and (num? b) (= b 1))   a)
    ((equal? a b)             1)
    ;; (2x / x^2) -> (2 / x)  -- needed for log(x^2) derivative
    ((and (pair? a) (eq? (car a) '*)
          (pair? b) (eq? (car b) '**)
          (equal? (caddr a) (cadr b))
          (num? (caddr b)))
     (s/ (cadr a) (s** (cadr b) (- (caddr b) 1))))
    ;; (2 / 2x) -> (1 / x)  -- needed for log(2x) derivative
    ((and (num? a)
          (pair? b) (eq? (car b) '*)
          (num? (cadr b))
          (= a (cadr b)))
     (s/ 1 (caddr b)))
    (else (list '/ a b))))

;;; exponentiation
(define (s** base exp)
  (cond
    ((and (num? base) (num? exp))  (expt base exp))
    ((and (num? exp) (= exp 0))    1)
    ((and (num? exp) (= exp 1))    base)
    (else (list '** base exp))))
