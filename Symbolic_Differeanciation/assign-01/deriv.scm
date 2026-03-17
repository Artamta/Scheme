;;; deriv.scm
;;; Symbolic differentiation - data-directed approach.
;;; Depends on simplify.scm - load that first.
;;;
;;; Usage:
;;;   (load "simplify.scm")
;;;   (load "deriv.scm")
;;;   (deriv '(** x 3) 'x)    =>  (* 3 (** x 2))

(load "simplify.scm")

;;; -------------------------------------------------------
;;; The dispatch table
;;; -------------------------------------------------------
;;; Maps operator symbols to differentiation procedures.
;;; This is the data-directed part: adding a new function
;;; means only adding one (put-rule! ...) call below.

(define *table* '())

(define (put-rule! op proc)
  (set! *table* (cons (cons op proc) *table*)))

(define (get-rule op)
  (let ((found (assq op *table*)))
    (if found (cdr found) #f)))

;;; -------------------------------------------------------
;;; Expression helpers
;;; -------------------------------------------------------

(define (variable? e)    (symbol? e))
(define (same-var? u v)  (and (variable? u) (variable? v) (eq? u v)))

;;; Constructors call simplify so results stay tidy
(define (make+ a b)   (s+ a b))
(define (make- a b)   (s- a b))
(define (make-neg a)  (sneg a))
(define (make* a b)   (s* a b))
(define (make/ a b)   (s/ a b))
(define (make** b e)  (s** b e))
(define (make-fn f u) (list f u))   ; e.g. (sin x)

;;; -------------------------------------------------------
;;; The differentiator
;;; -------------------------------------------------------

(define (deriv e var)
  (cond
    ((number? e)   0)
    ((variable? e) (if (same-var? e var) 1 0))
    ((pair? e)
     (let ((rule (get-rule (car e))))
       (if rule
           (rule e var)
           (error "deriv: unknown operator" (car e)))))
    (else (error "deriv: bad expression" e))))

;;; -------------------------------------------------------
;;; Differentiation rules
;;; -------------------------------------------------------

;;; d/dx (u + v) = u' + v'
(put-rule! '+
  (lambda (e var)
    (make+ (deriv (cadr e) var)
           (deriv (caddr e) var))))

;;; d/dx (u - v) = u' - v'   and   d/dx (-u) = -(u')
(put-rule! '-
  (lambda (e var)
    (if (= (length e) 2)
        (make-neg (deriv (cadr e) var))
        (make- (deriv (cadr e) var)
               (deriv (caddr e) var)))))

;;; d/dx (u * v) = u*v' + v*u'
(put-rule! '*
  (lambda (e var)
    (make+ (make* (cadr e)  (deriv (caddr e) var))
           (make* (caddr e) (deriv (cadr e)  var)))))

;;; d/dx (u / v) = (v*u' - u*v') / v^2
(put-rule! '/
  (lambda (e var)
    (let ((u (cadr e)) (v (caddr e)))
      (make/ (make- (make* v (deriv u var))
                    (make* u (deriv v var)))
             (make** v 2)))))

;;; d/dx (u^n) = n * u^(n-1) * u'           [n is a number]
;;; d/dx (a^v) = a^v * ln(a) * v'           [a is a number]
;;; d/dx (u^v) = u^v * (v/u*u' + ln(u)*v') [both symbolic]
(put-rule! '**
  (lambda (e var)
    (let ((u (cadr e)) (v (caddr e)))
      (cond
        ((number? v)
         (make* (make* v (make** u (- v 1)))
                (deriv u var)))
        ((number? u)
         (make* (make* e (make-fn 'log u))
                (deriv v var)))
        (else
         (make* e
                (make+ (make* (make/ v u) (deriv u var))
                       (make* (make-fn 'log u) (deriv v var)))))))))

;;; --- Trigonometric ---

;;; d/dx sin(u) = cos(u) * u'
(put-rule! 'sin
  (lambda (e var)
    (make* (make-fn 'cos (cadr e))
           (deriv (cadr e) var))))

;;; d/dx cos(u) = -sin(u) * u'
(put-rule! 'cos
  (lambda (e var)
    (make* (make-neg (make-fn 'sin (cadr e)))
           (deriv (cadr e) var))))

;;; d/dx tan(u) = 1/cos^2(u) * u'
(put-rule! 'tan
  (lambda (e var)
    (make* (make/ 1 (make** (make-fn 'cos (cadr e)) 2))
           (deriv (cadr e) var))))

;;; d/dx cot(u) = -1/sin^2(u) * u'
(put-rule! 'cot
  (lambda (e var)
    (make* (make-neg (make/ 1 (make** (make-fn 'sin (cadr e)) 2)))
           (deriv (cadr e) var))))

;;; d/dx sec(u) = sec(u)*tan(u) * u'
(put-rule! 'sec
  (lambda (e var)
    (make* (make* e (make-fn 'tan (cadr e)))
           (deriv (cadr e) var))))

;;; d/dx csc(u) = -csc(u)*cot(u) * u'
(put-rule! 'csc
  (lambda (e var)
    (make* (make-neg (make* e (make-fn 'cot (cadr e))))
           (deriv (cadr e) var))))

;;; --- Exponential and Logarithmic ---

;;; d/dx exp(u) = exp(u) * u'
(put-rule! 'exp
  (lambda (e var)
    (make* e (deriv (cadr e) var))))

;;; d/dx log(u) = u' / u     (natural log)
(put-rule! 'log
  (lambda (e var)
    (make/ (deriv (cadr e) var) (cadr e))))

;;; d/dx sqrt(u) = u' / (2 * sqrt(u))
(put-rule! 'sqrt
  (lambda (e var)
    (make/ (deriv (cadr e) var)
           (make* 2 e))))

;;; -------------------------------------------------------
;;; Convenience
;;; -------------------------------------------------------

(define (deriv2 e var)
  (deriv (deriv e var) var))

(define (show e var)
  (display "d/d") (display var) (display " ")
  (display e) (display "  =  ")
  (display (deriv e var)) (newline))

(display "Loaded. Try: (deriv '(** x 3) 'x)") (newline)
