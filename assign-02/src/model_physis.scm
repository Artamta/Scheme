;; Map the symbolic ** to the actual exponentiation function
;;(define (** base power) (expt base power))

(define (** base power) (expt base power))
(define lj-potential-expr
  '(* 4 (* epsilon (- (** (/ sigma r) 12) 
                      (** (/ sigma r) 6)))))

;; Generate the symbolic expression
(define lj-force-expr 
  (make-neg (simplify (deriv lj-potential-expr 'r))))

(define (calculate-force r-val epsilon-val sigma-val)
  (eval `(let ((r ,r-val)
               (epsilon ,epsilon-val)
               (sigma ,sigma-val))
           ,lj-force-expr) 
        (interaction-environment)))


