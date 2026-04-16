;; 1. The Power Function (needed for eval)
(define (** base power) (expt base power))

;; 2. Symbolic Equations
(define lj-potential-expr
  '(* 4 (* epsilon (- (** (/ sigma r) 12) 
                      (** (/ sigma r) 6)))))

(define lj-force-expr 
  (make-neg (simplify (deriv lj-potential-expr 'r))))

;; 3. Interaction Logic
(define (calculate-force r-val epsilon-val sigma-val)
  (eval `(let ((r ,r-val) (epsilon ,epsilon-val) (sigma ,sigma-val))
           ,lj-force-expr) 
        (interaction-environment)))

(define (total-force-on-i current-pos all-positions)
  (let ((other-positions (filter (lambda (p) (not (= p current-pos))) 
                                 all-positions)))
    (apply + (map (lambda (neighbor-pos)
                    (calculate-force (abs (- current-pos neighbor-pos)) epsilon sigma))
                  other-positions))))

;; 4. Numerical Dynamics (Verlet)
(define (calculate-acceleration force mass)
  (/ force mass))

(define (next-position r-current r-previous acceleration dt)
  (+ (- (* 2 r-current) r-previous)
     (* acceleration (expt dt 2))))