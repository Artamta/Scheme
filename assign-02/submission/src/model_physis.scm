;; Assignment II - physics layer

;; used by symbolic expression evaluator
(define (** base power)
  (expt base power))

;; Lennard-Jones potential and symbolic force (Assignment I reused)
(define lj-potential-expr
  '(* 4 (* epsilon
           (- (** (/ sigma r) 12)
              (** (/ sigma r) 6)))))

(define lj-force-expr
  (make-neg (simplify (deriv lj-potential-expr 'r))))

;; vector utilities
(define (zero-vector dim)
  (if (= dim 0)
      '()
      (cons 0.0 (zero-vector (- dim 1)))))

(define (vector-add a b)
  (map + a b))

(define (vector-sub a b)
  (map - a b))

(define (vector-scale scalar v)
  (map (lambda (x) (* scalar x)) v))

(define (vector-norm v)
  (sqrt (apply + (map (lambda (x) (* x x)) v))))

(define (unit-vector v)
  (let ((norm (vector-norm v)))
    (if (<= norm default-min-distance)
        (zero-vector (length v))
        (vector-scale (/ 1.0 norm) v))))

(define (calculate-lj-force-magnitude distance epsilon sigma)
  (if (<= distance default-min-distance)
      0.0
      (eval `(let ((r ,distance) (epsilon ,epsilon) (sigma ,sigma))
               ,lj-force-expr)
            (interaction-environment))))

;; force on i due to j
(define (pair-force-vector pos-i species-i pos-j species-j)
  (let* ((r-ji (vector-sub pos-i pos-j))
         (distance (vector-norm r-ji)))
    (if (<= distance default-min-distance)
        (zero-vector (length pos-i))
        (let* ((epsilon (lj-epsilon species-i species-j))
               (sigma (lj-sigma species-i species-j))
               (force-mag (calculate-lj-force-magnitude distance epsilon sigma))
               (direction (unit-vector r-ji)))
          (vector-scale force-mag direction)))))

(define (force-on-index index positions species-list)
  (let* ((pos-i (list-ref positions index))
         (species-i (list-ref species-list index))
         (dim (length pos-i)))
    (let loop ((j 0)
               (rest-pos positions)
               (rest-species species-list)
               (sum-force (zero-vector dim)))
      (if (null? rest-pos)
          sum-force
          (let ((pos-j (car rest-pos))
                (species-j (car rest-species)))
            (if (= j index)
                (loop (+ j 1)
                      (cdr rest-pos)
                      (cdr rest-species)
                      sum-force)
                (loop (+ j 1)
                      (cdr rest-pos)
                      (cdr rest-species)
                      (vector-add
                       sum-force
                       (pair-force-vector pos-i species-i pos-j species-j)))))))))

(define (all-forces positions species-list)
  (let ((count (length positions)))
    (let loop ((index 0) (result '()))
      (if (= index count)
          (reverse result)
          (loop (+ index 1)
                (cons (force-on-index index positions species-list) result))))))

;; Verlet integration helpers
(define (calculate-acceleration force-vector mass)
  (vector-scale (/ 1.0 mass) force-vector))

(define (next-position r-current r-previous acceleration dt)
  (vector-add
   (vector-sub (vector-scale 2.0 r-current) r-previous)
   (vector-scale (* dt dt) acceleration)))

(define (estimate-velocity r-next r-previous dt)
  (vector-scale (/ 1.0 (* 2.0 dt))
                (vector-sub r-next r-previous)))
