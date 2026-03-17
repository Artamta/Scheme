;;; tests.scm
;;; Run these after loading deriv.scm
;;; Usage:  guile -l deriv.scm -l tests.scm

(define pass 0)
(define fail 0)

(define (check msg expected got)
  (if (equal? expected got)
      (begin
        (set! pass (+ pass 1))
        (display "PASS  ") (display msg) (newline))
      (begin
        (set! fail (+ fail 1))
        (display "FAIL  ") (display msg) (newline)
        (display "      wanted: ") (display expected) (newline)
        (display "      got:    ") (display got)      (newline))))

(display "\n=== Simplifier ===\n")
(check "0+x"         'x          (simplify '(+ 0 x)))
(check "x+0"         'x          (simplify '(+ x 0)))
(check "x*0"         0           (simplify '(* x 0)))
(check "x*1"         'x          (simplify '(* x 1)))
(check "1*x"         'x          (simplify '(* 1 x)))
(check "x^0"         1           (simplify '(** x 0)))
(check "x^1"         'x          (simplify '(** x 1)))
(check "x-x"         0           (simplify '(- x x)))
(check "x/x"         1           (simplify '(/ x x)))
(check "2+3"         5           (simplify '(+ 2 3)))
(check "2*3"         6           (simplify '(* 2 3)))
(check "-(-x)"       'x          (simplify '(- (- x))))

(display "\n=== Constants and variables ===\n")
(check "d/dx 5"      0           (deriv 5 'x))
(check "d/dx x"      1           (deriv 'x 'x))
(check "d/dx y"      0           (deriv 'y 'x))

(display "\n=== Polynomials ===\n")
(check "d/dx x^2"    '(* 2 x)           (deriv '(** x 2) 'x))
(check "d/dx x^3"    '(* 3 (** x 2))    (deriv '(** x 3) 'x))
(check "d/dx x^4"    '(* 4 (** x 3))    (deriv '(** x 4) 'x))
(check "d/dx 5x"     5                  (deriv '(* 5 x) 'x))
(check "d/dx 3x^2"   '(* 6 x)           (deriv '(* 3 (** x 2)) 'x))
(check "d/dx x+x"    2                  (deriv '(+ x x) 'x))
(check "d/dx x-x"    0                  (deriv '(- x x) 'x))

(display "\n=== Product and quotient ===\n")
(check "d/dx x*x"    '(* 2 x)           (deriv '(* x x) 'x))
(check "d/dx 1/x"    '(/ -1 (** x 2))   (deriv '(/ 1 x) 'x))

(display "\n=== Trig ===\n")
(check "d/dx sin x"  '(cos x)            (deriv '(sin x) 'x))
(check "d/dx cos x"  '(- (sin x))        (deriv '(cos x) 'x))
(check "d/dx tan x"  '(/ 1 (** (cos x) 2))  (deriv '(tan x) 'x))
(check "d/dx sin 2x" '(* 2 (cos (* 2 x)))   (deriv '(sin (* 2 x)) 'x))

(display "\n=== Exp and log ===\n")
(check "d/dx exp x"  '(exp x)            (deriv '(exp x) 'x))
(check "d/dx exp 3x" '(* 3 (exp (* 3 x)))  (deriv '(exp (* 3 x)) 'x))
(check "d/dx log x"  '(/ 1 x)            (deriv '(log x) 'x))
(check "d/dx log x^2" '(/ 2 x)           (deriv '(log (** x 2)) 'x))
(check "d/dx log 2x"  '(/ 1 x)           (deriv '(log (* 2 x)) 'x))

(display "\n=== sqrt ===\n")
(check "d/dx sqrt x" '(/ 1 (* 2 (sqrt x)))  (deriv '(sqrt x) 'x))

(display "\n=== Chain rule ===\n")
(check "d/dx sin(exp x)"  '(* (cos (exp x)) (exp x))
       (deriv '(sin (exp x)) 'x))
(check "d/dx log(sin x)"  '(/ (cos x) (sin x))
       (deriv '(log (sin x)) 'x))

(display "\n=== Second derivative ===\n")
(check "d2/dx2 sin x"  '(- (sin x))    (deriv2 '(sin x) 'x))
(check "d2/dx2 x^3"    '(* 6 x)        (deriv2 '(** x 3) 'x))

(newline)
(display "Passed: ") (display pass) (newline)
(display "Failed: ") (display fail) (newline)
