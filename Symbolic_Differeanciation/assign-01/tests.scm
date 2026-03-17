;;; tests.scm
;;; Run: guile -l deriv.scm -l tests.scm

(define ok 0)
(define bad 0)

(define (check label expected got)
  (if (equal? expected got)
      (begin (set! ok  (+ ok  1))
             (display "PASS  ") (display label) (newline))
      (begin (set! bad (+ bad 1))
             (display "FAIL  ") (display label) (newline)
             (display "  want: ") (display expected) (newline)
             (display "  got:  ") (display got)      (newline))))

;;; --- simplifier ---
(display "\n-- simplifier --\n")
(check "0+x=x"        'x  (simplify '(+ 0 x)))
(check "x+0=x"        'x  (simplify '(+ x 0)))
(check "x*0=0"        0   (simplify '(* x 0)))
(check "x*1=x"        'x  (simplify '(* x 1)))
(check "x^0=1"        1   (simplify '(** x 0)))
(check "x^1=x"        'x  (simplify '(** x 1)))
(check "x-x=0"        0   (simplify '(- x x)))
(check "x/x=1"        1   (simplify '(/ x x)))
(check "-(-x)=x"      'x  (simplify '(- (- x))))
(check "2+3=5"        5   (simplify '(+ 2 3)))
(check "2*3=6"        6   (simplify '(* 2 3)))

;;; --- constants and variables ---
(display "\n-- constants and variables --\n")
(check "d/dx 5 = 0"   0  (deriv 5 'x))
(check "d/dx x = 1"   1  (deriv 'x 'x))
(check "d/dx y = 0"   0  (deriv 'y 'x))

;;; --- polynomials ---
(display "\n-- polynomials --\n")
(check "d/dx x^2"     '(* 2 x)          (deriv '(** x 2) 'x))
(check "d/dx x^3"     '(* 3 (** x 2))   (deriv '(** x 3) 'x))
(check "d/dx x^4"     '(* 4 (** x 3))   (deriv '(** x 4) 'x))
(check "d/dx 5x"      5                  (deriv '(* 5 x) 'x))
(check "d/dx 3x^2"    '(* 6 x)          (deriv '(* 3 (** x 2)) 'x))
(check "d/dx x+1"     1                  (deriv '(+ x 1) 'x))
(check "d/dx x-x"     0                  (deriv '(- x x) 'x))

;;; --- product and quotient ---
(display "\n-- product and quotient --\n")
(check "d/dx x*x"     '(* 2 x)           (deriv '(* x x) 'x))
(check "d/dx 1/x"     '(/ -1 (** x 2))   (deriv '(/ 1 x) 'x))
(check "d/dx x*sinx"  '(+ (* x (cos x)) (sin x))
                                          (deriv '(* x (sin x)) 'x))

;;; --- trig ---
(display "\n-- trig --\n")
(check "d/dx sin x"   '(cos x)               (deriv '(sin x) 'x))
(check "d/dx cos x"   '(- (sin x))           (deriv '(cos x) 'x))
(check "d/dx tan x"   '(/ 1 (** (cos x) 2)) (deriv '(tan x) 'x))
(check "d/dx sin 2x"  '(* 2 (cos (* 2 x)))  (deriv '(sin (* 2 x)) 'x))

;;; --- exp and log ---
(display "\n-- exp and log --\n")
(check "d/dx exp x"    '(exp x)              (deriv '(exp x) 'x))
(check "d/dx exp 3x"   '(* 3 (exp (* 3 x))) (deriv '(exp (* 3 x)) 'x))
(check "d/dx log x"    '(/ 1 x)              (deriv '(log x) 'x))
(check "d/dx log x^2"  '(/ 2 x)              (deriv '(log (** x 2)) 'x))
(check "d/dx log 2x"   '(/ 1 x)              (deriv '(log (* 2 x)) 'x))

;;; --- sqrt ---
(display "\n-- sqrt --\n")
(check "d/dx sqrt x"  '(/ 1 (* 2 (sqrt x)))  (deriv '(sqrt x) 'x))

;;; --- chain rule ---
(display "\n-- chain rule --\n")
(check "d/dx sin(exp x)"  '(* (cos (exp x)) (exp x))
                           (deriv '(sin (exp x)) 'x))
(check "d/dx log(sin x)"  '(/ (cos x) (sin x))
                           (deriv '(log (sin x)) 'x))

;;; --- second derivative ---
(display "\n-- second derivative --\n")
(check "d2/dx2 sin x"   '(- (sin x))   (deriv2 '(sin x) 'x))
(check "d2/dx2 x^3"     '(* 6 x)       (deriv2 '(** x 3) 'x))

;;; --- summary ---
(newline)
(display "Passed: ") (display ok)  (newline)
(display "Failed: ") (display bad) (newline)
