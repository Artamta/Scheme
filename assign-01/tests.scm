;;; ============================================================
;;;  tests.scm  –  Automated test suite
;;;  Run:  guile -l differentiate.scm -l tests.scm
;;; ============================================================

(define *tests-run*    0)
(define *tests-passed* 0)
(define *tests-failed* 0)

(define (check-equal label expected got)
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected got)
      (begin (set! *tests-passed* (+ *tests-passed* 1))
             (display "  PASS  ") (display label) (newline))
      (begin (set! *tests-failed* (+ *tests-failed* 1))
             (display "  FAIL  ") (display label) (newline)
             (display "        expected: ") (display expected) (newline)
             (display "        got:      ") (display got)      (newline))))

;;; ============================================================
;;; Simplifier unit tests
;;; ============================================================
(define (test-simplifier)
  (display "\n── Simplifier ──────────────────────────────────────\n")

  ;; identity laws
  (check-equal "0+x = x"           'x        (simplify '(+ 0 x)))
  (check-equal "x+0 = x"           'x        (simplify '(+ x 0)))
  (check-equal "x*1 = x"           'x        (simplify '(* x 1)))
  (check-equal "1*x = x"           'x        (simplify '(* 1 x)))
  (check-equal "x*0 = 0"           0         (simplify '(* x 0)))
  (check-equal "0*x = 0"           0         (simplify '(* 0 x)))
  (check-equal "x^0 = 1"           1         (simplify '(** x 0)))
  (check-equal "x^1 = x"           'x        (simplify '(** x 1)))
  (check-equal "x/1 = x"           'x        (simplify '(/ x 1)))
  (check-equal "0/x = 0"           0         (simplify '(/ 0 x)))

  ;; numeric folding
  (check-equal "2+3 = 5"           5         (simplify '(+ 2 3)))
  (check-equal "2*3 = 6"           6         (simplify '(* 2 3)))
  (check-equal "2^3 = 8"           8         (simplify '(** 2 3)))
  (check-equal "5-3 = 2"           2         (simplify '(- 5 3)))

  ;; cancellation
  (check-equal "x-x = 0"           0         (simplify '(- x x)))
  (check-equal "x/x = 1"           1         (simplify '(/ x x)))
  (check-equal "-(-x) = x"         'x        (simplify '(- (- x))))

  ;; canonical numeric-first ordering: u*n → n*u
  (check-equal "x*3 → 3*x"         '(* 3 x)  (simplify '(* x 3)))

  ;; quotient cancellation
  (check-equal "(2x)/(x^2) = 2/x"
    '(/ 2 x)   (simplify '(/ (* 2 x) (** x 2))))
  (check-equal "2/(2x) = 1/x"
    '(/ 1 x)   (simplify '(/ 2 (* 2 x)))))

;;; ============================================================
;;; Differentiation tests
;;; ============================================================
(define (test-deriv)

  ;;── Base cases ─────────────────────────────────────────────
  (display "\n── Constants & variables ───────────────────────────\n")
  (check-equal "d/dx[5]   = 0"    0    (d 5 'x))
  (check-equal "d/dx[x]   = 1"    1    (d 'x 'x))
  (check-equal "d/dx[y]   = 0"    0    (d 'y 'x))

  ;;── Sum / difference ───────────────────────────────────────
  (display "\n── Sum / difference ────────────────────────────────\n")
  (check-equal "d/dx[x+3]    = 1"       1   (d '(+ x 3) 'x))
  (check-equal "d/dx[x+y]    = 1"       1   (d '(+ x y) 'x))
  (check-equal "d/dx[2x+3x]  = 5"       5   (d '(+ (* 2 x) (* 3 x)) 'x))
  (check-equal "d/dx[x-x]    = 0"       0   (d '(- x x) 'x))
  (check-equal "d/dx[-x]     = -1"     -1   (d '(- x) 'x))

  ;;── Product / quotient ─────────────────────────────────────
  (display "\n── Product / quotient ──────────────────────────────\n")
  (check-equal "d/dx[3x]     = 3"       3          (d '(* 3 x) 'x))
  (check-equal "d/dx[x*x]    = 2x"      '(* 2 x)   (d '(* x x) 'x))

  ;; product rule: u=x, v=sin(x)
  ;;   → x*(cos x)*1 + (sin x)*1  =  (+ (* x (cos x)) (sin x))
  (check-equal "d/dx[x*sin x]"
    '(+ (* x (cos x)) (sin x))   (d '(* x (sin x)) 'x))

  ;; d/dx[1/x] = (x*0 - 1*1)/x^2 = -1/x^2
  (check-equal "d/dx[1/x]    = -1/x^2"
    '(/ -1 (** x 2))              (d '(/ 1 x) 'x))

  ;;── Power / polynomial ─────────────────────────────────────
  (display "\n── Power / polynomial ──────────────────────────────\n")
  (check-equal "d/dx[x^0]    = 0"       0                  (d '(** x 0) 'x))
  (check-equal "d/dx[x^1]    = 1"       1                  (d '(** x 1) 'x))
  (check-equal "d/dx[x^2]    = 2x"      '(* 2 x)           (d '(** x 2) 'x))
  (check-equal "d/dx[x^3]    = 3x^2"    '(* 3 (** x 2))    (d '(** x 3) 'x))
  (check-equal "d/dx[x^4]    = 4x^3"    '(* 4 (** x 3))    (d '(** x 4) 'x))
  (check-equal "d/dx[5x^3]   = 15x^2"   '(* 15 (** x 2))   (d '(* 5 (** x 3)) 'x))

  ;;── Trigonometric ──────────────────────────────────────────
  (display "\n── Trigonometric ───────────────────────────────────\n")
  (check-equal "d/dx[sin x]  = cos x"      '(cos x)          (d '(sin x) 'x))
  (check-equal "d/dx[cos x]  = -(sin x)"   '(- (sin x))      (d '(cos x) 'x))
  (check-equal "d/dx[tan x]  = 1/cos²x"
    '(/ 1 (** (cos x) 2))                                     (d '(tan x) 'x))

  ;; sin(2x): cos(2x)*2  →  canonical (* 2 (cos (* 2 x)))
  (check-equal "d/dx[sin(2x)] = 2*cos(2x)"
    '(* 2 (cos (* 2 x)))                                      (d '(sin (* 2 x)) 'x))

  ;; cos(x^2): -(sin(x^2)) * (2x)   (negation term comes first)
  (check-equal "d/dx[cos(x^2)]"
    '(* (- (sin (** x 2))) (* 2 x))                          (d '(cos (** x 2)) 'x))

  ;;── Exponential ────────────────────────────────────────────
  (display "\n── Exponential ─────────────────────────────────────\n")
  (check-equal "d/dx[exp x]     = exp(x)"
    '(exp x)                                                  (d '(exp x) 'x))
  ;; exp(3x): exp(3x)*3  → canonical (* 3 (exp (* 3 x)))
  (check-equal "d/dx[exp(3x)]   = 3*exp(3x)"
    '(* 3 (exp (* 3 x)))                                      (d '(exp (* 3 x)) 'x))
  ;; exp(x^2): exp(x^2)*(2x)
  (check-equal "d/dx[exp(x^2)]"
    '(* (exp (** x 2)) (* 2 x))                              (d '(exp (** x 2)) 'x))

  ;;── Logarithmic ────────────────────────────────────────────
  (display "\n── Logarithmic ─────────────────────────────────────\n")
  (check-equal "d/dx[log x]     = 1/x"    '(/ 1 x)           (d '(log x) 'x))
  ;; log(x^2) → (2x)/x^2 → simplifier → 2/x
  (check-equal "d/dx[log(x^2)]  = 2/x"   '(/ 2 x)           (d '(log (** x 2)) 'x))
  ;; log(2x)  → 2/(2x)   → simplifier → 1/x
  (check-equal "d/dx[log(2x)]   = 1/x"   '(/ 1 x)           (d '(log (* 2 x)) 'x))

  ;;── sqrt ───────────────────────────────────────────────────
  (display "\n── sqrt ────────────────────────────────────────────\n")
  (check-equal "d/dx[sqrt x]    = 1/(2*sqrt x)"
    '(/ 1 (* 2 (sqrt x)))                                     (d '(sqrt x) 'x))

  ;;── Chain rule ─────────────────────────────────────────────
  (display "\n── Chain rule ──────────────────────────────────────\n")
  ;; sin(exp x): cos(exp x) * exp(x)
  (check-equal "d/dx[sin(exp x)]"
    '(* (cos (exp x)) (exp x))                               (d '(sin (exp x)) 'x))
  ;; log(sin x): cos(x)/sin(x)
  (check-equal "d/dx[log(sin x)] = cos(x)/sin(x)"
    '(/ (cos x) (sin x))                                      (d '(log (sin x)) 'x))

  ;;── Unary minus ────────────────────────────────────────────
  (display "\n── Unary minus ─────────────────────────────────────\n")
  ;; d/dx[-(x^2)] = -(2x)
  (check-equal "d/dx[-(x^2)] = -(2x)"
    '(- (* 2 x))                                              (d '(- (** x 2)) 'x))

  ;;── Higher derivatives ─────────────────────────────────────
  (display "\n── Higher derivatives ──────────────────────────────\n")
  (check-equal "d²/dx²[x^3]   = 6x"    '(* 6 x)     (d2 '(** x 3) 'x))
  (check-equal "d²/dx²[sin x] = -sin x" '(- (sin x)) (d2 '(sin x) 'x))
  (check-equal "d⁴/dx⁴[sin x] = sin x"  '(sin x)     (dn '(sin x) 'x 4))
  (check-equal "d³/dx³[x^4]  = 24x"    '(* 24 x)    (dn '(** x 4) 'x 3)))

;;; ============================================================
;;; Run everything
;;; ============================================================
(define (test-suite)
  (display "\n╔══════════════════════════════════════════════════════╗\n")
  (display   "║   Test Suite: Symbolic Differentiation System        ║\n")
  (display   "╚══════════════════════════════════════════════════════╝\n")
  (test-simplifier)
  (test-deriv)
  (newline)
  (display "══════════════════════════════════════════════════════\n")
  (display "  Tests run:    ") (display *tests-run*)    (newline)
  (display "  Tests passed: ") (display *tests-passed*) (newline)
  (display "  Tests failed: ") (display *tests-failed*) (newline)
  (if (= *tests-failed* 0)
      (display "  ALL TESTS PASSED\n")
      (display "  SOME TESTS FAILED\n"))
  (display "══════════════════════════════════════════════════════\n"))

(test-suite)
