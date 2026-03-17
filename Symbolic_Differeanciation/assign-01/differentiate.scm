;;; ============================================================
;;;  differentiate.scm
;;;  Main entry point – (load "differentiate.scm") to start
;;; ============================================================
;;;
;;;  Public API:
;;;    (d  expr var)           – differentiate and simplify
;;;    (d2 expr var)           – second derivative
;;;    (dn expr var n)         – n-th derivative
;;;    (gradient expr vars)    – list of partial derivatives
;;;    (show-d expr var)       – pretty-print d/dvar[expr]
;;;
;;; ============================================================

(load "deriv-core.scm")
(load "simplify.scm")
(load "deriv-rules.scm")

;;; ----------------------------------------------------------
;;; Public interface
;;; ----------------------------------------------------------

(define (d expr var)
  "Differentiate EXPR with respect to VAR; return simplified result."
  (simplify (deriv expr var)))

(define (d2 expr var)
  "Second derivative of EXPR w.r.t. VAR."
  (d (d expr var) var))

(define (dn expr var n)
  "N-th derivative of EXPR w.r.t. VAR."
  (if (= n 0)
      (simplify expr)
      (dn (d expr var) var (- n 1))))

(define (gradient expr vars)
  "List of partial derivatives of EXPR w.r.t. each variable in VARS."
  (map (lambda (v) (d expr v)) vars))

(define (show-d expr var)
  "Pretty-print the derivative."
  (display "  d/d")  (display var)
  (display " [ ")    (display expr) (display " ]") (newline)
  (display "    = ") (display (d expr var))         (newline)
  (newline))

;;; ----------------------------------------------------------
;;; Startup demo
;;; ----------------------------------------------------------

(define (run-demo)
  (define bar (make-string 56 #\─))
  (display bar) (newline)
  (display "  Symbolic Differentiation  –  CTfS Assignment I") (newline)
  (display bar) (newline) (newline)

  (display "── Polynomials ──────────────────────────────────────\n")
  (show-d '(** x 3)                      'x)   ; 3x^2
  (show-d '(+ (* 3 (** x 2)) (* 2 x))   'x)   ; 6x+2
  (show-d '(- (* 5 (** x 4)) (* 3 x))   'x)   ; 20x^3-3

  (display "── Trigonometric ────────────────────────────────────\n")
  (show-d '(sin x)                       'x)   ; cos x
  (show-d '(cos (* 2 x))                 'x)   ; -2*sin(2x)
  (show-d '(tan (** x 2))                'x)   ; 2x/cos^2(x^2)
  (show-d '(* x (sin x))                 'x)   ; x*cos(x)+sin(x)

  (display "── Exponential ──────────────────────────────────────\n")
  (show-d '(exp x)                       'x)   ; exp(x)
  (show-d '(exp (* 3 x))                 'x)   ; 3*exp(3x)
  (show-d '(** 2 x)                      'x)   ; 2^x * log(2)

  (display "── Logarithmic ──────────────────────────────────────\n")
  (show-d '(log x)                       'x)   ; 1/x
  (show-d '(log (** x 3))                'x)   ; 3/x
  (show-d '(log (* 2 x))                 'x)   ; 1/x

  (display "── Quotient / sqrt ──────────────────────────────────\n")
  (show-d '(/ (sin x) x)                 'x)
  (show-d '(sqrt x)                      'x)   ; 1/(2*sqrt x)
  (show-d '(sqrt (** x 3))               'x)

  (display "── Chain rule ───────────────────────────────────────\n")
  (show-d '(sin (exp x))                 'x)
  (show-d '(log (sin x))                 'x)
  (show-d '(exp (** x 3))                'x)

  (display "── Higher derivatives ───────────────────────────────\n")
  (display "  d²/dx²[ sin x ] = ")
  (display (d2 '(sin x) 'x))  (newline)
  (display "  d⁴/dx⁴[ sin x ] = ")
  (display (dn '(sin x) 'x 4)) (newline)
  (display "  d³/dx³[ x^4 ]  = ")
  (display (dn '(** x 4) 'x 3)) (newline) (newline)

  (display "── Gradient ─────────────────────────────────────────\n")
  (display "  ∇(x*y + x^2) w.r.t. [x y] = ")
  (display (gradient '(+ (* x y) (** x 2)) '(x y)))
  (newline) (newline)

  (display bar) (newline)
  (display "  Use (d expr 'var) to differentiate symbolically.") (newline)
  (display "  Use (load \"tests.scm\") to run the test suite.")  (newline)
  (display bar) (newline))

(run-demo)
