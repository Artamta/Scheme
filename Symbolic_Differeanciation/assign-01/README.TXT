
                                            Assignment I - Symbolic differentiation
-----------
Ayush Raj
20221068
----------
CTfS

Files
------
  simplify.scm   algebraic simplifier (no dependencies)
  deriv.scm      dispatch table + deriv procedure + all rules
                 (loads simplify.scm automatically)
  tests.scm      test cases
  README.TXT     this file

How to run
-----------
  $ guile
  > (load "deriv.scm")
  > (deriv '(** x 3) 'x)
  (* 3 (** x 2))

To run tests:
  $ guile -l deriv.scm -l tests.scm

Sample interactions
--------------------
  (deriv '(** x 4) 'x)
  => (* 4 (** x 3))

  (deriv '(+ (* 3 (** x 2)) (* 2 x)) 'x)
  => (+ (* 6 x) 2)

  (deriv '(sin (* 2 x)) 'x)
  => (* 2 (cos (* 2 x)))

  (deriv '(cos (** x 2)) 'x)
  => (* (* 2 x) (- (sin (** x 2))))

  (deriv '(exp (* 3 x)) 'x)
  => (* 3 (exp (* 3 x)))

  (deriv '(log x) 'x)
  => (/ 1 x)

  (deriv '(log (** x 2)) 'x)
  => (/ 2 x)

  (deriv '(/ (sin x) x) 'x)
  => (/ (- (* x (cos x)) (sin x)) (** x 2))

  (deriv '(sqrt x) 'x)
  => (/ 1 (* 2 (sqrt x)))

  (deriv '(sin (exp x)) 'x)
  => (* (cos (exp x)) (exp x))

  (deriv2 '(sin x) 'x)
  => (- (sin x))

  (show '(** x 3) 'x)
  d/dx (** x 3)  =  (* 3 (** x 2))

Approach
---------

Expressions are ordinary Scheme lists in prefix notation. A number
is a constant, a symbol is a variable, everything else is a list
with the operator in front.

Data-directed design
The key idea is a table called *table* that maps operator symbols to
differentiation procedures. put-rule! adds entries, get-rule fetches
them. The deriv procedure itself is very short - it checks if the
expression is a number, a variable, or a compound expression, and
in the last case it just looks up the operator and calls the registered
procedure. Adding a new function (e.g. arctan) means adding exactly
one put-rule! line. The core of deriv never needs to change.

Simplifier
The simplifier in simplify.scm works bottom-up: it simplifies the
children of an expression before simplifying the parent node. The
constructors in deriv.scm (make+, make*, etc.) all go through the
simplifier, so intermediate results are always kept clean.

Rules covered:
  +  -   sum and difference rules
  *      product rule: uv' + vu'
  /      quotient rule: (vu' - uv') / v^2
  **     power rule for u^n, and the general case for u^v
  sin    cos(u) * u'
  cos    -sin(u) * u'
  tan    u' / cos^2(u)
  cot    -u' / sin^2(u)
  sec    sec(u) * tan(u) * u'
  csc    -csc(u) * cot(u) * u'
  exp    exp(u) * u'
  log    u' / u
  sqrt   u' / (2 * sqrt(u))

The chain rule is implicit - every rule calls (deriv u var) on its
inner expression and multiplies the result in. So (sin (** x 2))
automatically gives the right answer without any special casing.

Critique
---------
What works well:
- Very easy to extend. One put-rule! call adds a new function.
- Simplifier runs automatically through the constructors so you
  never see (+ 0 x) or (* 1 x) in the output.
- Code is short and follows the SICP style closely.
- Chain rule is handled automatically, no special cases needed.

Limitations:
- Simplifier is one pass only. Nested expressions may not fully
  reduce. Running (simplify (simplify e)) fixes most cases.
- No common factor cancellation in general - only the two specific
  patterns needed for log derivatives.
- Output ordering may differ from what you would write by hand,
  e.g. (* (cos x) x) instead of (* x (cos x)), though both are
  mathematically the same.
