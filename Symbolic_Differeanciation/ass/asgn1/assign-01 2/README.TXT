README.TXT
Assignment I - Symbolic Differentiation
CTfS

----------------------------------------------------------------------
APPROACH
----------------------------------------------------------------------

The system differentiates symbolic expressions written in Scheme prefix
notation. Everything is in one file, deriv.scm.

The expression representation is just ordinary Scheme lists. A number
or symbol is a leaf. Everything else is a list whose car is the operator
and whose cadr / caddr are the operands, exactly like Scheme itself.

  (** x 3)          means  x^3
  (+ (* 2 x) 1)     means  2x + 1
  (sin (* 2 x))     means  sin(2x)


Data-directed design
--------------------
I used a table called *table* which maps operator symbols to
differentiation procedures. put-rule! adds an entry, get-rule fetches
one. The main deriv procedure just looks up the operator and calls
whatever procedure it finds:

  (deriv e var)
    - if e is a number  -> 0
    - if e is a symbol  -> 1 if it equals var, else 0
    - otherwise look up (op e) in *table* and call the procedure

This means adding support for a new function (say arcsin) is just one
more put-rule! call at the bottom of the file. The core deriv procedure
never changes. This is the "additivity of code" property.


Simplifier
----------
After computing each subexpression we immediately simplify it. The
constructors make+, make*, etc all call simplify on the way out, so
intermediate results never blow up.

The simplifier covers:
  - numeric constant folding (2+3 = 5 etc)
  - identity laws: 0+u=u, u*1=u, u*0=0, u^0=1, u^1=u, u-u=0, u/u=1
  - double negation -(-u) = u
  - algebraic cancellation in division: (2x)/(x^2) -> 2/x
                                        2/(2x)     -> 1/x
  - canonical form: always put numeric factor first (x*3 -> 3*x)

Differentiation rules
---------------------
Standard calculus rules, all installed with put-rule!:

  +  -  (unary and binary)
  *     product rule:   uv' + vu'
  /     quotient rule:  (vu' - uv') / v^2
  **    power rule (3 cases: constant exponent, constant base, general)
  sin   cos(u) * u'
  cos   -sin(u) * u'
  tan   u' / cos^2(u)
  cot   -u' / sin^2(u)
  sec   sec(u)*tan(u)*u'
  csc   -csc(u)*cot(u)*u'
  exp   exp(u) * u'
  log   u' / u          (natural log)
  sqrt  u' / (2*sqrt(u))

The chain rule is automatic because every rule recursively calls
(deriv u var) on the inner expression and multiplies.

----------------------------------------------------------------------
HOW TO USE
----------------------------------------------------------------------

Start guile and load the file:

  $ guile
  scheme@(guile-user)> (load "deriv.scm")

Basic usage:

  (deriv '(** x 3) 'x)
  => (* 3 (** x 2))

  (deriv '(+ (* 3 (** x 2)) (* 2 x)) 'x)
  => (+ (* 6 x) 2)

  (deriv '(sin (* 2 x)) 'x)
  => (* 2 (cos (* 2 x)))

  (deriv '(log (** x 2)) 'x)
  => (/ 2 x)

  (deriv '(exp (* 3 x)) 'x)
  => (* 3 (exp (* 3 x)))

  (deriv '(/ (sin x) x) 'x)
  => (/ (- (* x (cos x)) (sin x)) (** x 2))

  (deriv '(* x (sin x)) 'x)
  => (+ (* x (cos x)) (sin x))

Second derivative:

  (deriv2 '(sin x) 'x)
  => (- (sin x))

  (deriv2 '(** x 4) 'x)
  => (* 12 (** x 2))

Pretty printing:

  (show '(** x 3) 'x)
  d/dx (** x 3)  =  (* 3 (** x 2))

Run tests:

  scheme@(guile-user)> (load "tests.scm")

----------------------------------------------------------------------
CRITIQUE
----------------------------------------------------------------------

What works well:

- The data-directed table makes the system easy to extend. Adding
  arcsin or any new function is just one extra put-rule! line.

- The simplifier is called inside the constructors (make+, make* etc)
  so results simplify automatically without any extra step.

- The code stays close to the SICP style - no macros, no objects,
  just lists and procedures.

- Chain rule is handled implicitly, which keeps each rule short.

Limitations:

- The simplifier is only one pass and does not reach a fully canonical
  form. Something like (* 2 (* 3 x)) would need a second pass to
  become (* 6 x). Running simplify twice fixes most cases.

- No common factor cancellation in general quotients, only the two
  specific cases needed for log derivatives.

- Expressions with multiple variables work for partial derivatives
  (just call deriv with a different var) but there is no built-in
  gradient procedure.

- No inverse trig or hyperbolic functions, which were out of scope.

----------------------------------------------------------------------
