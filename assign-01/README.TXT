============================================================
  README.TXT  –  Assignment I: Symbolic Differentiation
  Submitted by: Ayush Raj
============================================================

------------------------------------------------------------
FILES IN THIS SUBMISSION
------------------------------------------------------------

  differentiate.scm   Main entry point – load this file.
                      Calls load on the other modules and
                      runs a demo on startup.

  deriv-core.scm      Core engine:
                        • Expression vocabulary (make-*, operator, …)
                        • Data-directed dispatch table (put-deriv! / get-deriv)
                        • Central (deriv expr var) procedure

  simplify.scm        Bottom-up algebraic simplifier
                        • (simplify expr)
                        • Per-operator rules: +, -, *, /, **

  deriv-rules.scm     Differentiation rules registered via put-deriv!
                        • Arithmetic:     +  -  *  /  **
                        • Trigonometric:  sin cos tan cot sec csc
                        • Exponential:    exp  2^x (via **)
                        • Logarithmic:    log  log2  log10
                        • Algebraic:      sqrt

  tests.scm           Automated test suite (50+ assertions).

------------------------------------------------------------
APPROACH
------------------------------------------------------------

1. REPRESENTATION
   Expressions are ordinary Scheme lists in prefix notation,
   exactly as typed:
     (** x 3)          ≡  x³
     (+ (* 3 x) 1)     ≡  3x + 1
     (sin (* 2 x))     ≡  sin(2x)

   Atoms are either numbers (constants) or symbols (variables
   or function names).

2. DATA-DIRECTED DISPATCH  (20% feature)
   The core differentiator (deriv) holds a single association
   list *deriv-table* that maps operator symbols to procedures:

     (put-deriv! 'sin <proc>)   ; register the sin rule
     (get-deriv  'sin)          ; retrieve it

   deriv itself is just three lines:
     • base case: number  → 0
     • base case: symbol  → 1 (if same variable) else 0
     • compound: look up operator, call registered procedure

   "Additivity of code" is achieved because adding a new
   function family (e.g. 'arcsin) requires ONLY a single
   (put-deriv! …) call in deriv-rules.scm.  deriv-core.scm
   and simplify.scm are never touched.

3. DIFFERENTIATION RULES
   Each rule is a Scheme procedure (lambda (expr var) …).
   Standard calculus rules are applied:

     Arithmetic:
       Sum       u' + v'
       Difference u' − v'
       Product   u·v' + v·u'            (Leibniz rule)
       Quotient  (v·u' − u·v') / v²
       Power     n·uⁿ⁻¹·u'             (c-exponent case)
                 cⁿ·ln(c)·v'           (c-base case)
                 uᵥ·(v/u·u' + ln u·v') (general case)

     Trigonometric (chain rule applied automatically):
       sin u  →  cos(u)·u'
       cos u  →  −sin(u)·u'
       tan u  →  u' / cos²(u)
       cot u  →  −u' / sin²(u)
       sec u  →  sec(u)·tan(u)·u'
       csc u  →  −csc(u)·cot(u)·u'

     Exponential / Logarithmic (chain rule applied):
       exp(u)    →  exp(u)·u'
       log(u)    →  u'/u            (natural log)
       log2(u)   →  u'/(u·ln 2)
       log10(u)  →  u'/(u·ln 10)
       sqrt(u)   →  u'/(2·sqrt(u))

   The chain rule is implicit: every compound argument u is
   differentiated recursively and multiplied in.

4. SIMPLIFIER  (20% feature)
   (simplify expr) performs a single bottom-up pass.
   Children are simplified first, then the parent node:

     Identities applied (selection):
       0 + u  = u             u * 1  = u
       u + 0  = u             u * 0  = 0
       u − u  = 0             1 * u  = u
       u + u  = 2u            u / u  = 1
       u^0    = 1             u / 1  = u
       u^1    = u             0 / u  = 0
       n op m = <evaluated>   (both numeric)
       uⁿ * uᵐ = u^(n+m)     (same base)
       (u^n)^m = u^(nm)

   The simplifier is called after every recursive derivative
   call via the helper (ds expr var) = (simplify (deriv …)).
   This keeps intermediate results small and readable.

------------------------------------------------------------
HOW TO USE  (guile session)
------------------------------------------------------------

  $ guile
  GNU Guile 3.x …
  scheme@(guile-user)> (load "differentiate.scm")

  The demo runs automatically.  Thereafter:

  ── Basic differentiation ──────────────────────────────────

  scheme@(guile-user)> (d '(** x 3) 'x)
  $1 = (* 3 (** x 2))                  ; 3x²

  scheme@(guile-user)> (d '(+ (* 5 (** x 2)) (* 3 x)) 'x)
  $2 = (+ (* 10 x) 3)                  ; 10x + 3

  scheme@(guile-user)> (d '(sin (* 2 x)) 'x)
  $3 = (* 2 (cos (* 2 x)))             ; 2·cos(2x)

  scheme@(guile-user)> (d '(log (** x 2)) 'x)
  $4 = (/ 2 x)                         ; 2/x

  scheme@(guile-user)> (d '(exp (* 3 x)) 'x)
  $5 = (* 3 (exp (* 3 x)))             ; 3·e^(3x)

  scheme@(guile-user)> (d '(/ (sin x) x) 'x)
  $6 = (/ (- (* x (cos x)) (sin x)) (** x 2))

  ── Pretty printing ────────────────────────────────────────

  scheme@(guile-user)> (show-d '(* x (sin x)) 'x)
    d/dx[ (* x (sin x)) ]
    = (+ (sin x) (* x (cos x)))

  ── Higher derivatives ─────────────────────────────────────

  scheme@(guile-user)> (d2 '(sin x) 'x)
  $7 = (- (sin x))                     ; −sin x

  scheme@(guile-user)> (dn '(** x 4) 'x 4)
  $8 = 24                              ; 4! = 24

  ── Gradient (multi-variable) ──────────────────────────────

  scheme@(guile-user)> (gradient '(+ (* x y) (** x 2)) '(x y))
  $9 = ((+ y (* 2 x)) x)              ; [y+2x, x]

  ── Running the test suite ─────────────────────────────────

  scheme@(guile-user)> (load "tests.scm")
  (all tests pass)

------------------------------------------------------------
CRITIQUE
------------------------------------------------------------

PROS
  • Clean separation of concerns: core, rules, simplifier are
    independent files.  Adding a new family of functions is a
    single (put-deriv! …) call with zero changes elsewhere.

  • The data-directed table makes the system open for extension
    and closed for modification (OCP).

  • Recursive chain-rule application is automatic; every compound
    argument is differentiated and multiplied without special-
    casing in the caller.

  • The simplifier is purely functional and composable; it can
    be applied at any point and is idempotent on already-simple
    expressions.

  • Higher-order derivatives and gradients come "for free" by
    composing (d …) with itself.

CONS / LIMITATIONS
  • Single-pass simplifier – does not achieve a canonical form.
    For example  (* 2 (* 3 x))  simplifies to  (* 6 x)  but
    some nested products may need multiple passes.

  • No symbolic constant folding beyond numbers.  π and e
    are treated as plain symbols, not their numerical values.

  • Only total derivatives of single-argument operators; partial
    derivatives of multi-argument primitives (e.g. atan2, expt
    with two symbolic arguments) rely on the general power rule.

  • No inverse-trigonometric or hyperbolic functions (not in
    scope per the specification).

  • Expression normalisation (commutativity, associativity) is
    minimal; (+ x 1) and (+ 1 x) may not simplify identically.

  • No rational-function simplification (GCD cancellation), so
    quotient results can be verbose.

------------------------------------------------------------
END OF README
------------------------------------------------------------
