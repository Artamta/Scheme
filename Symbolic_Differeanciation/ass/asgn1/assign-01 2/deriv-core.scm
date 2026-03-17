;;; ============================================================
;;;  deriv-core.scm
;;;  Core differentiation engine – data-directed dispatch table
;;; ============================================================
;;;  Provides:
;;;    (deriv  expr var)        – raw derivative (un-simplified)
;;;    (put-deriv! op proc)     – register a differentiation rule
;;;    (get-deriv  op)          – look up a registered rule
;;; ============================================================

;;; ----------------------------------------------------------
;;; 1.  Expression vocabulary
;;; ----------------------------------------------------------

(define (variable? x)    (symbol? x))
(define (constant? x)    (number? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;; generic selectors for prefix s-expressions
(define (operator   expr) (car  expr))
(define (operands   expr) (cdr  expr))
(define (operand1   expr) (cadr  expr))
(define (operand2   expr) (caddr expr))
(define (unary?     expr) (and (pair? expr) (= (length expr) 2)))
(define (binary?    expr) (and (pair? expr) (= (length expr) 3)))

;;; constructors – always build canonical list structure
(define (make-sum        a b) (list '+  a b))
(define (make-difference a b) (list '-  a b))
(define (make-negation   a)   (list '-  a))
(define (make-product    a b) (list '*  a b))
(define (make-quotient   a b) (list '/  a b))
(define (make-power   base e) (list '** base e))
(define (make-app     f  arg) (list f arg))     ; unary function call: (make-app 'sin 'x) → (sin x)

;;; ----------------------------------------------------------
;;; 2.  Data-directed dispatch table
;;; ----------------------------------------------------------

;;; The table maps operator symbols → differentiation procedures.
;;; Using a plain association list keeps the code self-contained
;;; and avoids any dependency on hash tables or OO machinery.

(define *deriv-table* '())

(define (put-deriv! op proc)
  "Register PROC as the differentiation procedure for operator OP."
  (let ((existing (assq op *deriv-table*)))
    (if existing
        (set-cdr! existing proc)          ; update in place
        (set! *deriv-table*
              (cons (cons op proc)
                    *deriv-table*)))))

(define (get-deriv op)
  "Return the differentiation procedure registered for OP, or #f."
  (let ((entry (assq op *deriv-table*)))
    (if entry (cdr entry) #f)))

;;; ----------------------------------------------------------
;;; 3.  Central differentiator
;;; ----------------------------------------------------------

(define (deriv expr var)
  "Differentiate EXPR with respect to VAR (a symbol).
   Returns an un-simplified s-expression."
  (cond
    ;; ── base cases ──────────────────────────────────────────
    ((constant? expr)  0)
    ((variable? expr)  (if (same-variable? expr var) 1 0))
    ;; ── compound expression: look up operator ───────────────
    ((pair? expr)
     (let ((proc (get-deriv (operator expr))))
       (if proc
           (proc expr var)
           (error "deriv: unknown operator" (operator expr)))))
    (else
     (error "deriv: unrecognised expression" expr))))
