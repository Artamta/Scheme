;; Constants
(define epsilon 1.0)
(define sigma 1.0)

;; Helper for the Symbolic layer to wrap an expression in a negative sign
(define (make-neg expr)
  (list '- expr))