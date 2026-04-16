;; Assignment II - model definitions

(define default-min-distance 1e-6)
(define default-dt 0.01)
(define default-steps 40)

;; small helper used when building symbolic expressions
(define (make-neg expr)
  (list '- expr))

;; per-species masses
(define species-mass-table
  '((A . 1.0)
    (B . 2.0)))

(define (mass-of species)
  (let ((entry (assoc species species-mass-table)))
    (if entry
        (cdr entry)
        (error "Unknown species" species))))

;; LJ parameters: ((species-1 species-2) epsilon sigma)
(define lj-pair-table
  '(((A A) 1.0 1.0)
    ((A B) 1.1 1.0)
    ((B B) 0.9 0.95)))

(define (pair-key species-a species-b)
  (list species-a species-b))

(define (lj-params species-a species-b)
  (let ((entry (assoc (pair-key species-a species-b) lj-pair-table)))
    (if entry
        (list (cadr entry) (caddr entry))
        (let ((reverse-entry (assoc (pair-key species-b species-a) lj-pair-table)))
          (if reverse-entry
              (list (cadr reverse-entry) (caddr reverse-entry))
              (error "Missing LJ parameters for species pair"
                     species-a
                     species-b))))))

(define (lj-epsilon species-a species-b)
  (car (lj-params species-a species-b)))

(define (lj-sigma species-a species-b)
  (cadr (lj-params species-a species-b)))
