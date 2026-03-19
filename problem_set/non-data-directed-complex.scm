;; Complex numbers.

;; Data tagging

;; We  tag   a  pair  of   numbers  representing  a  complex   by  its
;; representation.  We  use symbols:  'polar for  polar representation
;; and 'rectangular for rectangular representation.

;; Implementation version 2: Non-Data directed approach
;; 
;; 0.      add-tag cnum-type-tag
;; RECTANGULAR Fundamental
;; 1.ctor  make-complex-rectangular
;; 1.pred  is-complex-rectangular?
;; 1.stor  get-type-tag-rectangular
;; 1.stor  get-real-part-rectangular
;; 1.stor  get-imag-part-rectangular
;; 1.stor  get-magnitude-rectangular
;; 1.stor  get-angle-rectangular
;; 1.stor  get-conjugate-rectangular
;; 2.rator add-rectangular-complex
;; 2.rator subtract-rectangular-complex
;; 2.rator multiply-rectangular-complex
;; 3.pred  is-zero-rectangular?
;; 3.pred  is-pure-real-rectangular?
;; 3.pred  is-pure-imaginary-rectangular?
;; POLAR Fundamental
;; 1.ctor  make-complex-polar
;; 1.pred  is-complex-polar?
;; 1.stor  get-type-tag-polar
;; 1.stor  get-real-part-polar
;; 1.stor  get-imag-part-polar
;; 1.stor  get-magnitude-polar
;; 1.stor  get-angle-polar
;; 1.stor  get-conjugate-polar
;; 2.rator add-polar-complex
;; 2.rator subtract-polar-complex
;; 2.rator multiply-polar-complex
;; 3.pred  is-zero-polar?
;; 3.pred  is-pure-real-polar?
;; 3.pred  is-pure-imaginary-polar?
;; RECTANGULAR - POLAR Interconversion etc.
;; 4.rator polar->rectangular
;; 4.rator rectangular->polar
;; 4.stor  get-magnitude-rectangular
;; 4.stor  get-angle-rectangular
;; 4.stor  get-real-part-polar
;; 4.stor  get-imag-part-polar
;; Useful procedures
;;

;; Level 0: Low level procedures.
;; Some are Scheme primitives, and hence are just listed below.
;; cons, car, cdr, ...
(define (add-tag cnum-type-tag cnum)
  (if (symbol? cnum-type-tag)
      (cons cnum-type-tag cnum)
      #f))

;; Demo :-)
(define my-pi (* 4 (atan 1)))

;; Level 1: Ctors, Stors, Preds for rectangular complex numbers
;; Constructor
(define (make-complex-rectangular real-part imag-part)
  (if (and (real? real-part)
           (real? imag-part))
      (add-tag 'rectangular (cons real-part imag-part))
      #f))

;; Predicate
(define is-complex-rectangular?
  (lambda (z)
    (and (pair?  z)
	 (eq? 'rectangular (car z))
         (pair? (cdr z))
         (real? (car (cdr z)))
         (real? (cdr (cdr z))))))

;; Selectors
;; Get representation type
(define get-type-tag-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
	(car z)
	#f)))

;; Get real part
(define get-real-part-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
        (cadr z)			; (cadr z) = (car (cdr z))
        #f)))

;; Get imaginary part
(define get-imag-part-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
        (cddr z)			; (cddr z) = (cdr (cdr z))
        #f)))

;; ;; Get magnitude
;; (define get-magnitude-rectangular
;;   (lambda (z)
;;     (if (is-complex-rectangular? z)
;; 	(let ((rp      (get-real-part-rectangular z))
;; 	      (ip      (get-imag-part-rectangular z)))
;; 	  (sqrt (* rp rp) (* ip ip)))
;;         #f)))

;; ;; Get angle
;; (define get-angle-rectangular
;;   (lambda (z)
;;     (if (is-complex-rectangular? z)
;; 	(let ((rp      (get-real-part-rectangular z))
;; 	      (ip      (get-imag-part-rectangular z)))
;; 	  (arctan  ip  rp))
;;         #f)))

;; Get conjugate
(define get-conjugate-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
        (make-complex-rectangular
         (get-real-part-rectangular z)
         (- (get-imag-part-rectangular z)))
        #f)))

;; Level 2: Complex number arithmetic and algebra
(define add-rectangular-complex
  (lambda (z1 z2)
    (if (and (is-complex-rectangular? z1)
             (is-complex-rectangular? z2))
        (make-complex-rectangular
         (+ (get-real-part-rectangular z1)
            (get-real-part-rectangular z2))
         (+ (get-imag-part-rectangular z1)
            (get-imag-part-rectangular z2)))
        #f)))

(define subtract-rectangular-complex
  (lambda (z1 z2)
    (if (and (is-complex-rectangular? z1)
             (is-complex-rectangular? z2))
        (make-complex-rectangular
         (- (get-real-part-rectangular z1)
            (get-real-part-rectangular z2))
         (- (get-imag-part-rectangular z1)
            (get-imag-part-rectangular z2)))
        #f)))

(define multiply-rectangular-complex
  (lambda (z1 z2)
    (if (and (is-complex-rectangular? z1)
             (is-complex-rectangular? z2))
        (make-complex-rectangular
         (- (* (get-real-part-rectangular z1)
               (get-real-part-rectangular z2))
            (* (get-imag-part-rectangular z1)
               (get-imag-part-rectangular z2)))
         (+ (* (get-real-part-rectangular z1)
               (get-imag-part-rectangular z2))
            (* (get-imag-part-rectangular z1)
               (get-real-part-rectangular z2))))
        #f)))

;; RECTANGULAR Fundamental
;; 3.pred  is-zero-rectangular?
;; 3.pred  is-pure-real-rectangular?
;; 3.pred  is-pure-imaginary-rectangular?
;; Get magnitude
;; (define get-magnitude-rectangular
;;   (lambda (z)
;;     (if (is-complex-rectangular? z)
;;         ; What is the difference if we write and use a "square" procedure?      
;;         (sqrt (+ (* (get-real-part-rectangular z)
;;                     (get-real-part-rectangular z))
;;                  (* (get-imag-part-rectangular z)
;;                     (get-imag-part-rectangular z))))
;;         #f)))
(define get-magnitude-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
        ; What is the difference if we write and use a "square" procedure?      
	(let ((rp     (get-real-part-rectangular z))
	      (ip     (get-imag-part-rectangular z)))
          (sqrt (+ (* rp rp)
                   (* ip ip))))
        #f)))

;; Get angle
(define get-angle-rectangular
  (lambda (z)
    (if (is-complex-rectangular? z)
        (atan (get-imag-part-rectangular z)
              (get-real-part-rectangular z))
        #f)))

;; Level 3: Useful procedures
;; Useful predicates
(define is-zero-rectangular?
  (lambda (z)
    (and (eq? 'rectangular (car z))
         (pair? (cdr z))
         (zero? (car (cdr z)))
         (zero? (cdr (cdr z))))))
    
(define is-pure-real-rectangular?
  (lambda (z)
    (and (eq? 'rectangular (car z))
         (pair? (cdr z))
         (not (zero? (car (cdr z))))
         (zero? (cdr (cdr z))))))
    
(define is-pure-imaginary-rectangular?
  (lambda (z)
    (and (eq? 'rectangular (car z))
         (pair? (cdr z))
         (zero? (car (cdr z)))
         (not (zero? (cdr (cdr z)))))))
    

;; Level 1: Ctors, Stors, Preds for polar complex numbers
;; Polar
(define (make-complex-polar magnitude angle)
  (if (and (real? magnitude)
           (real? angle))
      (add-tag 'polar (cons magnitude angle))))

(define is-complex-polar?
  (lambda (z)
    (and (pair? z)
	 (eq? 'polar (car z))
         (pair? (cdr z))
	 (real? (car (cdr z)))
	 (real? (cdr (cdr z))))))

(define get-type-tag-polar
  (lambda (z)
    (if (is-complex-polar? z)
	(car z)
	#f)))

(define is-zero-polar?
  (lambda (z)
    (and (eq? 'polar (car z))
         (pair? (cdr z))
         (zero? (car (cdr z)))
         (zero? (cdr (cdr z))))))
    
(define is-pure-real-polar?
  (lambda (z)
    (and (eq? 'polar (car z))
         (pair? (cdr z))
         (not (zero? (car (cdr z))))
         (or (eq? 0 (cdr (cdr z)))
	     (eq? my-pi (cdr (cdr z))))
	 )))
    
(define is-pure-imaginary-polar?
  (lambda (z)
    (and (eq? 'polar (car z))
         (pair? (cdr z))
         (zero? (car (cdr z)))
         (not (zero? (cdr (cdr z)))))))
    
(define get-magnitude-polar
  (lambda (z)
    (if (is-complex-polar? z)
        (car (cdr z))
        #f)))

(define get-angle-polar
  (lambda (z)
    (if (is-complex-polar? z)
        (cddr z)
        #f)))

(define get-real-part-polar
  (lambda (z)
    (if (is-complex-polar? z)
        (* (get-magnitude-polar z)
           (cos (get-angle-polar z)))
        #f)))

(define get-imag-part-polar
  (lambda (z)
    (if (is-complex-polar? z)
        (* (get-magnitude-polar z)
           (sin (get-angle-polar z)))
        #f)))

(define get-conjugate-polar
  (lambda (z)
    (if (is-complex-polar? z)
        (make-complex-polar
         (get-magnitude-polar z)
         (- (get-angle-polar z)))
        #f)))

(define add-polar-complex
  (lambda (z1 z2)
    (if (and (is-complex-polar? z1)
             (is-complex-polar? z2))
        (make-complex-polar
         (+ (get-magnitude-polar z1)
            (get-magnitude-polar z2))
         (+ (get-angle-polar z1)
            (get-angle-polar z2)))
        #f)))

(define subtract-polar-complex
  (lambda (z1 z2)
    (if (and (is-complex-polar? z1)
             (is-complex-polar? z2))
        (make-complex-polar
         (- (get-magnitude-polar z1)
            (get-magnitude-polar z2))
         (- (get-angle-polar z1)
            (get-angle-polar z2)))
        #f)))


(define multiply-polar-complex
  (lambda (z1 z2)
    (if (and (is-complex-polar? z1)
             (is-complex-polar? z2))
        (make-complex-polar
         (- (* (get-magnitude-polar z1)
               (get-magnitude-polar z2))
            (* (get-angle-polar z1)
               (get-angle-polar z2)))
         (+ (* (get-magnitude-polar z1)
               (get-angle-polar z2))
            (* (get-angle-polar z1)
               (get-magnitude-polar z2))))
        #f)))

(define (polar->rectangular z)
  (if (is-complex-polar? z)
      (make-complex-rectangular
       (get-real-part-polar z)
       (get-imag-part-polar z))
      #f))


(define (rectangular->polar z)
  (if (is-complex-rectangular? z)
      (make-complex-polar
       (get-magnitude-rectangular z)
       (get-angle-rectangular z))
      #f))

;; Some useful definitions
(define pi (* 4 (atan 1)))
(define z1 (make-complex-rectangular 1 2))
(define z2 (make-complex-rectangular 2 1))
(define z3 (make-complex-polar 2 pi))
(define z4 (make-complex-polar 3 (/ pi 2)))
(define z5 (rectangular->polar z1))
(define z6 (polar->rectangular z5))

