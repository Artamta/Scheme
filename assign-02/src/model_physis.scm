;;this module is used for Defining the physic of the model
;;force accleration lj potential etc

;;epsiolon used for lj-potential
(define (epsilon ?))

;;sigma used for lj-potential
(define (sigma ?))

;;lj-potential-formula
(define (lj-potential r)
    (* (* 4 epsilon (expt sigma 6))(- (* (expt sigma 2) (expt (1/r) 12)) (expt 1/r 6)))


