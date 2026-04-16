;;this file is the main file

;;Importing modeules
(load "../../assign-01/simplify.scm")
(load "../../assign-01/deriv.scm")
(load "model_defination.scm")
(load "model_physis.scm")


(newline)
(display "--- Lennard-Jones Force Test ---")
(newline)
(display "Distance (r): 1.122") (newline)
(display "Force: ")
(display (calculate-force 0.0003 epsilon sigma))
(newline)
(display "--------------------------------")
(newline)
(display "Distance (r): 1.122") (newline)
(display "Force: ")
(display (calculate-force 1.122 epsilon sigma)) ;; This will be near 0
(newline)

(display "Distance (r): 0.95") (newline)
(display "Force: ")
(display (calculate-force 0.95 epsilon sigma))  ;; This will be a large positive number
(newline)
(newline)
(newline)
(newline)
