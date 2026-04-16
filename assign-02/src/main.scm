(load "../../assign-01/simplify.scm")
(load "../../assign-01/deriv.scm")
(load "model_defination.scm")
(load "model_physis.scm")

;; Step 6: The Simulation Loop
(define (run-simulation steps dt current-pos previous-pos)
  (if (<= steps 0)
      (display "--- Simulation Complete ---\n")
      (let* ((force (total-force-on-i current-pos (list current-pos 0.0))) ;; Simplified for 1D test
             (accel (calculate-acceleration force 1.0))
             (next-p (next-position current-pos previous-pos accel dt)))
        
        (display "Step: ") (display steps)
        (display " | Position: ") (display current-pos)
        (display " | Force: ") (display force) (newline)
        
        ;; Repeat for next time step
        (run-simulation (- steps 1) dt next-p current-pos))))

;; --- Run the Test Case ---
(display "--- Starting MD Simulation ---") (newline)
;; Start Atom A at 1.1, moving slightly from 1.11
(run-simulation 10 0.01 1.1 1.11) 

;; REMOVE THE ,q FROM HERE OR IT WILL ERROR

(exit)