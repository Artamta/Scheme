;; Assignment II - simple main driver

(define (load-first-existing path-list)
  (if (null? path-list)
      (error "Could not find dependency")
      (catch #t
        (lambda ()
          (load (car path-list)))
        (lambda args
          (load-first-existing (cdr path-list))))))

;; local Assignment I files copied into submission/asg1
(load-first-existing '("../asg1/simplify.scm"
                       "asg1/simplify.scm"
                       "../../assign-01/simplify.scm"))

(load-first-existing '("../asg1/deriv.scm"
                       "asg1/deriv.scm"
                       "../../assign-01/deriv.scm"))

(load-first-existing '("model_defination.scm"
                       "src/model_defination.scm"))

(load-first-existing '("model_physis.scm"
                       "src/model_physis.scm"))

;; particle layout: (id species mass position previous-position)
(define (make-particle particle-id species position previous-position)
  (list particle-id species (mass-of species) position previous-position))

(define (particle-id p) (list-ref p 0))
(define (particle-species p) (list-ref p 1))
(define (particle-mass p) (list-ref p 2))
(define (particle-position p) (list-ref p 3))
(define (particle-previous-position p) (list-ref p 4))

(define (particle-with-next-position p next-position)
  (list (particle-id p)
        (particle-species p)
        (particle-mass p)
        next-position
        (particle-position p)))

(define (all-true predicate values)
  (if (null? values)
      #t
      (and (predicate (car values))
           (all-true predicate (cdr values)))))

(define (validate-particles particles dim)
  (if (null? particles)
      (error "Particle list cannot be empty")
      #t)
  (if (not (all-true (lambda (p) (= (length (particle-position p)) dim)) particles))
      (error "Position dimension mismatch" dim)
      #t)
  (if (not (all-true (lambda (p) (= (length (particle-previous-position p)) dim)) particles))
      (error "Previous-position dimension mismatch" dim)
      #t)
  #t)

;; row format: (step particle species position velocity force acceleration)
(define (simulate-step particles dt step-index)
  (let* ((positions (map particle-position particles))
         (species-list (map particle-species particles))
         (forces (all-forces positions species-list))
         (step-results
          (map (lambda (particle force)
                 (let* ((accel (calculate-acceleration force (particle-mass particle)))
                        (next-pos (next-position (particle-position particle)
                                                 (particle-previous-position particle)
                                                 accel
                                                 dt))
                        (velocity (estimate-velocity next-pos
                                                     (particle-previous-position particle)
                                                     dt))
                        (next-particle (particle-with-next-position particle next-pos))
                        (row (list step-index
                                   (particle-id particle)
                                   (particle-species particle)
                                   (particle-position particle)
                                   velocity
                                   force
                                   accel)))
                   (list next-particle row)))
               particles
               forces)))
    (list (map car step-results)
          (map cadr step-results))))

(define (show-row row)
  (display "  particle=") (display (list-ref row 1))
  (display " species=") (display (list-ref row 2))
  (display " pos=") (display (list-ref row 3))
  (display " vel=") (display (list-ref row 4))
  (newline))

(define (show-step-summary step-rows)
  (display "Step ") (display (list-ref (car step-rows) 0))
  (display " done")
  (newline)
  (for-each show-row step-rows))

(define (run-simulation dim steps dt particles)
  (validate-particles particles dim)
  (let loop ((step 1)
             (state particles)
             (history '()))
    (if (> step steps)
        (begin
          (display "Simulation complete.\n")
          history)
        (let* ((step-output (simulate-step state dt step))
               (next-state (list-ref step-output 0))
               (step-rows (list-ref step-output 1)))
          (show-step-summary step-rows)
          (loop (+ step 1)
                next-state
                (append history step-rows))))))

;; CSV helpers
(define (to-text value)
  (cond ((string? value) value)
        ((symbol? value) (symbol->string value))
        (else (number->string value))))

(define (join-with-comma fields)
  (if (null? fields)
      ""
      (let loop ((rest (cdr fields))
                 (result (car fields)))
        (if (null? rest)
            result
            (loop (cdr rest)
                  (string-append result "," (car rest)))))))

(define (component-names prefix dim)
  (let loop ((index 1) (result '()))
    (if (> index dim)
        (reverse result)
        (loop (+ index 1)
              (cons (string-append prefix (number->string index)) result)))))

(define (csv-header dim)
  (join-with-comma
   (append '("step" "particle" "species")
           (component-names "x" dim)
           (component-names "v" dim)
           (component-names "f" dim)
           (component-names "a" dim))))

(define (row->csv-fields row)
  (append (list (to-text (list-ref row 0))
                (to-text (list-ref row 1))
                (to-text (list-ref row 2)))
          (map to-text (list-ref row 3))
          (map to-text (list-ref row 4))
          (map to-text (list-ref row 5))
          (map to-text (list-ref row 6))))

(define (write-history-csv file-path history dim)
  (call-with-output-file
      file-path
    (lambda (port)
      (display (csv-header dim) port)
      (newline port)
      (for-each
       (lambda (row)
         (display (join-with-comma (row->csv-fields row)) port)
         (newline port))
       history))))

;; sample systems
(define (sample-particles-1d)
  (list (make-particle 0 'A '(0.0) '(-0.01))
        (make-particle 1 'B '(1.3) '(1.31))
        (make-particle 2 'A '(2.8) '(2.79))))

(define (sample-particles-1d-approach)
  (list (make-particle 0 'A '(0.2) '(0.1))
        (make-particle 1 'B '(2.9) '(3.0))))

(define (sample-particles-2d)
  (list (make-particle 0 'A '(0.0 0.0) '(-0.01 0.0))
        (make-particle 1 'B '(1.1 0.2) '(1.11 0.2))
        (make-particle 2 'A '(0.4 1.3) '(0.41 1.29))
        (make-particle 3 'B '(1.8 1.0) '(1.81 1.01))))

(define (sample-particles-3d)
  (list (make-particle 0 'A '(0.0 0.0 0.0) '(-0.01 0.0 0.0))
        (make-particle 1 'B '(1.2 0.2 0.1) '(1.21 0.2 0.11))
        (make-particle 2 'A '(0.5 1.1 0.4) '(0.51 1.1 0.4))
        (make-particle 3 'B '(1.9 1.3 0.8) '(1.9 1.29 0.79))))

;; user-facing wrappers
(define (run-1d steps dt)
  (run-simulation 1 steps dt (sample-particles-1d)))

(define (run-1d-approach steps dt)
  (run-simulation 1 steps dt (sample-particles-1d-approach)))

(define (run-2d steps dt)
  (run-simulation 2 steps dt (sample-particles-2d)))

(define (run-3d steps dt)
  (run-simulation 3 steps dt (sample-particles-3d)))

(define (run-1d-to-file steps dt file-path)
  (let ((history (run-1d steps dt)))
    (write-history-csv file-path history 1)
    history))

(define (run-1d-approach-to-file steps dt file-path)
  (let ((history (run-1d-approach steps dt)))
    (write-history-csv file-path history 1)
    history))

(define (run-2d-to-file steps dt file-path)
  (let ((history (run-2d steps dt)))
    (write-history-csv file-path history 2)
    history))

(define (run-3d-to-file steps dt file-path)
  (let ((history (run-3d steps dt)))
    (write-history-csv file-path history 3)
    history))

(define (print-usage)
  (display "Submission simulator loaded.\n")
  (display "Examples:\n")
  (display "  (run-1d default-steps default-dt)\n")
  (display "  (run-1d-approach default-steps default-dt)\n")
  (display "  (run-2d default-steps default-dt)\n")
  (display "  (run-3d default-steps default-dt)\n")
  (display "CSV examples:\n")
  (display "  (run-1d-approach-to-file 40 0.01 \"output/one_d_particles_closer.csv\")\n")
  (display "  (run-2d-to-file 40 0.01 \"output/two_species_2d.csv\")\n")
  (display "  (run-3d-to-file 40 0.01 \"output/two_species_3d.csv\")\n"))

(print-usage)
