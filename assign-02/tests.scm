;;; tests.scm - Assignment II dedicated tests
;;; Run from assign-02 folder:
;;;   guile -l tests.scm

(load "src/main.scm")

(use-modules (ice-9 rdelim))

(define tests-run 0)
(define tests-passed 0)
(define tests-failed 0)
(define tolerance 1e-6)

(define (check-true label condition)
  (set! tests-run (+ tests-run 1))
  (if condition
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "PASS  ") (display label) (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "FAIL  ") (display label) (newline))))

(define (check-equal label expected got)
  (check-true label (equal? expected got))
  (if (not (equal? expected got))
      (begin
        (display "  expected: ") (display expected) (newline)
        (display "  got:      ") (display got) (newline))
      #t))

(define (check-approx label expected got)
  (check-true label (< (abs (- expected got)) tolerance))
  (if (not (< (abs (- expected got)) tolerance))
      (begin
        (display "  expected: ") (display expected) (newline)
        (display "  got:      ") (display got) (newline))
      #t))

(define (contains? text pattern)
  (let ((text-len (string-length text))
        (pattern-len (string-length pattern)))
    (let loop ((index 0))
      (cond ((> (+ index pattern-len) text-len) #f)
            ((string=? (substring text index (+ index pattern-len)) pattern) #t)
            (else (loop (+ index 1)))))))

(define (find-history-row history step particle-id)
  (let loop ((rows history))
    (if (null? rows)
        #f
        (let ((row (car rows)))
          (if (and (= (list-ref row 0) step)
                   (= (list-ref row 1) particle-id))
              row
              (loop (cdr rows)))))))

(define (distance-at-step history step particle-a particle-b)
  (let* ((row-a (find-history-row history step particle-a))
         (row-b (find-history-row history step particle-b))
         (xa (car (list-ref row-a 3)))
         (xb (car (list-ref row-b 3))))
    (abs (- xa xb))))

(display "\n-- force sanity --\n")
(let* ((eps-aa (lj-epsilon 'A 'A))
       (sig-aa (lj-sigma 'A 'A))
       (force-short (calculate-lj-force-magnitude 0.9 eps-aa sig-aa))
       (force-long (calculate-lj-force-magnitude 1.5 eps-aa sig-aa)))
  (check-true "LJ repulsive at short range" (> force-short 0.0))
  (check-true "LJ attractive at long range" (< force-long 0.0)))

(display "\n-- species tables --\n")
(check-approx "epsilon symmetry A-B == B-A"
              (lj-epsilon 'A 'B)
              (lj-epsilon 'B 'A))
(check-approx "sigma symmetry A-B == B-A"
              (lj-sigma 'A 'B)
              (lj-sigma 'B 'A))
(check-approx "mass of species A"
              1.0
              (mass-of 'A))
(check-approx "mass of species B"
              2.0
              (mass-of 'B))

(display "\n-- dimension progression --\n")
(let ((history-1d (run-1d 1 0.01))
  (history-1d-approach (run-1d-approach 1 0.01))
      (history-2d (run-2d 1 0.01))
      (history-3d (run-3d 1 0.01)))
  (check-equal "1D rows = steps * particle-count"
       5
               (length history-1d))
  (check-equal "1D approach rows = steps * particle-count"
       2
       (length history-1d-approach))
  (check-equal "2D rows = steps * particle-count"
       5
               (length history-2d))
  (check-equal "3D rows = steps * particle-count"
       6
               (length history-3d))
  (check-equal "1D position has 1 component"
               1
               (length (list-ref (car history-1d) 3)))
  (check-equal "2D position has 2 components"
               2
               (length (list-ref (car history-2d) 3)))
  (check-equal "3D position has 3 components"
               3
               (length (list-ref (car history-3d) 3))))

  (display "\n-- 1D approach behavior --\n")
  (let* ((approach-history (run-1d-approach 6 0.01))
    (d1 (distance-at-step approach-history 1 0 1))
    (d2 (distance-at-step approach-history 2 0 1))
    (d3 (distance-at-step approach-history 3 0 1))
    (d4 (distance-at-step approach-history 4 0 1))
    (d5 (distance-at-step approach-history 5 0 1))
    (d6 (distance-at-step approach-history 6 0 1))
    (min-distance (apply min (list d1 d2 d3 d4 d5 d6))))
    (check-true "Approach pair gets closer at some step"
      (< min-distance d1)))

(display "\n-- CSV output --\n")
(define test-csv-path "test_two_species_2d.csv")
(run-2d-to-file 2 0.01 test-csv-path)
(check-true "CSV file created"
            (file-exists? test-csv-path))
(let ((header-line (call-with-input-file
                       test-csv-path
                     (lambda (port)
                       (read-line port)))))
  (check-true "CSV header has x2" (contains? header-line "x2"))
  (check-true "CSV header has v2" (contains? header-line "v2"))
  (check-true "CSV header has f2" (contains? header-line "f2")))

(newline)
(display "Tests run:    ") (display tests-run) (newline)
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)
