README.TXT
CTfS Assignment II - Simple MD Submission

Name: Ayush Raj
Roll No: 20221068

------------------------------------------------------------
1) Folder contents
------------------------------------------------------------

submission/
  README.TXT
  report.md
  tests.scm
  asg1/
    simplify.scm
    deriv.scm
    README.TXT
  src/
    model_defination.scm
    model_physis.scm
    main.scm
  output/

Notes:
- asg1 folder is copied from Assignment 1.
- Assignment 2 reuses deriv and simplify from asg1.

------------------------------------------------------------
2) Short approach
------------------------------------------------------------

I used a small layered design:

1. Model layer (src/model_defination.scm)
   Stores species masses and LJ pair parameters.

2. Physics layer (src/model_physis.scm)
   Builds symbolic LJ force using Assignment 1,
   computes pair force vectors and net force,
   and provides Verlet update functions.

3. Driver layer (src/main.scm)
   Builds particles, runs time-stepping,
   prints per-step output, and writes CSV files.

The same engine supports 1D, 2D, and 3D.

------------------------------------------------------------
3) Sample interaction (Guile)
------------------------------------------------------------

$ cd submission/src
$ guile
scheme@(guile-user)> (load "main.scm")
scheme@(guile-user)> (run-1d default-steps default-dt)
scheme@(guile-user)> (run-2d 20 0.01)
scheme@(guile-user)> (run-1d-approach-to-file 40 0.01 "../output/one_d_particles_closer.csv")

Or from submission root:

$ cd submission
$ guile -l tests.scm

------------------------------------------------------------
4) Pros and cons (critique)
------------------------------------------------------------

Pros:
- Small code and easy to read.
- Clear abstraction layers.
- Reuses Assignment 1 instead of hardcoding derivative.
- Supports multi-species with a table lookup.

Cons:
- O(N^2) pair loop, so not fast for large N.
- No boundary condition yet.
- No thermostat/energy analysis yet.

------------------------------------------------------------
5) Final zip command
------------------------------------------------------------

From assign-02 folder:

$ zip -r Ayush-Raj-assign-02.zip submission

If your instructor strictly wants folder name assign-02 inside zip,
rename or copy submission to assign-02 before zipping.
