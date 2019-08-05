# sudoku-sat

A WIP Sudoku solver.

I'd like to solve Sudoku by converting it to a SAT (boolean satisfiability problem) and then using a general-purpose SAT solving algorithm to find the solution.

Currently this repository contains an implementation of the Davis–Putnam–Logemann–Loveland (DPLL) algorithm in "DPLL.hs". This is a pretty simple algorithm which I suspect will be too slow to solve difficult problems. I am considering also implementing the conflict-driven clause learning (CDCL) algorithm, which should be much faster as it allows the solver to 'decompose' a partially solved problem into multiple smaller problems by inferring which variables are dependent on other variables.
