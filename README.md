# sudoku-sat

A WIP Sudoku solver.

I'd like to solve Sudoku by converting it to a SAT (boolean satisfiability problem) and using a general-purpose SAT solving algorithm to find the solution.

Currently this repository contains an implementation of the Davis–Putnam–Logemann–Loveland (DPLL) algorithm in "DPLL.hs". This is a pretty simple algorithm which I suspect will be too slow to solve difficult problems. However, a variety of more efficient algorithms (such as conflict-driven clause learning) are built on top of this algorithm so it seems like a good place to start.

Tests for this project are written in QuickCheck.


TODO:

- [ ] Assert that if a given CNF problem can be solved by the DPLL algorithm, then the resulting solution is valid
- [ ] Assert that all solvable CNF problems can be solved by the DPLL algorithm
- [ ] More granular tests for parts of the algorithm
