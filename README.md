# sudoku-sat

A Sudoku solver, using SAT as a problem representation.

I'd like to solve Sudoku by converting it to a SAT (boolean satisfiability problem) and using a general-purpose SAT solving algorithm to find the solution.

Currently this repository contains an implementation of the Davis–Putnam–Logemann–Loveland (DPLL) algorithm in "DPLL.hs". This is a pretty simple algorithm which I suspect will be too slow to solve difficult problems. However, a variety of more efficient algorithms (such as conflict-driven clause learning) are built on top of this algorithm so it seems like a good place to start.

Tests for this project are written in QuickCheck.

I am not convinced that the backtracking part of DPLL is working correctly, I need to add a test to ensure that.

The implementation of the algorithm also needs to be made a lot faster if it is to solve Sudoku problems, since they contain a large number of rules (2916 or (3^3^3)^2 * 4 for the 9x9 case). UPDATE: It can solve Sudoku problems in about 1.5s.

TODO:

- [x] Assert that if a given CNF problem can be solved by the DPLL algorithm without backtracking (i.e. one step), then the resulting solution is valid
- [x] Assert that is a CNF problem problem can be solved with backtracking, then the resulting solution is valid
- [x] Solve a 4x4x4 Sudoku problem
- [x] Solve a 9x9x9 standard Sudoku problem
- [ ] More granular tests for parts of the algorithm
