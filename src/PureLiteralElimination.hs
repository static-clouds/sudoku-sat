module PureLiteralElimination where

import qualified Data.List as List
import qualified Data.Set as Set

import CNF (CNF(..), Clause(..), Lit(..), allLiterals, posAtoms, negAtoms)

updateLiterals :: (Set.Set Lit -> Set.Set Lit) -> Clause -> Clause
updateLiterals f (Disj lits) = Disj (f lits)

eliminateLiteral :: Lit -> CNF -> CNF
eliminateLiteral lit (CNF clauses) = CNF $ Set.map updateClause clauses
  where updateClause = updateLiterals $ Set.delete lit

allPureLiterals :: CNF -> Set.Set Lit
allPureLiterals cnf = purePosLiterals `Set.union` pureNegLiterals
  where
    purePosLiterals = Set.map Pos $ posAtoms' `Set.difference` negAtoms'
    pureNegLiterals = Set.map Neg $ negAtoms' `Set.difference` posAtoms'
    posAtoms' = posAtoms $ allLiterals cnf
    negAtoms' = negAtoms $ allLiterals cnf

eliminateAllPureLiterals :: CNF -> CNF
eliminateAllPureLiterals cnf = Set.fold eliminateLiteral cnf (allPureLiterals cnf)
