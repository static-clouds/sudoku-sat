module PureLiteralElimination where

import qualified Data.List as List
import qualified Data.Set as Set

import CNF (CNF(..), Clause(..), Lit(..), allLiterals, posAtoms, negAtoms)

deleteUnlessSingleton :: Lit -> Clause -> Clause
deleteUnlessSingleton lit (Disj lits) = Disj newLits
  where newLits
          | Set.size lits == 1 = lits
          | otherwise          = Set.delete lit lits

eliminateLiteral :: Lit -> CNF -> CNF
eliminateLiteral lit (CNF clauses) = CNF $ Set.map updateClause clauses
  where updateClause = deleteUnlessSingleton lit

allPureLiterals :: CNF -> Set.Set Lit
allPureLiterals cnf = purePosLiterals `Set.union` pureNegLiterals
  where
    purePosLiterals = Set.map Pos $ posAtoms' `Set.difference` negAtoms'
    pureNegLiterals = Set.map Neg $ negAtoms' `Set.difference` posAtoms'
    posAtoms' = posAtoms $ allLiterals cnf
    negAtoms' = negAtoms $ allLiterals cnf

eliminateAllPureLiterals :: CNF -> CNF
eliminateAllPureLiterals cnf = Set.fold eliminateLiteral cnf (allPureLiterals cnf)
