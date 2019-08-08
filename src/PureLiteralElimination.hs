module PureLiteralElimination where

import qualified Data.List as List
import qualified Data.Set as Set

import CNF (CNF(..), Clause(..), Lit(..), allLiterals, posAtoms, negAtoms)

isUnitLiteral :: Clause a -> Bool
isUnitLiteral (Disj lits) = Set.size lits == 1

removeLiteral :: (Ord a) => Lit a -> Clause a -> Clause a
removeLiteral lit (Disj lits) = Disj $ Set.delete lit lits

deleteUnlessSingleton :: (Ord a) => Lit a -> Clause a -> Clause a
deleteUnlessSingleton lit clause = f clause
  where f
          | isUnitLiteral clause = id
          | otherwise            = removeLiteral lit

eliminateLiteral :: (Ord a) => Lit a -> CNF a -> CNF a
eliminateLiteral lit (CNF clauses) = CNF $ Set.map updateClause clauses
  where updateClause = deleteUnlessSingleton lit

allPureLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allPureLiterals cnf = purePosLiterals `Set.union` pureNegLiterals
  where
    purePosLiterals = Set.map Pos $ posAtoms' `Set.difference` negAtoms'
    pureNegLiterals = Set.map Neg $ negAtoms' `Set.difference` posAtoms'
    posAtoms' = posAtoms $ allLiterals cnf
    negAtoms' = negAtoms $ allLiterals cnf

eliminateAllPureLiterals :: (Ord a) => CNF a -> CNF a
eliminateAllPureLiterals cnf = Set.fold eliminateLiteral cnf (allPureLiterals cnf)
