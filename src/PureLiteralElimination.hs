module PureLiteralElimination where

import qualified Data.List as List
import qualified Data.Set as Set

import CNF (CNF, Clause, Lit(..), allLiterals, isUnitLiteral, posAtoms, mapClauses, negAtoms, removeLiteral)

deleteUnlessSingleton :: (Ord a) => Lit a -> Clause a -> Clause a
deleteUnlessSingleton lit clause = f clause
  where f
          | isUnitLiteral clause = id
          | otherwise            = removeLiteral lit

eliminateLiteral :: (Ord a) => Lit a -> CNF a -> CNF a
eliminateLiteral lit = mapClauses $ deleteUnlessSingleton lit

allPureLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allPureLiterals cnf = purePosLiterals <> pureNegLiterals
  where
    purePosLiterals = Set.map Pos $ posAtoms' `Set.difference` negAtoms'
    pureNegLiterals = Set.map Neg $ negAtoms' `Set.difference` posAtoms'
    posAtoms' = posAtoms $ allLiterals cnf
    negAtoms' = negAtoms $ allLiterals cnf

eliminateAllPureLiterals :: (Ord a) => CNF a -> CNF a
eliminateAllPureLiterals cnf = Set.fold eliminateLiteral cnf (allPureLiterals cnf)
