module UnitPropagation where

import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import CNF (CNF(..), Clause, Lit, allUnitLiterals, fromUnitLiterals, invLit, litInClause, makeUnitLiteral, mapMaybe, removeLiteral)

unitPropagate :: (Ord a) => Lit a -> Clause a -> Maybe (Clause a)
unitPropagate lit clause
      | litInClause lit clause  = Nothing
      | litInClause lit' clause = Just $ removeLiteral lit' clause
      | otherwise               = Just $ clause
  where lit' = invLit lit

unitPropagate' :: (Ord a) => Lit a -> CNF a -> CNF a
unitPropagate' a (CNF clauses) = CNF $ mapMaybe (unitPropagate a) clauses

allUnitLiteralsPropagated :: (Ord a) => CNF a -> Bool
allUnitLiteralsPropagated = ((==) mempty) . allUnitLiterals

propagateUnitLiterals :: (Ord a) => (Set.Set (Lit a), CNF a) -> (Set.Set (Lit a), CNF a)
propagateUnitLiterals (propagated, cnf) = (propagated', cnf')
  where
    nextLiteral = Set.elemAt 0 $ allUnitLiterals cnf
    propagated' = Set.insert nextLiteral propagated
    cnf'        = unitPropagate' nextLiteral cnf

unitPropagateAll :: (Ord a) => CNF a -> CNF a
unitPropagateAll cnf = reducedCnf <> fromUnitLiterals propagated
  where
    -- Propagate the unit literals through the CNF expression
    (propagated, reducedCnf) = until (allUnitLiteralsPropagated . snd) propagateUnitLiterals (Set.empty, cnf)
