module UnitPropagation where

import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import CNF

unitPropagate :: Lit -> Clause -> Maybe Clause
unitPropagate lit (Disj xs)
      | lit  `elem` xs = Nothing
      | Set.null xs    = Just $ Disj xs
      | lit' `elem` xs = Just $ Disj $ Set.delete lit' xs
      | otherwise      = Just $ Disj xs
  where lit' = invLit lit

unitPropagate' :: Lit -> CNF -> CNF
unitPropagate' a (CNF clauses) = CNF $ mapMaybe (unitPropagate a) clauses


allUnitLiteralsPropagated :: (Set.Set Lit, CNF) -> Bool
allUnitLiteralsPropagated (propagated, cnf) = Set.null unusedLiterals
  where
    unusedLiterals = (allUnitLiterals cnf) `Set.difference` propagated

propagateUnitLiterals :: (Set.Set Lit, CNF) -> (Set.Set Lit, CNF)
propagateUnitLiterals (propagated, cnf) = (Set.insert nextLiteral propagated, unitPropagate' nextLiteral cnf)
  where
    unusedLiterals = (allUnitLiterals cnf) `Set.difference` propagated
    nextLiteral = Set.elemAt 0 unusedLiterals

unitPropagateAll :: CNF -> CNF
unitPropagateAll cnf = snd $ until allUnitLiteralsPropagated propagateUnitLiterals (Set.empty, cnf)
