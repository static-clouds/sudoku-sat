module UnitPropagation where

import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import CNF

unitPropagate :: Lit -> Clause -> Maybe Clause
unitPropagate lit (Disj xs)
      | lit  `elem` xs = Nothing
      | lit' `elem` xs = Just $ Disj $ Set.delete lit' xs
      | otherwise      = Just $ Disj xs
  where lit' = invLit lit

unitPropagate' :: Lit -> CNF -> CNF
unitPropagate' a (CNF clauses) = CNF $ mapMaybe (unitPropagate a) clauses


allUnitLiteralsPropagated :: (Set.Set Lit, CNF) -> Bool
allUnitLiteralsPropagated (propagated, cnf) = Set.null $ allUnitLiterals cnf

propagateUnitLiterals :: (Set.Set Lit, CNF) -> (Set.Set Lit, CNF)
propagateUnitLiterals (propagated, cnf) = (Set.insert nextLiteral propagated, unitPropagate' nextLiteral cnf)
  where
    nextLiteral = Set.elemAt 0 $ allUnitLiterals cnf

makeUnitLiteral :: Lit -> Clause
makeUnitLiteral lit = Disj $ Set.singleton lit

unitPropagateAll :: CNF -> CNF
unitPropagateAll cnf = CNF $ Set.union updatedClauses propagatedClauses
  where
    -- Propagate the unit literals through the CNF expression
    (propagated, (CNF updatedClauses)) = until allUnitLiteralsPropagated propagateUnitLiterals (Set.empty, cnf)
    -- Add the propagated unit literals back into the CNF expression
    propagatedClauses = Set.map makeUnitLiteral propagated
