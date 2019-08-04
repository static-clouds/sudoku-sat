module DPLL where

import qualified Data.List as List
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)
import qualified Data.Set  as Set
import CNF (CNF(..), Clause(..), Lit, invLit, posAtoms, negAtoms, isEmpty, extractUnitLiteral, allLiterals, allUnitLiterals, addUnitClause, isConsistent)
import PureLiteralElimination (eliminateAllPureLiterals)
import UnitPropagation (unitPropagateAll)

isConsistentSetOfLiterals :: CNF -> Bool
isConsistentSetOfLiterals (CNF clauses) = isAllUnitLiterals && isConsistent unitLiterals
  where
    extractedUnitLiterals = Set.map extractUnitLiteral clauses
    isAllUnitLiterals = all isJust extractedUnitLiterals
    unitLiterals = Set.map fromJust extractedUnitLiterals

hasEmptyClauses :: CNF -> Bool
hasEmptyClauses (CNF clauses) = any isEmpty clauses


takeOne :: Set.Set a -> Maybe a
takeOne s
  | Set.size s > 0 = Just $ Set.elemAt 0 s
  | otherwise      = Nothing

chooseLiteral :: CNF -> Maybe Lit
chooseLiteral cnf = takeOne $ allLiterals cnf `Set.difference` allUnitLiterals cnf

makeBranches :: CNF -> [CNF]
makeBranches cnf = case chooseLiteral cnf of
  Just lit -> [addUnitClause lit cnf, addUnitClause (invLit lit) cnf]
  Nothing  -> []

dpll :: CNF -> Maybe CNF
dpll cnf
  | isConsistentSetOfLiterals cnf = Just cnf
  | hasEmptyClauses           cnf = Nothing
  | otherwise                     = listToMaybe . mapMaybe dpll $ branches
  where branches = makeBranches $ dpllStep cnf

dpllStep :: CNF -> CNF
dpllStep = eliminateAllPureLiterals . unitPropagateAll
