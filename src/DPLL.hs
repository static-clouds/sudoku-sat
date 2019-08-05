module DPLL where

import qualified Data.List as List
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)
import qualified Data.Set  as Set
import CNF (CNF(..), Clause(..), Lit, invLit, posAtoms, negAtoms, isEmpty, extractUnitLiteral, allLiterals, allUnitLiterals, addUnitClause, isConsistent)
import PureLiteralElimination (eliminateAllPureLiterals)
import UnitPropagation (unitPropagateAll)

import Debug.Trace (trace)

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

makeBranches :: Lit -> CNF -> [CNF]
makeBranches lit cnf = map (flip addUnitClause $ cnf) [lit, invLit lit]

seqMaybe :: [Maybe a] -> Maybe a
seqMaybe []            = Nothing
seqMaybe (Nothing:xs)  = seqMaybe xs
seqMaybe ((Just a):xs) = Just a

dpll :: CNF -> Maybe CNF
dpll cnf
  | isConsistentSetOfLiterals cnf = Just cnf
  | hasEmptyClauses           cnf = Nothing
  | otherwise                     = case (chooseLiteral cnf') of
      Just newLiteral -> seqMaybe $ map dpll $ makeBranches newLiteral cnf'
      Nothing         -> dpll cnf'
  where cnf' = dpllStep cnf


dpllStep :: CNF -> CNF
dpllStep = eliminateAllPureLiterals . unitPropagateAll
