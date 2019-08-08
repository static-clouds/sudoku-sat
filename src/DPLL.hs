module DPLL where

import qualified Data.List as List
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)
import qualified Data.Set  as Set
import CNF (CNF(..), Clause(..), Lit, invLit, posAtoms, negAtoms, isEmpty, extractUnitLiteral, allLiterals, allUnitLiterals, addUnitClause, isConsistent)
import PureLiteralElimination (eliminateAllPureLiterals)
import UnitPropagation (unitPropagateAll)

import Debug.Trace (trace)

isConsistentSetOfLiterals :: (Ord a) => CNF a -> Bool
isConsistentSetOfLiterals (CNF clauses) = isAllUnitLiterals && isConsistent unitLiterals
  where
    extractedUnitLiterals = Set.map extractUnitLiteral clauses
    isAllUnitLiterals = all isJust extractedUnitLiterals
    unitLiterals = Set.map fromJust extractedUnitLiterals

hasEmptyClauses :: (Ord a) => CNF a -> Bool
hasEmptyClauses (CNF clauses) = any isEmpty clauses


takeOne :: Set.Set a -> Maybe a
takeOne s
  | Set.size s > 0 = Just $ Set.elemAt 0 s
  | otherwise      = Nothing

chooseLiteral :: (Ord a) => CNF a -> Maybe (Lit a)
chooseLiteral cnf = takeOne $ allLiterals cnf `Set.difference` allUnitLiterals cnf

seqMaybe :: [Maybe a] -> Maybe a
seqMaybe []            = Nothing
seqMaybe (Nothing:xs)  = seqMaybe xs
seqMaybe ((Just a):xs) = Just a

makeBranches :: (Ord a) => CNF a -> [CNF a]
makeBranches cnf = case (chooseLiteral cnf) of
      Just newLiteral -> [ addUnitClause newLiteral cnf
                         , addUnitClause (invLit newLiteral) cnf
                         ]
      Nothing         -> [cnf]


dpll :: (Ord a) => CNF a -> Maybe (CNF a)
dpll cnf
  | isConsistentSetOfLiterals cnf = Just cnf
  | hasEmptyClauses           cnf = Nothing
  | otherwise                     = seqMaybe $ map dpll $ makeBranches cnf'
  where cnf' = dpllStep cnf


dpllStep :: (Ord a) => CNF a -> CNF a
dpllStep = eliminateAllPureLiterals . unitPropagateAll
