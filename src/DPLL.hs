module DPLL where

import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import qualified Data.Set  as Set
import CNF (CNF(..), Lit, invLit, isEmpty, extractUnitLiteral, allLiterals, allUnitLiterals, addUnitClause, isConsistent)
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

data Result a = Solution a | NoSolution | Branches [a] deriving Show

dpll' :: (Ord a) => CNF a -> Result (CNF a)
dpll' cnf
  | isConsistentSetOfLiterals cnf = Solution cnf
  | hasEmptyClauses           cnf = NoSolution
  | otherwise                     = Branches $ makeBranches $ dpllStep cnf

treeSearch :: (a -> Result a) -> a -> Maybe a
treeSearch step cnf = case step cnf of
  Solution cnf' -> Just cnf'
  NoSolution    -> Nothing
  Branches branches -> seqMaybe $ map (treeSearch step) branches

dpll :: (Ord a) => CNF a -> Maybe (CNF a)
dpll = treeSearch dpll'

dpllStep :: (Ord a) => CNF a -> CNF a
dpllStep = eliminateAllPureLiterals . unitPropagateAll
