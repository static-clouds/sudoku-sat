module DPLL where

import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import qualified Data.Set  as Set
import CNF (CNF(..), Lit, invLit, isEmpty, extractUnitLiteral, allLiterals, allUnitLiterals, addUnitClause, isConsistent, fromUnitLiterals)
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

makeBranches :: (Ord a) => DPLLState a -> [DPLLState a]
makeBranches (propagated, cnf) = case (chooseLiteral cnf) of
      Just newLiteral -> [ (propagated, addUnitClause newLiteral cnf)
                         , (propagated, addUnitClause (invLit newLiteral) cnf)
                         ]
      Nothing         -> [(propagated, cnf)]

data Result a = Solution a | NoSolution | Branches [a] deriving Show

type DPLLState a = (Set.Set (Lit a), CNF a)

dpll' :: (Ord a) => DPLLState a -> Result (DPLLState a)
dpll' (propagated, cnf)
  | isConsistentSetOfLiterals cnf = Solution (propagated, cnf)
  | hasEmptyClauses           cnf = NoSolution
  | otherwise                     = Branches $ makeBranches $ dpllStep (propagated, cnf)

treeSearch :: (a -> Result a) -> a -> Maybe a
treeSearch step cnf = case step cnf of
  Solution cnf' -> Just cnf'
  NoSolution    -> Nothing
  Branches branches -> seqMaybe $ map (treeSearch step) branches

treeSearchN :: (a -> Result a) -> Int -> a -> Maybe a
treeSearchN step n cnf
  | n == 0    = Nothing
  | otherwise = case step cnf of
      Solution cnf' -> Just cnf'
      NoSolution    -> Nothing
      Branches branches -> seqMaybe $ map (treeSearchN step $ n - 1) branches

fromDPLLState :: (Ord a) => DPLLState a -> CNF a
fromDPLLState (propagated, cnf) = cnf <> fromUnitLiterals propagated

dpll :: (Ord a) => CNF a -> Maybe (CNF a)
dpll cnf = case treeSearch dpll' (Set.empty, cnf) of
  Just (propagated, cnf') -> Just $ fromDPLLState (propagated, cnf')
  Nothing                 -> Nothing

dpllStep :: (Ord a) => (Set.Set (Lit a), CNF a) -> (Set.Set (Lit a), CNF a)
dpllStep = eliminateAllPureLiterals . unitPropagateAll
