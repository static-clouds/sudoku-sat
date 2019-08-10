module EvalCNF where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import CNF(Clause(..), CNF(..), Lit(..), Polarity(..), allUnitLiterals, allDisjunctions)

type AtomMapping a = Map.Map a Bool

truthValue :: Polarity -> Bool
truthValue Pos = True
truthValue Neg = False

evaluateLit :: Lit Bool -> Bool
evaluateLit (Lit polarity value) = value == truthValue polarity

evaluateClause :: Clause Bool -> Bool
evaluateClause (Disj lits) = any evaluateLit lits

evaluateCnf :: CNF Bool -> Bool
evaluateCnf (CNF clauses) = all evaluateClause clauses

replaceAtomsInCnf :: (Ord a) => AtomMapping a -> CNF a -> CNF Bool
replaceAtomsInCnf mapping (CNF clauses) = CNF $ Set.map (replaceAtomsInClause mapping) clauses

replaceAtomsInClause :: (Ord a) => AtomMapping a -> Clause a -> Clause Bool
replaceAtomsInClause mapping (Disj lits) = Disj $ Set.map (replaceAtomsInLit mapping) lits

replaceAtomsInLit :: (Ord a) => AtomMapping a -> Lit a -> Lit Bool
replaceAtomsInLit mapping (Lit p a) = Lit p a'
  where a' = Map.findWithDefault False a mapping

makeAtomMapping :: (Ord a) => Set.Set (Lit a) -> AtomMapping a
makeAtomMapping literals = Map.fromList pairs
  where
    pairs = Set.toList $ Set.map (\(Lit p a) -> (a, truthValue p)) literals

isValidCNF :: (Ord a) => CNF a -> Bool
isValidCNF cnf = evaluateCnf $ replaceAtomsInCnf atomMapping cnfToSolve
  where
    atomMapping = makeAtomMapping literals
    literals = allUnitLiterals cnf
    cnfToSolve = CNF $ allDisjunctions cnf
