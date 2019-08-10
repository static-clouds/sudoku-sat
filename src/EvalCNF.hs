module EvalCNF where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import CNF(Clause(..), CNF(..), Lit(..), Polarity(..), allUnitLiterals, allDisjunctions, mapCnf)

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

makeAtomMapping :: (Ord a) => Set.Set (Lit a) -> AtomMapping a
makeAtomMapping literals = Map.fromList pairs
  where
    pairs = Set.toList $ Set.map (\(Lit p a) -> (a, truthValue p)) literals

isValidCNF :: (Ord a) => CNF a -> Bool
isValidCNF cnf = evaluateCnf $ mapCnf replaceAtoms cnfToSolve
  where
    replaceAtoms a = Map.findWithDefault False a atomMapping
    atomMapping = makeAtomMapping literals
    literals = allUnitLiterals cnf
    cnfToSolve = CNF $ allDisjunctions cnf
