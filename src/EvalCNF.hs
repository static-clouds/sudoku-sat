module EvalCNF where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import CNF(Clause(..), CNF(..), Lit(..), allUnitLiterals, allDisjunctions)

type AtomMapping a = Map.Map a Bool

truthValue :: Lit a -> Bool
truthValue (Pos _) = True
truthValue (Neg _) = False

unwrapLit :: Lit a -> a
unwrapLit (Pos a) = a
unwrapLit (Neg a) = a

disjunctionIsTrue :: (Ord a) => AtomMapping a -> Clause a -> Bool
disjunctionIsTrue atomMapping (Disj lits') = any evaluate $ zip lits values
  where
    lits = Set.toList lits'
    atoms = map unwrapLit lits
    values = map (\atom -> Map.findWithDefault False atom atomMapping) $ atoms
    evaluate ((Pos _), True ) = True
    evaluate ((Neg _), False) = True
    evaluate _                = False

isValidClause :: (Ord a) => AtomMapping a -> Clause a -> Bool
isValidClause atomMapping clause = disjunctionIsTrue atomMapping clause

makeAtomMapping :: (Ord a) => Set.Set (Lit a) -> AtomMapping a
makeAtomMapping literals = Map.mapKeys unwrapLit $ Map.fromSet truthValue literals

isValidCNF :: (Ord a) => CNF a -> Bool
isValidCNF cnf@(CNF clauses) = all (isValidClause atomMapping) disjunctions
  where
    atomMapping = makeAtomMapping literals
    literals = allUnitLiterals cnf
    disjunctions = allDisjunctions cnf
