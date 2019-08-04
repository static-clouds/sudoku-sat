module EvalCNF where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import CNF(Atom, Clause(..), CNF(..), Lit(..), allUnitLiterals, allDisjunctions)

type AtomMapping = Map.Map Atom Bool

truthValue :: Lit -> Bool
truthValue (Pos _) = True
truthValue (Neg _) = False

unwrapLit :: Lit -> Atom
unwrapLit (Pos a) = a
unwrapLit (Neg a) = a

coveredByMapping :: AtomMapping -> Clause -> Bool
coveredByMapping atomMapping (Disj lits) = all isJust $ Set.map (flip Map.lookup atomMapping) atoms
  where atoms = Set.map unwrapLit lits

disjunctionIsTrue :: AtomMapping -> Clause -> Bool
disjunctionIsTrue atomMapping (Disj lits') = any evaluate $ zip lits values
  where
    lits = Set.toList lits'
    atoms = map unwrapLit lits
    values = map (fromJust . flip Map.lookup atomMapping) $ atoms
    evaluate ((Pos _), True ) = True
    evaluate ((Neg _), False) = True
    evaluate _                = False

isValidClause :: AtomMapping -> Clause -> Bool
isValidClause atomMapping clause = coveredByMapping atomMapping clause && disjunctionIsTrue atomMapping clause

isValidCNF :: CNF -> Bool
isValidCNF cnf@(CNF clauses) = all (isValidClause atomMapping) disjunctions
  where
    atomMapping = Map.mapKeys unwrapLit $ Map.fromSet truthValue literals
    literals = allUnitLiterals cnf
    disjunctions = allDisjunctions cnf
