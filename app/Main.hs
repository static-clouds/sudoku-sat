module Main where

import qualified Data.List as List
import qualified Data.Set  as Set

import Lib

data Atom = A String deriving (Eq, Ord)
instance Show Atom where
  show (A s) = s

data Lit = Pos Atom | Neg Atom deriving (Eq, Ord)
instance Show Lit where
  show (Pos a) = show a
  show (Neg a) = "¬" ++ show a

data Clause = Disj [Lit] deriving Eq
instance Show Clause where
  show (Disj []) = "∅"
  show (Disj xs) = "(" ++ (List.intercalate " ∨ " $ map show xs) ++ ")"
  
data CNF = CNF [Clause] deriving Eq
instance Show CNF where
  show (CNF [])  = "{}"
  show (CNF xs)  = List.intercalate " ∧ " $ map show xs

isPos :: Lit -> Bool
isPos (Pos _) = True
isPos (Neg _) = False

isNeg :: Lit -> Bool
isNeg = isPos . invLit

invLit :: Lit -> Lit
invLit (Pos a) = Neg a
invLit (Neg a) = Pos a

isEmpty :: Clause -> Bool
isEmpty (Disj []) = True
isEmpty _         = False

isLiteral :: Clause -> Bool
isLiteral (Disj [lit]) = True
isLiteral _            = False

splitAtoms :: [Lit] -> ([Lit], [Lit])
splitAtoms = List.partition isPos 

isConsistentSetOfLiterals :: CNF -> Bool
isConsistentSetOfLiterals (CNF clauses) = isAllLiterals && isConsistent
  where
    isAllLiterals = all isLiteral clauses
    literals = map (\(Disj lits) -> head lits) clauses
    (posLits, negLits) = List.partition isPos literals
    posAtoms = Set.fromList $ map (\(Pos a) -> a) posLits
    negAtoms = Set.fromList $ map (\(Neg a) -> a) negLits
    isConsistent = Set.null $ posAtoms `Set.intersection` negAtoms

hasEmptyClauses :: CNF -> Bool
hasEmptyClauses (CNF clauses) = any isEmpty clauses

unitPropagate :: Lit -> Clause -> Clause
unitPropagate a (Disj xs) = Disj ns
  where
    ns
      | xs == [a]    = xs
      | a  `elem` xs = []
      | a' `elem` xs = List.delete a' xs
      | otherwise    = xs
    a' = invLit a

unitPropagate' :: Lit -> CNF -> CNF
unitPropagate' a (CNF clauses) = CNF $ map (unitPropagate a) clauses


allLiterals :: CNF -> Set.Set Lit
allLiterals (CNF clauses) = Set.fromList $ map extractLiteral $ filter isLiteral clauses
  where extractLiteral (Disj [a]) = a

unitPropagateAll :: Set.Set Lit -> CNF -> CNF
unitPropagateAll propagatedLits cnf
  | allLiterals' == propagatedLits = cnf
  | otherwise                      = unitPropagateAll (Set.insert nextLiteral propagatedLits) (unitPropagate' nextLiteral cnf)
  where
    allLiterals' = allLiterals cnf
    unusedLiterals = allLiterals' `Set.difference` propagatedLits
    nextLiteral = Set.elemAt 0 unusedLiterals


main :: IO ()
main = someFunc
