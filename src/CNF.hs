{-# LANGUAGE DeriveGeneric #-}
module CNF where

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC.Generics
import Generic.Random
import Test.QuickCheck

data Atom = A String deriving (Eq, Ord, Generic)
instance Show Atom where
  show (A s) = s
instance Arbitrary Atom where
  arbitrary = genericArbitraryU

data Lit = Pos Atom | Neg Atom deriving (Eq, Ord, Generic)
instance Show Lit where
  show (Pos a) = show a
  show (Neg a) = "¬" ++ show a
instance Arbitrary Lit where
  arbitrary = genericArbitraryU

data Clause = Disj [Lit] deriving (Eq, Generic)
instance Show Clause where
  show (Disj []) = "∅"
  show (Disj xs) = "(" ++ (List.intercalate " ∨ " $ map show xs) ++ ")"
instance Arbitrary Clause where
  arbitrary = genericArbitraryU

data CNF = CNF [Clause] deriving (Eq, Generic)
instance Show CNF where
  show (CNF [])  = "{}"
  show (CNF xs)  = List.intercalate " ∧ " $ map show xs
instance Arbitrary CNF where
  arbitrary = genericArbitraryU

invLit :: Lit -> Lit
invLit (Pos a) = Neg a
invLit (Neg a) = Pos a

isEmpty :: Clause -> Bool
isEmpty (Disj []) = True
isEmpty _         = False

allLiterals :: CNF -> [Lit]
allLiterals (CNF clauses) = List.concatMap (\(Disj xs) -> xs) $ clauses

posAtoms :: [Lit] -> Set.Set Atom
posAtoms = Set.fromList . (mapMaybe extractPos)
  where
    extractPos (Pos a) = Just a
    extractPos _       = Nothing

negAtoms :: [Lit] -> Set.Set Atom
negAtoms = Set.fromList . (mapMaybe extractNeg)
  where
    extractNeg (Neg a) = Just a
    extractNeg _       = Nothing

extractUnitLiteral :: Clause -> Maybe Lit
extractUnitLiteral (Disj [lit]) = Just lit
extractUnitLiteral _            = Nothing

extractDisjunction :: Clause -> Maybe [Lit]
extractDisjunction (Disj lits)
  | List.length lits >= 2 = Just lits
  | otherwise             = Nothing

allUnitLiterals :: CNF -> Set.Set Lit
allUnitLiterals (CNF clauses) = Set.fromList $ mapMaybe extractUnitLiteral clauses

allDisjunctions :: CNF -> Set.Set [Lit]
allDisjunctions (CNF clauses) = Set.fromList $ mapMaybe extractDisjunction clauses

addUnitClause :: Lit -> CNF -> CNF
addUnitClause lit (CNF clauses) = CNF $ (Disj [lit]):clauses
