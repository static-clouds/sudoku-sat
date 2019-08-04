{-# LANGUAGE DeriveGeneric #-}
module CNF where

import qualified Data.List as List
import qualified Data.Set as Set
import GHC.Generics
import Generic.Random
import Test.QuickCheck

data Atom = A String deriving (Eq, Ord, Generic)
instance Show Atom where
  show (A s) = s
instance Arbitrary Atom where
  arbitrary = do
    s <- elements ["a", "b"]
    pure $ A s

data Lit = Pos Atom | Neg Atom deriving (Eq, Ord, Generic)
instance Show Lit where
  show (Pos a) = show a
  show (Neg a) = "¬" ++ show a
instance Arbitrary Lit where
  arbitrary = genericArbitraryU

data Clause = Disj (Set.Set Lit) deriving (Eq, Generic, Ord)
instance Show Clause where
  show (Disj literals)
    | Set.null literals = "∅"
    | otherwise        = "(" ++ (List.intercalate " ∨ " $ showItems) ++ ")"
      where showItems = Set.toList $ Set.map show literals
instance Arbitrary Clause where
  arbitrary = Disj <$> do
    -- for all a, don't generate a disjunction containing both a and ¬a
    lits <- suchThat' largeEnough $ suchThat' isConsistent $ arbitrary
    return lits
    where
      suchThat' = flip suchThat
      largeEnough lits = (Set.size lits) `elem` [1,2]


data CNF = CNF (Set.Set Clause) deriving (Eq, Generic)
instance Show CNF where
  show (CNF clauses)
    | Set.null clauses = "{}"
    | otherwise        = List.intercalate " ∧ " showItems
      where showItems = Set.toList $ Set.map show clauses
instance Arbitrary CNF where
  arbitrary = genericArbitraryU

invLit :: Lit -> Lit
invLit (Pos a) = Neg a
invLit (Neg a) = Pos a

isEmpty :: Clause -> Bool
isEmpty (Disj literals) = Set.null literals

allLiterals :: CNF -> Set.Set Lit
allLiterals (CNF clauses) = Set.unions $ Set.map (\(Disj xs) -> xs) clauses

mapMaybe :: (Ord b) => (a -> Maybe b) -> Set.Set a -> Set.Set b
mapMaybe f values = foldMap resultToSet values
  where resultToSet value = case f value of
                              Just result -> Set.singleton result
                              Nothing     -> Set.empty

posAtoms :: Set.Set Lit -> Set.Set Atom
posAtoms = mapMaybe extractPos
  where
    extractPos (Pos a) = Just a
    extractPos _       = Nothing

negAtoms :: Set.Set Lit -> Set.Set Atom
negAtoms = mapMaybe extractNeg
  where
    extractNeg (Neg a) = Just a
    extractNeg _       = Nothing

extractUnitLiteral :: Clause -> Maybe Lit
extractUnitLiteral (Disj literals)
  | Set.size literals == 1 = Just $ Set.elemAt 0 literals
  | otherwise              = Nothing

extractDisjunction :: Clause -> Maybe Clause
extractDisjunction (Disj literals)
  | Set.size literals > 1  = Just (Disj literals)
  | otherwise              = Nothing

allUnitLiterals :: CNF -> Set.Set Lit
allUnitLiterals (CNF clauses) = mapMaybe extractUnitLiteral clauses

allDisjunctions :: CNF -> Set.Set Clause
allDisjunctions (CNF clauses) = mapMaybe extractDisjunction clauses

addUnitClause :: Lit -> CNF -> CNF
addUnitClause lit (CNF clauses) = CNF $ Set.insert (Disj $ Set.singleton lit) clauses

isConsistent :: Set.Set Lit -> Bool
isConsistent literals = Set.null $ (posAtoms literals) `Set.intersection` (negAtoms literals)
