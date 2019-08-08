{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module CNF where

import qualified Data.List as List
import qualified Data.Set as Set
import GHC.Generics
import Generic.Random
import Test.QuickCheck

data Lit a = Pos a | Neg a deriving (Eq, Ord, Generic)
instance (Show a) => Show (Lit a) where
  show (Pos a) = show a
  show (Neg a) = "¬" ++ show a
instance (Arbitrary a) => Arbitrary (Lit a) where
  arbitrary = genericArbitraryU

data Clause a = Disj (Set.Set (Lit a)) deriving (Eq, Generic, Ord)
instance (Show a) => Show (Clause a) where
  show (Disj literals)
    | Set.null literals = "∅"
    | otherwise        = "(" ++ (List.intercalate " ∨ " $ showItems) ++ ")"
      where showItems = Set.toList $ Set.map show literals
instance (Arbitrary a, Ord a) => Arbitrary (Clause a) where
  arbitrary = Disj <$> do
    -- for all a, don't generate a disjunction containing both a and ¬a
    lits <- suchThat' largeEnough $ suchThat' isConsistent $ arbitrary
    return lits
    where
      suchThat' = flip suchThat

      largeEnough lits = (Set.size lits) `elem` [1, 2, 3]

data CNF a = CNF (Set.Set (Clause a)) deriving (Eq, Generic)
instance (Show a) => Show (CNF a) where
  show (CNF clauses)
    | Set.null clauses = "{}"
    | otherwise        = List.intercalate " ∧ " showItems
      where showItems =  foldMap (\clause -> [show clause]) clauses
instance (Arbitrary a, Ord a) => Arbitrary (CNF a) where
  arbitrary = genericArbitraryU
instance (Ord a) => Semigroup (CNF a) where
  (<>) (CNF clauses) (CNF clauses') = CNF $ clauses <> clauses'
instance (Ord a) => Monoid (CNF a) where
  mempty = CNF mempty

invLit :: Lit a -> Lit a
invLit (Pos a) = Neg a
invLit (Neg a) = Pos a

isEmpty :: (Ord a) => Clause a -> Bool
isEmpty (Disj literals) = literals == mempty

allLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allLiterals (CNF clauses) = Set.unions $ Set.map (\(Disj xs) -> xs) clauses

mapMaybe :: (Foldable m, Ord b) => (a -> Maybe b) -> m a -> Set.Set b
mapMaybe f values = foldMap resultToSet values
  where resultToSet value = case f value of
                              Just result -> Set.singleton result
                              Nothing     -> Set.empty

posAtoms :: (Foldable m, Ord a) => m (Lit a) -> Set.Set a
posAtoms = mapMaybe extractPos
  where
    extractPos (Pos a) = Just a
    extractPos _       = Nothing

negAtoms :: (Foldable m, Ord a) => m (Lit a) -> Set.Set a
negAtoms = mapMaybe extractNeg
  where
    extractNeg (Neg a) = Just a
    extractNeg _       = Nothing

extractUnitLiteral :: Clause a -> Maybe (Lit a)
extractUnitLiteral (Disj literals)
  | Set.size literals == 1 = Just $ Set.elemAt 0 literals
  | otherwise              = Nothing

extractDisjunction :: Clause a -> Maybe (Clause a)
extractDisjunction (Disj literals)
  | Set.size literals > 1  = Just (Disj literals)
  | otherwise              = Nothing

allUnitLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allUnitLiterals (CNF clauses) = mapMaybe extractUnitLiteral clauses

allDisjunctions :: (Ord a) => CNF a -> Set.Set (Clause a)
allDisjunctions (CNF clauses) = mapMaybe extractDisjunction clauses

makeUnitLiteral :: (Ord a) => Lit a -> Clause a
makeUnitLiteral lit = Disj $ Set.singleton lit

addUnitClause :: (Ord a) => Lit a -> CNF a -> CNF a
addUnitClause lit = (<>) (CNF $ Set.singleton $ makeUnitLiteral lit)

numDisjunctions :: (Ord a) => CNF a -> Int
numDisjunctions = length . allDisjunctions

numUnitLiterals :: (Ord a) => CNF a -> Int
numUnitLiterals  = length . allUnitLiterals

isConsistent :: (Foldable m, Ord a) => m (Lit a) -> Bool
isConsistent literals = mempty == (posAtoms literals) `Set.intersection` (negAtoms literals)

p = Pos
n = Neg

disj :: (Ord a) => [Lit a] -> Clause a
disj = Disj . Set.fromList

cnf :: (Ord a) => [Clause a] -> CNF a
cnf  = CNF  . Set.fromList
