{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module CNF where

import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Set as Set
import GHC.Generics
import Generic.Random
import Test.QuickCheck

data Polarity = Pos | Neg deriving (Eq, Ord, Generic)
instance Arbitrary Polarity where
  arbitrary = genericArbitraryU

data Lit a = Lit Polarity a deriving (Eq, Ord, Generic)
instance (Show a) => Show (Lit a) where
  show (Lit Pos a) = show a
  show (Lit Neg a) = "¬" ++ show a
instance (Arbitrary a) => Arbitrary (Lit a) where
  arbitrary = genericArbitraryU

data Clause a = Disj (Set.Set (Lit a)) | UnitLiteral (Lit a) | Empty deriving (Eq, Generic, Ord)
instance (Show a) => Show (Clause a) where
  show (Disj literals) = "(" ++ (List.intercalate " ∨ " $ showItems) ++ ")"
    where showItems = Set.toList $ Set.map show literals
  show (UnitLiteral literal) = show literal
  show Empty = "∅"

instance (Arbitrary a, Ord a) => Arbitrary (Clause a) where
  arbitrary = clauseFromSet <$> do
    -- for all a, don't generate a disjunction containing both a and ¬a
    lits <- suchThat' largeEnough $ suchThat' isConsistent $ arbitrary
    return lits
    where
      suchThat' = flip suchThat

      largeEnough lits = (Set.size lits) `elem` [1, 2, 3]

-- Constructor for Clause
clauseFromSet :: (Ord a) => Set.Set (Lit a) -> Clause a
clauseFromSet lits = case Set.size lits of
  0 -> Empty
  1 -> UnitLiteral $ Set.elemAt 0 lits
  _ -> Disj lits


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

invert :: Polarity -> Polarity
invert Pos = Neg
invert Neg = Pos

invLit :: Lit a -> Lit a
invLit (Lit p a) = Lit (invert p) a

isEmpty :: (Ord a) => Clause a -> Bool
isEmpty Empty = True
isEmpty _     = False

clauseToSet Empty           = Set.empty
clauseToSet (UnitLiteral a) = Set.singleton a
clauseToSet (Disj lits)     = lits

allLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allLiterals (CNF clauses) = Set.unions $ Set.map clauseToSet clauses

mapMaybe :: (Foldable m, Ord b) => (a -> Maybe b) -> m a -> Set.Set b
mapMaybe f values = foldMap resultToSet values
  where resultToSet value = case f value of
                              Just result -> Set.singleton result
                              Nothing     -> Set.empty

literalAtom :: Lit a -> a
literalAtom (Lit _ a) = a

hasPolarity :: Polarity -> Lit a -> Bool
hasPolarity p (Lit p' _) = p == p'

posAtoms :: (Ord a) => Set.Set (Lit a) -> Set.Set a
posAtoms = (Set.map literalAtom) . Set.filter (hasPolarity Pos)

negAtoms :: (Ord a) => Set.Set (Lit a) -> Set.Set a
negAtoms = (Set.map literalAtom) . Set.filter (hasPolarity Neg)

extractUnitLiteral :: Clause a -> Maybe (Lit a)
extractUnitLiteral (UnitLiteral lit) = Just lit
extractUnitLiteral _                 = Nothing

allUnitLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allUnitLiterals (CNF clauses) = mapMaybe extractUnitLiteral clauses

allDisjunctions :: (Ord a) => CNF a -> Set.Set (Clause a)
allDisjunctions (CNF clauses) = Set.filter isDisjunction clauses

isUnitLiteral :: Clause a -> Bool
isUnitLiteral = isJust . extractUnitLiteral

isDisjunction :: Clause a -> Bool
isDisjunction (Disj _) = True
isDisjunction _        = False

removeLiteral :: (Ord a) => Lit a -> Clause a -> Clause a
removeLiteral lit = clauseFromSet . (Set.delete lit) . clauseToSet

makeUnitLiteral :: (Ord a) => Lit a -> Clause a
makeUnitLiteral = UnitLiteral

addUnitClause :: (Ord a) => Lit a -> CNF a -> CNF a
addUnitClause lit = (<>) (CNF $ Set.singleton $ makeUnitLiteral lit)

numDisjunctions :: (Ord a) => CNF a -> Int
numDisjunctions = length . allDisjunctions

numUnitLiterals :: (Ord a) => CNF a -> Int
numUnitLiterals  = length . allUnitLiterals

isConsistent :: (Ord a) => Set.Set (Lit a) -> Bool
isConsistent literals = mempty == (posAtoms literals) `Set.intersection` (negAtoms literals)

litInClause :: (Eq a) => Lit a -> Clause a -> Bool
litInClause _ Empty                = False
litInClause lit (UnitLiteral lit') = lit == lit'
litInClause lit (Disj xs)          = lit `elem` xs

mapClauses :: (Ord a) => (Clause a -> Clause a) -> CNF a -> CNF a
mapClauses f (CNF clauses) = CNF $ Set.map f clauses

-- These three functions are similar to fmap, but I have chosen not to define
-- instances of Functor because these actually violate the Functor laws
-- (e.g. if a function being used to map is not injective)
mapCnf :: (Ord a, Ord b) => (a -> b) -> CNF a -> CNF b
mapCnf f (CNF clauses) = CNF $ Set.map (mapClause f) clauses

mapClause :: (Ord a, Ord b) => (a -> b) -> Clause a -> Clause b
mapClause f (Disj lits) = clauseFromSet $ Set.map (mapLit f) lits

mapLit :: (Ord a, Ord b) => (a -> b) -> Lit a -> Lit b
mapLit f (Lit p a) = Lit p (f a)


fromUnitLiterals :: (Ord a) => Set.Set (Lit a) -> CNF a
fromUnitLiterals = CNF . Set.map makeUnitLiteral


makePos :: a -> Lit a
makePos = Lit Pos

makeNeg :: a -> Lit a
makeNeg = Lit Neg


p = Lit Pos
n = Lit Neg

disj :: (Ord a) => [Lit a] -> Clause a
disj = clauseFromSet . Set.fromList

cnf :: (Ord a) => [Clause a] -> CNF a
cnf  = CNF  . Set.fromList
