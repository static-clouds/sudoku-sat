{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module CNF where

import qualified Data.List as List
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

invert :: Polarity -> Polarity
invert Pos = Neg
invert Neg = Pos

invLit :: Lit a -> Lit a
invLit (Lit p a) = Lit (invert p) a

isEmpty :: (Ord a) => Clause a -> Bool
isEmpty (Disj literals) = literals == mempty

allLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allLiterals (CNF clauses) = Set.unions $ Set.map (\(Disj xs) -> xs) clauses

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
extractUnitLiteral clause@(Disj literals)
  | isUnitLiteral clause = Just $ Set.elemAt 0 literals
  | otherwise            = Nothing

allUnitLiterals :: (Ord a) => CNF a -> Set.Set (Lit a)
allUnitLiterals (CNF clauses) = mapMaybe extractUnitLiteral clauses

allDisjunctions :: (Ord a) => CNF a -> Set.Set (Clause a)
allDisjunctions (CNF clauses) = Set.filter (not . isUnitLiteral) clauses

isUnitLiteral :: Clause a -> Bool
isUnitLiteral (Disj literals) = Set.size literals == 1

removeLiteral :: (Ord a) => Lit a -> Clause a -> Clause a
removeLiteral lit (Disj lits) = Disj $ Set.delete lit lits

makeUnitLiteral :: (Ord a) => Lit a -> Clause a
makeUnitLiteral lit = Disj $ Set.singleton lit

addUnitClause :: (Ord a) => Lit a -> CNF a -> CNF a
addUnitClause lit = (<>) (CNF $ Set.singleton $ makeUnitLiteral lit)

numDisjunctions :: (Ord a) => CNF a -> Int
numDisjunctions = length . allDisjunctions

numUnitLiterals :: (Ord a) => CNF a -> Int
numUnitLiterals  = length . allUnitLiterals

isConsistent :: (Ord a) => Set.Set (Lit a) -> Bool
isConsistent literals = mempty == (posAtoms literals) `Set.intersection` (negAtoms literals)

litInClause :: (Eq a) => Lit a -> Clause a -> Bool
litInClause lit (Disj xs) = lit `elem` xs

mapClauses :: (Ord a) => (Clause a -> Clause a) -> CNF a -> CNF a
mapClauses f (CNF clauses) = CNF $ Set.map f clauses

-- These three functions are similar to fmap, but I have chosen not to define
-- instances of Functor because these actually violate the Functor laws
-- (e.g. if a function being used to map is not injective)
mapCnf :: (Ord a, Ord b) => (a -> b) -> CNF a -> CNF b
mapCnf f (CNF clauses) = CNF $ Set.map (mapClause f) clauses

mapClause :: (Ord a, Ord b) => (a -> b) -> Clause a -> Clause b
mapClause f (Disj lits) = Disj $ Set.map (mapLit f) lits

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
disj = Disj . Set.fromList

cnf :: (Ord a) => [Clause a] -> CNF a
cnf  = CNF  . Set.fromList
