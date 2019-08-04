module Main where

import qualified Data.List as List
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
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
    isConsistent = Set.null $ (posAtoms literals) `Set.intersection` (negAtoms literals)

hasEmptyClauses :: CNF -> Bool
hasEmptyClauses (CNF clauses) = any isEmpty clauses

unitPropagate :: Lit -> Clause -> Maybe Clause
unitPropagate lit (Disj xs)
      | lit  `elem` xs = Nothing
      | xs == [lit]    = Just $ Disj xs
      | lit' `elem` xs = Just $ Disj $ List.delete lit' xs
      | otherwise      = Just $ Disj xs
  where lit' = invLit lit

unitPropagate' :: Lit -> CNF -> CNF
unitPropagate' a (CNF clauses) = CNF $ catMaybes $ map (unitPropagate a) clauses

eliminateLiteralFromClause :: Lit -> Clause -> Clause
eliminateLiteralFromClause lit (Disj xs) = Disj ns
  where
    ns
      | lit `elem` xs = List.delete lit xs
      | otherwise     = xs

eliminateLiteral :: Lit -> CNF -> CNF
eliminateLiteral lit (CNF clauses) = CNF $ map (eliminateLiteralFromClause lit) clauses

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

allPureLiterals :: CNF -> Set.Set Lit
allPureLiterals cnf = purePosLiterals `Set.union` pureNegLiterals
  where
    purePosLiterals = Set.map Pos $ posAtoms' `Set.difference` negAtoms'
    pureNegLiterals = Set.map Neg $ negAtoms' `Set.difference` posAtoms'
    posAtoms' = posAtoms $ allLiterals cnf
    negAtoms' = negAtoms $ allLiterals cnf

eliminateAllPureLiterals :: CNF -> CNF
eliminateAllPureLiterals cnf = Set.fold eliminateLiteral cnf (allPureLiterals cnf)

allUnitLiterals :: CNF -> Set.Set Lit
allUnitLiterals (CNF clauses) = Set.fromList $ mapMaybe extractUnitLiteral clauses
  where
    extractUnitLiteral (Disj [a]) = Just a
    extractUnitLiteral _          = Nothing

allUnitLiteralsPropagated :: (Set.Set Lit, CNF) -> Bool
allUnitLiteralsPropagated (propagated, cnf) = (allUnitLiterals cnf) == propagated

propagateUnitLiterals :: (Set.Set Lit, CNF) -> (Set.Set Lit, CNF)
propagateUnitLiterals (propagated, cnf) = (Set.insert nextLiteral propagated, unitPropagate' nextLiteral cnf)
  where
    allLiterals' = allUnitLiterals cnf
    unusedLiterals = allLiterals' `Set.difference` propagated
    nextLiteral = Set.elemAt 0 unusedLiterals

unitPropagateAll :: CNF -> CNF
unitPropagateAll cnf = snd $ until allUnitLiteralsPropagated propagateUnitLiterals (Set.empty, cnf)

headMaybe :: [a] -> Maybe a
headMaybe (x:xs) = Just x
headMaybe _      = Nothing

chooseLiteral :: CNF -> Maybe Lit
chooseLiteral cnf = headMaybe $ Set.toList lits
  where
    lits = allLiterals cnf `Set.difference` allUnitLiterals cnf

dpll :: CNF -> Maybe CNF
dpll cnf
  | isConsistentSetOfLiterals cnf = Just cnf
  | hasEmptyClauses           cnf = Nothing
  | otherwise                     = listToMaybe . mapMaybe dpll $ (updateDpll cnf)

addLiteral :: Lit -> CNF -> CNF
addLiteral lit (CNF clauses) = CNF $ (Disj [lit]):clauses

makeBranches :: CNF -> [CNF]
makeBranches cnf = case chooseLiteral cnf of
  Just lit -> [addLiteral lit cnf, addLiteral (invLit lit) cnf]
  Nothing  -> []

updateDpll :: CNF -> [CNF]
updateDpll = makeBranches . eliminateAllPureLiterals . unitPropagateAll

main :: IO ()
main = someFunc
