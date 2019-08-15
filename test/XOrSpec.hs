module XOrSpec where

import Data.List (nub)
import Test.QuickCheck

import CNF(Lit(..), CNF, cnf, disj, invLit, Polarity)
import EvalCNF
import XOr

suchThat' = flip suchThat

data PureLiterals a = Pure [Lit a] deriving Show
instance (Arbitrary a, Eq a) => Arbitrary (PureLiterals a) where
  arbitrary = Pure <$> do
    atoms <- suchThat' (nubIsLongerThan 2) arbitrary
    polarities <- arbitrary :: Gen [Polarity]
    pure $ map (uncurry Lit) (zip polarities $ nub atoms)
    where nubIsLongerThan n l = (length $ nub l) >= n
    
cnfOneLit :: (Ord a) => Lit a -> CNF a
cnfOneLit lit = cnf [disj [lit]]

addLits :: (Ord a) => [Lit a] -> CNF a -> CNF a
addLits lits cnf' = cnf' <> cnf (map (\lit -> disj [lit]) lits)

litsToCNF :: (Ord a) => [Lit a] -> CNF a
litsToCNF lits = toCNF $ XOrForm [XOr lits]

prop_convertXOrProducesEmptyCnf :: Bool
prop_convertXOrProducesEmptyCnf = emptyCnf == toCNF (xor :: XOrForm Int)
  where
    emptyCnf = cnf [disj []]
    xor = XOrForm [XOr []]

prop_convertXOrInvalidIfNoLitsTrue :: PureLiterals Int -> Bool
prop_convertXOrInvalidIfNoLitsTrue (Pure (lit:lits)) = not $ isValidCNF $ addLits invLits cnf
  where
    cnf = litsToCNF (lit:lits)
    invLits = map invLit (lit:lits)
prop_convertXOrInvalidIfNoLitsTrue _ = True    

prop_convertXOrValidIfOneLitIsTrue :: PureLiterals Int -> Bool
prop_convertXOrValidIfOneLitIsTrue (Pure (lit:lits)) = isValidCNF $ addLits unitLiterals cnf
  where
    cnf = litsToCNF (lit:lits)
    -- Set the first literal to true, set the others to false
    unitLiterals = (lit : map invLit lits)
prop_convertXOrValidIfOneLitIsTrue _ = True

prop_convertXOrInvalidIfMoreThanOneLitIsTrue :: PureLiterals Int -> Bool
prop_convertXOrInvalidIfMoreThanOneLitIsTrue (Pure (lit : lit' : lits)) = not $ isValidCNF $ addLits unitLiterals cnf
  where
    cnf = litsToCNF (lit : lit' : lits)
    -- Set two literals to true, set the others to false
    unitLiterals = (lit : lit' : map invLit lits)
prop_convertXOrInvalidIfMoreThanOneLitIsTrue _ = True
