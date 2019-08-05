{-# LANGUAGE DeriveGeneric #-}
import Test.QuickCheck

import CNF
import DPLL(dpll)
import EvalCNF(isValidCNF)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Generic.Random
import GHC.Generics


data TestAtom = AA | AB | AC | AD deriving (Eq, Generic, Ord, Show)
instance Arbitrary TestAtom where
  arbitrary = genericArbitraryU

main :: IO ()
main = do
  quickCheck prop_dpllMakesCNFValid
  quickCheck prop_dpllMakesCNFValidSimple

prop_dpllMakesCNFValid :: CNF TestAtom -> Bool
prop_dpllMakesCNFValid cnf = case dpll cnf of
  -- Can the result of dpll cnf be used to solve cnf?
  Just cnf' -> isValidCNF (combine cnf cnf')
  Nothing   -> True


genSimpleCNF :: Gen (CNF TestAtom)
genSimpleCNF = elements $ [ cnf [ disj [ p AA]
                                , disj [ n AA, n AB]
                                ]
                          ]

prop_dpllMakesCNFValidSimple :: Gen Bool
prop_dpllMakesCNFValidSimple = do
  cnf <- genSimpleCNF
  pure $ isJust $ dpll cnf
