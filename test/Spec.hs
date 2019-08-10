{-# language DeriveGeneric #-}
import Test.QuickCheck

import CNF
import DPLL(dpll, dpll', Result(..), fromDPLLState)
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
  quickCheck prop_dpllMakesCNFValidComplex
  quickCheck prop_dpllShrinksInput

prop_dpllMakesCNFValid :: CNF TestAtom -> Bool
prop_dpllMakesCNFValid cnf = case dpll cnf of
  -- Can the result of dpll cnf be used to solve cnf?
  Just cnf' -> isValidCNF $ cnf <> cnf'
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


genComplexCNF :: Gen (CNF TestAtom)
genComplexCNF = elements $ [ cnf [ disj [ n AA, p AB, p AC]
                                 , disj [ p AA, p AC, p AD]
                                 , disj [ p AA, p AC, n AD]
                                 , disj [ p AA, n AC, p AD]
                                 , disj [ p AA, n AC, n AD]
                                 , disj [ n AA, p AB, n AC]
                                 , disj [ n AA, n AB, p AC]
                                 ]
                           ]

prop_dpllMakesCNFValidComplex :: Gen Bool
prop_dpllMakesCNFValidComplex = do
  cnf <- genComplexCNF
  pure $ case dpll cnf of
    Just cnf' -> isValidCNF $ cnf <> cnf'
    -- if no solution can be found, fail
    Nothing   -> False


prop_dpllShrinksInput :: CNF TestAtom -> Bool
prop_dpllShrinksInput cnf = case dpll' (Set.empty, cnf) of
  Branches successors -> all (isSmaller . fromDPLLState) successors
  _             -> True
  where
    isSmaller successor = moreUnitLiterals successor || fewerDisjunctions successor
    moreUnitLiterals successor = numUnitLiterals successor > numUnitLiterals cnf
    fewerDisjunctions successor = numDisjunctions successor < numDisjunctions cnf
