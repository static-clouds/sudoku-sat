import Test.QuickCheck

import CNF
import DPLL(dpll)
import EvalCNF(isValidCNF)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set


main :: IO ()
main = do
  quickCheck prop_dpllMakesCNFValid
  quickCheck prop_dpllMakesCNFValidSimple

prop_dpllMakesCNFValid :: CNF -> Bool
prop_dpllMakesCNFValid cnf = case dpll cnf of
  -- Can the result of dpll cnf be used to solve cnf?
  Just cnf' -> isValidCNF (combine cnf cnf')
  Nothing   -> True


genSimpleCNF :: Gen CNF
genSimpleCNF = elements $ [ cnf [ disj [ p "a"]
                                , disj [ n "a", n "b"]
                                ]
                          ]

prop_dpllMakesCNFValidSimple :: Gen Bool
prop_dpllMakesCNFValidSimple = do
  cnf <- genSimpleCNF
  pure $ isJust $ dpll cnf
