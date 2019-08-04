import Test.QuickCheck

import CNF
import DPLL(dpll)
import EvalCNF(isValidCNF)


main :: IO ()
main = do
  quickCheck prop_dpllMakesCNFValid

prop_dpllMakesCNFValid :: CNF -> Bool
prop_dpllMakesCNFValid cnf = case dpll cnf of
  Just cnf' -> isValidCNF cnf'
  Nothing   -> True
