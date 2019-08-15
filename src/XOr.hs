module XOr where

import CNF(Lit, CNF(..), Clause, clauseFromList, invLit)
import Data.List (tails)
import qualified Data.Set as Set

data XOrClause a = XOr [Lit a] deriving Show
data XOrForm a = XOrForm [XOrClause a] deriving Show

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

toCNFClauses :: (Ord a) => XOrClause a -> [Clause a]
toCNFClauses (XOr lits) = map clauseFromList $ oneMustBeTrue : onlyOneIsTrue
  where
    oneMustBeTrue = lits
    onlyOneIsTrue = [[invLit lit1, invLit lit2] | (lit1, lit2) <- pairs lits]

toCNF :: (Ord a) => XOrForm a -> CNF a
toCNF (XOrForm xor) = CNF $ Set.fromList $ concatMap toCNFClauses xor
