module Sudoku where

import CNF
import GHC.Generics
import Test.QuickCheck


data SudokuCellAtom = C { row :: Int, col :: Int, val :: Int } deriving (Eq, Generic, Ord, Show)
