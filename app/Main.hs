{-# LANGUAGE DeriveGeneric #-}

module Main where

import CNF(CNF(..))
import qualified Data.Set as Set
import DPLL
import Sudoku (easyData, sudokuCnf, toBoard)

main :: IO ()
main = do
  let cnf = sudokuCnf <> toBoard easyData
  let result = treeSearchN dpll' 2 (Set.empty, cnf)
  case result of
    Just _ -> putStrLn "solution"
    Nothing -> putStrLn "no solution"
