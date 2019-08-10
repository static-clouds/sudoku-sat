{-# LANGUAGE DeriveGeneric #-}

module Main where

import CNF(CNF(..))
import DPLL
import Sudoku (easyData, sudokuCnf, toBoard)

main :: IO ()
main = do
  let cnf = sudokuCnf <> toBoard easyData
  let result = dpll' cnf
  case result of
    Branches _ -> putStrLn "branches"
    Solution _ -> putStrLn "solution"
    NoSolution -> putStrLn "no solution"
