{-# LANGUAGE DeriveGeneric #-}

module Main where

import CNF(CNF(..))
import qualified Data.Set as Set
import DPLL
import Sudoku (sudokuCnf, toBoard, evilData)

main :: IO ()
main = do
  let cnf = sudokuCnf 3 <> toBoard evilData
  let result = dpll cnf
  case result of
    Just _ -> putStrLn "solution"
    Nothing -> putStrLn "no solution"
