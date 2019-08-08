module Sudoku where

import CNF
import GHC.Generics
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Test.QuickCheck

allPositions = [(r, c) | r <- [0..8], c <- [0..8]]

toVal :: Char -> Maybe Int
toVal '-' = Nothing
toVal s   = Just (read [s] :: Int)

toMoves :: String -> [SudokuCellAtom]
toMoves s = map (uncurry3 C) allValues
  where
    allValues = [(row, col, fromJust val) | (row, col) <- allPositions
                                          , let val' = board !! row !! col
                                          , let val = toVal val'
                                          , isJust val]
    board     = splitOn "X" s

toBoard :: String -> CNF SudokuCellAtom
toBoard = toCNF . toMoves
  where
    toCNF :: [SudokuCellAtom] -> CNF SudokuCellAtom
    toCNF = CNF . Set.fromList . map (makeUnitLiteral . Pos)

data SudokuCellAtom = C { row :: Int, col :: Int, val :: Int } deriving (Eq, Ord)
instance Show SudokuCellAtom where
  show C { row = row, col = col, val = val } = "C" ++ show (row, col, val)

data Grid = Grid { sudokuSize :: Int } deriving (Show)

gridSpan = [0..8]

pos' (row, col, val) = Pos $ C { row = row, col = col, val = val }
neg' (row, col, val) = Neg $ C { row = row, col = col, val = val }

toDisj :: (Ord a) => [Lit a] -> Clause a
toDisj lits = Disj $ Set.fromList lits

posIf :: ((Int, Int, Int) -> Bool) -> [(Int, Int, Int)] -> [Lit SudokuCellAtom]
posIf f values = map innerF values
  where innerF a
          | f a       = pos' a
          | otherwise = neg' a

cellRule :: Int -> Int -> Int -> [Lit SudokuCellAtom]
cellRule row col val = posIf cond cellValues
  where
    cellValues = [(row, col, v') | v' <- gridSpan]
    cond (_, _, v') = v' == val

rowRule :: Int -> Int -> Int -> [Lit SudokuCellAtom]
rowRule row col val = posIf cond cellValues
  where
    cellValues = [(row, col', val) | col' <- gridSpan]
    cond (_, col', _) = col' == col

colRule :: Int -> Int -> Int -> [Lit SudokuCellAtom]
colRule row col val = posIf cond cellValues
  where
    cellValues = [(row', col, val) | row' <- gridSpan]
    cond (row', _, _) = row' == row

box' :: Int -> Int -> [(Int, Int)]
box' r_ c_ = [(r, c) | r <- [(r_*3)..(r_*3 + 2)], c <- [(c_*3)..(c_*3 + 2)]]

boxRule :: Int -> Int -> Int -> [Lit SudokuCellAtom]
boxRule row col val = posIf cond cellValues
  where
    boxRow = row `quot` 3
    boxCol = col `quot` 3
    cellValues = [(row, col, val) | (row, col) <- box' boxRow boxCol]
    cond (row', col',_) = row' == row && col' == col

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

sudokuCnf :: CNF SudokuCellAtom
sudokuCnf = CNF $ Set.fromList clauses
  where
    clauses = cellRules ++ rowRules ++ colRules ++ boxRules
    params = [(row, col, val) | row <- gridSpan, col <- gridSpan, val <- gridSpan]
    cellRules = map (toDisj . uncurry3 cellRule) params
    rowRules  = map (toDisj . uncurry3 rowRule) params
    colRules  = map (toDisj . uncurry3 colRule) params
    boxRules  = map (toDisj . uncurry3 boxRule) params  


easyData   = "7-6-9--8-X-----69--X98-5-2-7-X312-4---5X---153---X4---6-318X-6-8-9-31X--73-----X-4--2-8-7"
