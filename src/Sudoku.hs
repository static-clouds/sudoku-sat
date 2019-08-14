module Sudoku where

import CNF(CNF(..), Lit(..), Polarity(..), makeUnitLiteral, clauseFromSet)
import GHC.Generics
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Test.QuickCheck

allPositions :: Int -> [(Int, Int)]
allPositions n = [(r, c) | r <- [0..gridSize], c <- [0..gridSize]]
  where gridSize = (n * n) - 1

toVal :: Char -> Maybe Int
toVal '-' = Nothing
toVal s   = Just (read [s] :: Int)

toMoves :: String -> [SudokuCellAtom]
toMoves s = map (uncurry3 C) allValues
  where
    allValues = [(row, col, fromJust val) | (row, col) <- allPositions 9
                                          , let val' = board !! row !! col
                                          , let val = toVal val'
                                          , isJust val]
    board     = splitOn "X" s

toBoard :: String -> CNF SudokuCellAtom
toBoard = toCNF . toMoves
  where
    toCNF :: [SudokuCellAtom] -> CNF SudokuCellAtom
    toCNF = CNF . Set.fromList . map (makeUnitLiteral . Lit Pos)

data SudokuCellAtom = C { row :: Int, col :: Int, val :: Int } deriving (Eq, Ord)
instance Show SudokuCellAtom where
  show C { row = row, col = col, val = val } = "C" ++ show (row, col, val)

posIf :: (a -> Bool) -> a -> Lit a
posIf condition atom = Lit polarity atom
  where polarity
          | condition atom = Pos
          | otherwise      = Neg

gridSpan :: [Int]
gridSpan = [0..8]

makeRule :: (a -> Bool) -> [a] -> [Lit a]
makeRule cond atoms = map (posIf cond) atoms

matchesVal :: Int -> SudokuCellAtom -> Bool
matchesVal val (C {val = val'}) = val' == val

cellValues :: SudokuCellAtom -> [SudokuCellAtom]
cellValues cell = [cell { val = v' } | v' <- gridSpan]

cellRule :: SudokuCellAtom -> [Lit SudokuCellAtom]
cellRule cell@(C { val = val }) = makeRule (matchesVal val) $ cellValues cell

rowValues :: SudokuCellAtom -> [SudokuCellAtom]
rowValues cell = [cell { col = col' } | col' <- gridSpan]

matchesCol :: Int -> SudokuCellAtom -> Bool
matchesCol col (C { col = col' }) = col' == col

rowRule :: SudokuCellAtom -> [Lit SudokuCellAtom]
rowRule cell@(C { col = col }) = makeRule (matchesCol col) $ rowValues cell

matchesRow :: Int -> SudokuCellAtom -> Bool
matchesRow row (C { row = row' }) = row' == row

colValues :: SudokuCellAtom -> [SudokuCellAtom]
colValues cell = [cell { row = row' } | row' <- gridSpan]

colRule :: SudokuCellAtom -> [Lit SudokuCellAtom]
colRule cell@(C { row = row }) = makeRule (matchesRow row) $ colValues cell

box' :: Int -> Int -> [(Int, Int)]
box' r_ c_ = [(r, c) | r <- [(r_*3)..(r_*3 + 2)], c <- [(c_*3)..(c_*3 + 2)]]

boxValues :: SudokuCellAtom -> [SudokuCellAtom]
boxValues cell@(C{ row = row, col = col }) = [cell {row = row', col = col'} | (row', col') <- box' boxRow boxCol]
  where
    boxRow = row `quot` 3
    boxCol = col `quot` 3

matchesRowCol :: Int -> Int -> SudokuCellAtom -> Bool
matchesRowCol row col cell@(C { row = row', col = col' } ) = row == row' && col == col'

boxRule :: SudokuCellAtom -> [Lit SudokuCellAtom]
boxRule cell@(C { row = row, col = col }) = makeRule (matchesRowCol row col) $ boxValues cell

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

sudokuCnf :: CNF SudokuCellAtom
sudokuCnf = CNF $ Set.fromList clauses
  where
    params = [C { row = row, col = col, val = val } | row <- gridSpan, col <- gridSpan, val <- gridSpan]
    rules = [cellRule, rowRule, colRule, boxRule]
    clauses = [clauseFromSet $ Set.fromList $ rule param | rule <- rules, param <- params]

easyData   = "7-6-9--8-X-----69--X98-5-2-7-X312-4---5X---153---X4---6-318X-6-8-9-31X--73-----X-4--2-8-7"
mediumData = "--6-3--5-X------2-8X-8--95--4X934---527X----4----X815---649X2--81--9-X4-1------X-6--7-3--"
hardData   = "73------4X-----32--X--958----X17------5X--57413--X3------71X----391--X--24-----X8------62"
evilData   = "-2---4-8-X--79---5-X-----7-3-X67---92--X---------X--16---93X-5-8-----X-9---14--X-4-7---1-"
