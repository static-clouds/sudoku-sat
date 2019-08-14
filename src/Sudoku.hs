module Sudoku where

import CNF(CNF(..), Lit(..), Polarity(..), makeUnitLiteral, clauseFromSet)
import GHC.Generics
import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Test.QuickCheck

allPositions :: Int -> [(Int, Int)]
allPositions n = [(r, c) | r <- gridSpan n, c <- gridSpan n]

toVal :: Char -> Maybe Int
toVal '-' = Nothing
toVal s   = Just (read [s] :: Int)

toMoves :: String -> [SudokuCellAtom]
toMoves s = [(C { row = row, col = col, val = (fromJust val) - 1}) | (row, col) <- allPositions gridSize
                                                             , let val' = board !! row !! col
                                                             , let val = toVal val'
                                                             , isJust val
                                                             ]
  where
    gridSize = case length board of
      9 -> 3
      4 -> 2
      x -> error $ "invalid board length given (" ++ show x ++ ")"
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

gridSpan :: Int -> [Int]
gridSpan n = [0..m]
  where m = (n * n) - 1

makeRule :: (a -> Bool) -> [a] -> [Lit a]
makeRule cond atoms = map (posIf cond) atoms

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

makeXorRule :: [a] -> [[Lit a]]
makeXorRule lits = oneMustBeTrue : onlyOneIsTrue
  where
    oneMustBeTrue = map (Lit Pos) lits
    onlyOneIsTrue = [[Lit Neg a, Lit Neg b] | (a, b) <- pairs lits]

matchesVal :: Int -> SudokuCellAtom -> Bool
matchesVal val (C {val = val'}) = val' == val

setVal :: SudokuCellAtom -> Int -> SudokuCellAtom
setVal cell val = cell { val = val }

cellValues :: [Int] -> SudokuCellAtom -> [SudokuCellAtom]
cellValues values cell = map (setVal cell) values

cellRule :: [Int] -> SudokuCellAtom -> [[Lit SudokuCellAtom]]
cellRule values cell = makeXorRule $ cellValues values cell

setCol :: SudokuCellAtom -> Int -> SudokuCellAtom
setCol cell col = cell { col = col }

rowValues :: [Int] -> SudokuCellAtom -> [SudokuCellAtom]
rowValues values cell = map (setCol cell) values

matchesCol :: Int -> SudokuCellAtom -> Bool
matchesCol col (C { col = col' }) = col' == col

rowRule :: [Int] -> SudokuCellAtom -> [[Lit SudokuCellAtom]]
rowRule values cell@(C { col = col }) = makeXorRule $ rowValues values cell

matchesRow :: Int -> SudokuCellAtom -> Bool
matchesRow row (C { row = row' }) = row' == row

setRow :: SudokuCellAtom -> Int -> SudokuCellAtom
setRow cell row = cell { row = row }

colValues :: [Int] -> SudokuCellAtom -> [SudokuCellAtom]
colValues values cell = map (setRow cell) values

colRule :: [Int] -> SudokuCellAtom -> [[Lit SudokuCellAtom]]
colRule values cell@(C { row = row }) = makeXorRule $ colValues values cell

box' :: Int -> Int -> Int -> [(Int, Int)]
box' boxSize r_ c_ = [(r, c) | r <- [rowStart..rowEnd], c <- [colStart..colEnd]]
  where
    rowStart = r_ * boxSize
    rowEnd = rowStart + boxSize - 1
    colStart = c_ * boxSize
    colEnd = colStart + boxSize - 1

box :: Int -> SudokuCellAtom -> [(Int, Int)]
box boxSize (C { row = row, col = col } ) = box' boxSize boxRow boxCol
  where
    boxRow = row `quot` boxSize
    boxCol = col `quot` boxSize

setRowCol :: SudokuCellAtom -> (Int, Int) -> SudokuCellAtom
setRowCol cell (row, col) = (flip setRow $ row) . (flip setCol $ col) $ cell

boxValues :: [(Int, Int)] -> SudokuCellAtom -> [SudokuCellAtom]
boxValues values cell = map (setRowCol cell) $ values

matchesRowCol :: Int -> Int -> SudokuCellAtom -> Bool
matchesRowCol row col cell@(C { row = row', col = col' } ) = row == row' && col == col'

boxRule :: Int -> SudokuCellAtom -> [[Lit SudokuCellAtom]]
boxRule gridSize cell = makeXorRule $ boxValues (box gridSize cell) cell

sudokuCnf :: Int -> CNF SudokuCellAtom
sudokuCnf gridSize = CNF $ Set.fromList clauses
  where
    values = gridSpan gridSize
    params = [C { row = row, col = col, val = val } | row <- values, col <- values, val <- values]
    rules = [cellRule values, rowRule values, colRule values, boxRule gridSize]
    clauses = map (clauseFromSet . Set.fromList) clauseLists
    clauseLists = concat [rule param | rule <- rules, param <- params]

easyData   = "7-6-9--8-X-----69--X98-5-2-7-X312-4---5X---153---X4---6-318X-6-8-9-31X--73-----X-4--2-8-7"
mediumData = "--6-3--5-X------2-8X-8--95--4X934---527X----4----X815---649X2--81--9-X4-1------X-6--7-3--"
hardData   = "73------4X-----32--X--958----X17------5X--57413--X3------71X----391--X--24-----X8------62"
evilData   = "-2---4-8-X--79---5-X-----7-3-X67---92--X---------X--16---93X-5-8-----X-9---14--X-4-7---1-"

smallEmptyData = "----X----X----X----"
smallData      = "1234X----X----X----"
