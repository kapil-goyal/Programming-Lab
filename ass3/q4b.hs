import Data.List
import System.IO
import Data.Char(digitToInt)
import Control.Monad 
import Data.Array

type Grid = Array (Int, Int) Int

sudokuSize :: Int
sudokuSize = 9
boxSize = 3

check_each_element x = do
    if x `notElem` [0..sudokuSize] then
        error "Invalid input format."
        else True

get_row_input = do
    input <- getLine
    let rowChar = input :: [Char]
    let rowInt = map digitToInt rowChar
    let chk = map check_each_element rowInt
    if length rowInt /= sudokuSize then do
        error "Invalid input format."
        return []
        else return rowInt

make_arr_row row_num x = do
    let len = length x
    rowArr <- [((row_num, j), (x !! j)) | j <- [0..len-1] ]
    return rowArr

validate_sudoku grid = do
    let chk = [validate (grid ! pos) pos grid | pos <- range((0,0), (sudokuSize-1, sudokuSize-1)), grid ! pos /= 0]
    if False `elem` chk then do
        False
        else True

main = do
    putStrLn "Enter Input:"
    arr <- sequence (map (\_ -> get_row_input) [1..sudokuSize])
    let ind_arr = zip [0..] arr
    let my_grid = foldl (++) [] ([make_arr_row (fst x) (snd x) | x <- ind_arr])
    let puzzleGrid = array ((0, 0), (sudokuSize-1, sudokuSize-1)) my_grid
    if validate_sudoku puzzleGrid == True then do
      let possibleSoln = validSolns puzzleGrid
      if null possibleSoln then 
        putStrLn "Not solvable"
      else
        do 
          putStrLn "The solved puzzle:"
          mapM_ putStrLn [show $ [(possibleSoln!!0) ! pos | pos <- range((row, 0), (row, sudokuSize-1))] | row <- [0..sudokuSize-1]]
      else putStrLn "Not solvable: Invalid puzzle."
    
validSolns :: Grid -> [Grid]
validSolns sol = soln (blankCells sol) sol
  where
    soln :: [(Int, Int)] -> Grid -> [Grid]
    soln []     sol = [sol]
    soln (x:xs) sol = concatMap (soln xs) $ map (\val -> updatePuzzle val x sol) $ [val | val <- [1..sudokuSize], validate val x sol]


updatePuzzle :: Int -> (Int, Int) -> Grid -> Grid
updatePuzzle mark (row, col) sol = sol // [((row, col), mark)]

blankCells :: Grid -> [(Int, Int)]
blankCells val = [(row, col) | row <- [0..sudokuSize-1], col <- [0..sudokuSize-1], val ! (row, col) == 0]

validate :: Int -> (Int, Int) -> Grid -> Bool
validate val (row, col) grid = (notElem val rowVals) && (notElem val colVals) && (notElem val boxVals)
  where
    row' = (row `div` boxSize) * boxSize
    col' = (col `div` boxSize) * boxSize
    rowVals = [grid ! pos | pos <- range((row, 0), (row, sudokuSize-1)) , pos /= (row, col)]
    colVals = [grid ! pos | pos <- range((0, col), (sudokuSize-1, col)) , pos /= (row, col)]
    boxVals = [grid ! pos | pos <- range((row', col'), (row' + boxSize-1, col' + boxSize-1)) , pos /= (row, col)]