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
    putStrLn "Enter Input"
    arr <- sequence (map (\_ -> get_row_input) [1..sudokuSize])
    let ind_arr = zip [0..] arr
    let my_grid = foldl (++) [] ([make_arr_row (fst x) (snd x) | x <- ind_arr])
    let puzzleBoard = array ((0, 0), (sudokuSize-1, sudokuSize-1)) my_grid
    if validate_sudoku puzzleBoard == True then do
      let solution = solve puzzleBoard
      printBoard solution
      else putStrLn "Not solvable: Invalid puzzle."
    

solve :: Grid -> Maybe Grid
solve = headOrNothing . solutions

solutions :: Grid -> [Grid]
solutions b = solutions' (emptyLocations b) b
  where
    solutions' :: [(Int, Int)] -> Grid -> [Grid]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..sudokuSize], validate m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks


copyWithMark :: Int -> (Int, Int) -> Grid -> Grid
copyWithMark mark (row, col) b = b // [((row, col), mark)]

emptyLocations :: Grid -> [(Int, Int)]
emptyLocations b = [(row, col) | row <- [0..sudokuSize-1], col <- [0..sudokuSize-1], b ! (row, col) == 0]

validate :: Int -> (Int, Int) -> Grid -> Bool
validate val (row, col) grid = (notElem val rowVals) && (notElem val colVals) && (notElem val boxVals)
  where
    row' = (row `div` boxSize) * boxSize
    col' = (col `div` boxSize) * boxSize
    rowVals = [grid ! pos | pos <- range((row, 0), (row, sudokuSize-1)) , pos /= (row, col)]
    colVals = [grid ! pos | pos <- range((0, col), (sudokuSize-1, col)) , pos /= (row, col)]
    boxVals = [grid ! pos | pos <- range((row', col'), (row' + boxSize-1, col' + boxSize-1)) , pos /= (row, col)]

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

printBoard :: Maybe Grid -> IO ()
printBoard Nothing  = putStrLn "Not solvable"
printBoard (Just b) = do 
  putStrLn "The solved puzzle:"
  mapM_ putStrLn [show $ [b ! pos | pos <- range((row, 0), (row, sudokuSize-1))] | row <- [0..sudokuSize-1]]
