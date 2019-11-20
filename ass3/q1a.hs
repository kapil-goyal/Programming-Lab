import Data.List
import System.IO

-- helper function to calculate product of sums
prod :: [[Int]] -> Int
-- initialise value with 1 if list is empty
prod [] = 1
-- recursively multiply the products of sum of head and tail
prod (x:xs) = sum x * prod xs

-- function to return products of sum of each list
m :: [[Int]] -> Int
-- return 0 if input is null, else call prod function
m x = if null x  
             then 0
             else prod x