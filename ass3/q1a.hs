import Data.List
import System.IO

prod :: [[Int]] -> Int
prod [] = 1
prod (x:xs) = sum x * prod xs

prodList :: [[Int]] -> Int
prodList x = if null x  
             then 0
             else prod x