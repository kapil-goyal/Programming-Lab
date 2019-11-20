import Data.List
import System.IO

greatest :: (a -> Int) -> [a] -> a
greatest y [] = error "give valid input"
greatest y [x] = x
greatest y (x:xs) = if(y x >= y (greatest y xs))
                    then x 
                    else greatest y xs
