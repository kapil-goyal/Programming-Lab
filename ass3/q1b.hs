import Data.List
import System.IO

-- function to return the element of list that maximises given function
greatest :: (a -> Int) -> [a] -> a
-- throw error if list is empty
greatest y [] = error "give valid input"
-- initialise list with the only element if list has 1 element
greatest y [x] = x
-- recursively check for the greatest value for the head and the tail 
greatest y (x:xs) = if(y x >= y (greatest y xs))
                    then x 
                    else greatest y xs
