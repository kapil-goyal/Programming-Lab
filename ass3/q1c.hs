import Data.List
import System.IO

-- datatype for custom list "List" 
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq) 

-- function to convert regular custom list to haskell list
toHaskellList :: List a -> [a]
-- convert Empty to []
toHaskellList Empty = []
-- convert the cons list to normal haskell list
toHaskellList (x `Cons` xs) = x : toHaskellList xs

-- function to convert regular haskell list to custom list
toList :: [a] -> List a
-- convert [] to Empty
toList [] = Empty
-- convert the list to cons list recursively
toList (x:xs) = x `Cons` (toList xs)

