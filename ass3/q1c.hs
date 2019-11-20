import Data.List
import System.IO

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq) 

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (x `Cons` xs) = x : toHaskellList xs

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = x `Cons` (toList xs)	

