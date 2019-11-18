import Data.List
import System.IO

substring :: String -> [String]
substring s = [ sort i | t <- tails s, i <- tail (inits t) ]

count :: [String] -> Int
count [] = 0
count (x:xs) = (sum $ map (\a -> 1) $ filter (== x) (xs) )  + count xs

main = do
  putStrLn "Enter first string:"
  input1 <- getLine
  putStrLn "Enter second string:"
  input2 <- getLine
  putStrLn "Number of possible anagram pairs:"
  let list1 = input1 :: [Char]
  let list2 = list1 ++ input2 :: [Char]
  let list3 = substring list2
  let a = count list3
  print a  