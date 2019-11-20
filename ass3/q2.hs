import Data.List
import System.IO

-- function to generate substrings of given string
substringGen :: String -> [String]
-- generate different prefixes fr the given tail, sort the substring and store it in the list
substringGen s = [ sort i | t <- tails s, i <- tail (inits t) ]

-- function to count the number of anagrams from the list of substrings
count :: [String] -> Int
-- initialise the count with 0 when list is empty
count [] = 0
-- count the occurences of each element in the suffix of the element in the list
count (x:xs) = (sum $ map (\a -> 1) $ filter (== x) (xs) )  + count xs

-- main function to handle IOs
main = do
  putStrLn "Enter first string:"
  -- first string input
  string1 <- getLine
  putStrLn "Enter second string:"
  -- second string input
  string2 <- getLine
  putStrLn "Number of possible anagram pairs:"
  -- convert first input from IO to normal string
  let temp = string1 :: [Char]
  -- convert second input from IO to normal string and append it with the first sting
  let fullString = temp ++ string2 :: [Char]
  -- generate all substrings of the given string
  let substrings = substringGen fullString
  -- count the anagrams from the list of generated substrings
  let anagrams = count substrings
  -- print the count of anagrams
  print anagrams  