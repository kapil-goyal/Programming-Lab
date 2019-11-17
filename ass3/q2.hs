import Data.List
import System.IO

substring :: String -> [String]
substring s = [ sort i | t <- tails s, i <- tail (inits t) ]
	-- tail . inits =<< tails s
-- continuousSubSeqs s = filter (not . null) . concatMap inits . tails

count :: [String] -> Int
count [] = 0
count (x:xs) = (sum $ map (\a -> 1) $ filter (== x) (xs) )  + count xs

main = do
  input1 <- getLine
  input2 <- getLine
  let list1 = read input1 :: [Char]
  let list2 = list1 ++ read input2 :: [Char]
  let list3 = substring list2
  let a = count list3
  print a
  -- putStrLn $ show list3
  -- list1 ++ list2
  