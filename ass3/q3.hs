import Data.List
import System.IO
import System.IO.Unsafe
import System.Random

idToAcro = ["BS","CM","CH","CV","CS","DS","EE","HU","MA","ME","PH","ST"]

shuffle x = if length x < 2 then return x else do
	i <- System.Random.randomRIO (0, length(x)-1)
	r <- shuffle (take i x ++ drop (i+1) x)
	return (x!!i : r)

matchList = shuffle [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
fixList = unsafePerformIO matchList

evens (x:xs) = x:odds xs
evens _ = []

odds (_:xs) = evens xs
odds _ = []

evenElems = evens fixList
oddElems = odds fixList
matchPairs = zip evenElems oddElems

printMatchSlot :: (Int, Int) -> Int -> String
printMatchSlot (id1, id2) slot
    | slot == 0 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 1-11 9:30AM"
    | slot == 1 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 1-11 7:30PM"
    | slot == 2 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 2-11 9:30AM"
    | slot == 3 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 2-11 7:30PM"
    | slot == 4 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 3-11 9:30AM"
    | slot == 5 =  idToAcro!!id1  ++ " vs " ++ idToAcro!!id2 ++ " 3-11 7:30PM"
    | otherwise = "wrong info"

findElemTuple x = do 
    case elemIndex x (evenElems ++ oddElems) of
        Just n -> n `mod` 6

printMatch n = do
    let ind = findElemTuple n
    let pair = matchPairs!! ind
    let ans = printMatchSlot pair ind
    putStrLn $ show ans
    
printAllMatch [] = putStrLn ""
printAllMatch (x:xs) = do
    printMatch (fst x)
    printAllMatch xs

fixture "all" = do
    printAllMatch matchPairs
fixture x = do
    case elemIndex x idToAcro of
      Just n -> printMatch n 
      Nothing -> putStrLn "Wrong Input! please try again."

printSlot slot = do
    let pair = matchPairs !! slot
    let ans = printMatchSlot pair slot
    putStrLn $ show ans
    
nextMatch 1 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 0
    | time <= 19.5 = do printSlot 1
    | time < 24 = do printSlot 2
    | otherwise = do putStrLn "Wrong Input! please try again."

nextMatch 2 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 2
    | time <= 19.5 = do printSlot 3
    | time < 24 = do printSlot 4
    | otherwise = do putStrLn "Wrong Input! please try again."

nextMatch 3 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 4
    | time <= 19.5 = do printSlot 5
    | time < 24 = do putStrLn "Round 1 completed. No match further."
    | otherwise = do putStrLn "Wrong Input! please try again."

nextMatch day _ 
    | day > 3 && day <= 30 = do putStrLn "Round 1 completed. No match further."

nextMatch _ _ = do putStrLn "Wrong Input! please try again."

