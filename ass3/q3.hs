import Data.List
import System.IO
import System.IO.Unsafe
import System.Random

-- list of team name acronyms for the fixtures
acronym = ["BS","CM","CH","CV","CS","DS","EE","HU","MA","ME","PH","ST"]

-- shuffle the list of elements by generating a random number and inserting 
-- the element at that position in the beginning of the array
shuffle x = if length x < 2 then return x else do
	i <- System.Random.randomRIO (0, length(x)-1)
	r <- shuffle (take i x ++ drop (i+1) x)
	return (x!!i : r)

-- call the function to shuffle the teams 
matchList = shuffle [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
-- make the list of fixtures static after allocating once
fixList = unsafePerformIO matchList

-- select the teams at even positions of the array
evens (x:xs) = x:odds xs
evens _ = []

-- select the teams at odd positions of the array
odds (_:xs) = evens xs
odds _ = []

-- select the even and odd numbered teams and pair them for a fixture
evenElems = evens fixList
oddElems = odds fixList
matchPairs = zip evenElems oddElems

-- function to print details of each fixture
printMatchSlot :: (Int, Int) -> Int -> String
printMatchSlot (id1, id2) slot
    | slot == 0 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 1-11 9:30AM"
    | slot == 1 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 1-11 7:30PM"
    | slot == 2 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 2-11 9:30AM"
    | slot == 3 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 2-11 7:30PM"
    | slot == 4 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 3-11 9:30AM"
    | slot == 5 =  acronym!!id1  ++ " vs " ++ acronym!!id2 ++ " 3-11 7:30PM"
    | otherwise = "wrong info"

-- find the match slot of the current team by searching in even and odd lists
findElemTuple x = do 
    case elemIndex x (evenElems ++ oddElems) of
        Just n -> n `mod` 6

-- print match of the team with given index
printMatch n = do
    -- find slot of the current team
    let ind = findElemTuple n
    -- find the current opponent
    let pair = matchPairs!! ind
    -- print the fixture details
    let ans = printMatchSlot pair ind
    putStrLn $ show ans
    
-- print the list of all fixtures
printAllMatch [] = putStrLn ""
printAllMatch (x:xs) = do
    -- call function to print each fixture recursively
    printMatch (fst x)
    printAllMatch xs

-- call funtion to print all fixtures
fixture "all" = do
    printAllMatch matchPairs
-- call function to print fixture of specific team
fixture x = do
    case elemIndex x acronym of
      Just n -> printMatch n 
      Nothing -> putStrLn "Wrong Input! please try again."

-- print the fixture details of the given slot
printSlot slot = do
    let pair = matchPairs !! slot
    let ans = printMatchSlot pair slot
    putStrLn $ show ans
    
-- print the next match of current time on 1st Nov
-- select the slot based on current time of day and check validity of input
nextMatch 1 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 0
    | time <= 19.5 = do printSlot 1
    | time < 24 = do printSlot 2
    | otherwise = do putStrLn "Wrong Input! please try again."

-- print the next match of current time on 2nd Nov
-- select the slot based on current time of day and check validity of input
nextMatch 2 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 2
    | time <= 19.5 = do printSlot 3
    | time < 24 = do printSlot 4
    | otherwise = do putStrLn "Wrong Input! please try again."

-- print the next match of current time on 3rd Nov
-- select the slot based on current time of day and check validity of input
nextMatch 3 time
    | time < 0 = do putStrLn "Wrong Input! please try again."
    | time <= 9.5 = do printSlot 4
    | time <= 19.5 = do printSlot 5
    | time < 24 = do putStrLn "Round 1 completed. No match further."
    | otherwise = do putStrLn "Wrong Input! please try again."

-- check for valid input
nextMatch day _ 
    | day > 3 && day <= 30 = do putStrLn "Round 1 completed. No match further."

nextMatch _ _ = do putStrLn "Wrong Input! please try again."
