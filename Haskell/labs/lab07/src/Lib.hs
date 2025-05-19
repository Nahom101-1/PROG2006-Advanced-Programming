module Lib(countScore) where

-- | Count the total score of the five balls lottery game data.
countScore :: String -> Int
countScore txt = sum (map processLine (lines txt)) -- uses map to apply processLine to each line of the input data


-- | Process a single line of the input data.
-- Each line represents a game, the first three numbers are the "Winning Numbers", 
-- and the next five are the lottery numbers. Calculate the score for the single game.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 5
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 5
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
--
-- >>> processLine "35 35 35 1 5 6 35 16"
-- 32
--
-- >>> processLine "35 35 6 1 5 6 35 35"
-- 49
-- 
-- >>> processLine "35 35 35 1 5 6 35 35"
-- 96
-- 
processLine :: String -> Int
processLine line =
  let (winning, scoring) = parseLine line -- uses parseLine to split the line into winning and scoring numbers
  in sum (map (`scoreBall` winning) scoring) -- uses map to apply scoreBall to each scoring number with the winning numbers

-- | Parse a line of input into a pair of winning numbers and scoring numbers.
-- >>> parseLine "8 18 14 9 16 13 11 10"
-- ([8,18,14],[9,16,13,11,10])
parseLine :: String -> ([Int], [Int])  
parseLine str = splitAt 3 (map read (words str)) -- uses splitAt to separate the first three numbers as winning numbers and the rest as scoring numbers
-- use map and read to convert the string numbers into integers

-- | Get the base score of a number depending on its value. 
-- >>> baseScore 5
-- 1
-- >>> baseScore 14
-- 2
-- >>> baseScore 25
-- 4
-- >>> baseScore 33
-- 8
baseScore :: Int -> Int  
baseScore x
  | x >= 1  && x <= 9  = 1
  | x >= 10 && x <= 19 = 2
  | x >= 20 && x <= 29 = 4
  | x >= 30 && x <= 39 = 8
  | otherwise = error "Not a valid number"


-- | Compute the multiplier based on how many times the number appears in winning numbers.
-- >>> winningMultiplier 12 [12,12,24]
-- 2
-- >>> winningMultiplier 12 [12,12,12]
-- 4
-- >>> winningMultiplier 12 [12,24,35]
-- 1
-- >>> winningMultiplier 12 [1,2,3]
-- 1
winningMultiplier :: Int -> [Int] -> Int
winningMultiplier x wins = 
    let count = length (filter (== x) wins)  -- uses filter to count how many times x appears in the winning numbers
    in if count == 0 then 1 else 2 ^ (count - 1) -- if count is 0, return 1, else return 2^(count-1)



-- | Score a single ball given the winning numbers.
-- >>> scoreBall 12 [12,12,24]
-- 4
-- >>> scoreBall 30 [12,12,30]
-- 8
-- >>> scoreBall 13 [12,12,24]
-- 0
scoreBall :: Int -> [Int] -> Int
scoreBall ball winningNumbers =
  if ball `elem` winningNumbers  -- checks if the ball is in the winning numbers
    then baseScore ball * winningMultiplier ball winningNumbers  -- uses baseScore and winningMultiplier to calculate the score
    else 0 -- if the ball is not in the winning numbers, return 0