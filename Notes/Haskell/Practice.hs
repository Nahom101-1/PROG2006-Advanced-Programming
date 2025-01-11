-- Problem: Define two constants: `myAge` as 20 and `nextYearAge` as `myAge + 1`
-- Then, try to reassign `myAge` to 30 and observe the error.
-- Write your answers below:

-- Answer:
myAge = 20 :: Int
nextYearAge = 21 :: Int
-- myAge = 31 :: Int
--     Multiple declarations of ‘myAge’
--     Declared at: /Users/nahomberhane/Desktop/Haskell/Practice.hs:6:1
--                  /Users/nahomberhane/Desktop/Haskell/Practice.hs:8:1
--   |
-- 8 | myAge = 31 :: Int



-- Problem: Define a constant `myDouble` with the value 4.2 and specify its type as `Double`.
-- Use :t in GHCi to confirm its type.
-- Write your answers below:

-- Answer:
myDouble = 4.2 :: Double
-- ghci> myDouble = 4.2 :: Double
-- ghci> :t myDouble
-- myDouble :: Double
-- ghci> 

-- Problem: Define a constant `myNum = 25 :: Int`. Use `sqrt` to calculate its square root.
-- Try removing `fromIntegral` and observe the error.
-- Write your answers below:

-- Answer:
myNum = 25 :: Int
myFloat = (fromIntegral myNum)
--     • No instance for (Floating Int) arising from a use of ‘sqrt’
--     • In the expression: sqrt myNum
--       In an equation for ‘sqrtResult’: sqrtResult = sqrt myNum
--    |
-- 35 | sqrtResult = sqrt myNum
--    |              ^^^^
sqrtResult = sqrt myFloat


-- Problem: Evaluate the following Boolean expressions and write the results.
-- 1. True && False || True
-- 2. not (False || True)
-- 3. (5 > 3) && (7 < 10)
-- Write your answers below:
-- 1)  (True && False || True ) = True && False → False False || True → True
-- 2)  not (False || True) = False || True → True  not True → False
-- 3)  (5 > 3) && (7 < 10) = 5 > 3 → True  7 < 10 → True True && True → True
-- Answer:

-- Problem: Use built-in math functions to compute the following:
-- 1. The value of pi
-- 2. log 100
-- 3. floor 8.999
-- 4. 9 ** 3
-- Write your answers below:

-- Answer:


-- Problem: Use built-in math functions to compute the following:
-- 1. The value of pi
-- 2. log 100
-- 3. floor 8.999
-- 4. 9 ** 3
-- Write your answers below:

-- Answer:
myPi = pi
myLog100 = log 100
myFloorvak = floor 8.999
mySquare = 9 ** 3 


-- Problem: Create a tuple (5, "Haskell", True). 
-- Write a function to extract the first, second, and third elements using pattern matching.
-- Write your answers below:

-- Answer:
myTuple:: (Int, String, Bool)
myTuple = (1, "hello",True)
firstVal :: (a, b , c) -> a
firstVal (x, _, _) = x
lastVal :: (a, b , c ) -> c
lastVal (_, _, x) = x

-- Problem: Using Data.List, perform the following:
-- 1. Define a list [1, 2, 3, 4, 5].
-- 2. Reverse the list.
-- 3. Find the sum of all elements.
-- 4. Append [6, 7, 8] to it.
-- Write your answers below:

-- Answer:
myList = [1,2,3,4,5]
reversedList = reverse myList
 
-- Problem: Define a list of integers from 1 to 10^6 using [1..10^6].
-- Write a function to compute the sum of the first 10 elements of this list.
-- Observe that Haskell only evaluates the first 10 elements due to laziness.
-- Write your answers below:

-- Answer:
myBigList :: [Int]
myBigList = [1..10^6]
myCompute :: [Int] -> Int
myCompute [] = 0
myCompute (x:xs) = x + myCompute xs 

-- Problem: Write a recursive function `factorial` to compute the factorial of a number.
-- Example: factorial 5 should return 120.
-- Write your answers below:

-- Answer:
factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial ( n - 1 )


main::IO()
main = do 
    -- show :: Show a => a -> String
    putStrLn("My age is :" ++ show myAge)
    putStrLn ("Next year, my age will be: " ++ show nextYearAge)
    putStrLn("My double : " ++ show myDouble)
    putStrLn("Sqrt of " ++ show myNum ++ ": " ++ show sqrtResult)
    putStrLn("The value of pi is: " ++ show myPi)
    putStrLn("The log of 100 is: " ++ show myLog100)
    putStrLn("The floor of 8.999 is: " ++ show myFloorvak)
    putStrLn("9 raised to the power of 3 is: " ++ show mySquare)
    print (firstVal myTuple)   
    print (lastVal myTuple) 
    print(reversedList)
    print(myCompute myBigList)
    print(factorial 5)