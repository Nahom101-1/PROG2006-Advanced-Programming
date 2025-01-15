


--Functions are First class
--No need for void, int or parentheses around parametes unless necesaary

square x = x * x 
--No need for semicolons to end statments line break or indentation handle thi

--Variables in Haskell, once assigned, cannot be changed, Theyre called bindgings
 x = 10 --This vlaue can´t be reassigned. 

--Type Internence:
y :: Int
y = 10
-- No need to explocotliy declare varibles types, but can if you want. 
-- Whitespace sensitiviy forces a consistent style
--Immutable bindings prevent bugs related to variable reassignments
--Type inference reduces boilerplate code


-- Practice Problems
-- Easy

-- Define a function double that takes an integer and returns its double. Then, create a binding y equal to the double of 10.

-- double x::Int = x * 2

-- y = double 10

--correct version 
double :: Int -> Int
double x = x * 2
-- Challenging
y = double 10

-- Write a function areaOfCircle that calculates the area of a circle given its radius. Use a binding for pi and ensure the function works for floating-point numbers.

-- areaOfCircle R::Int = pi::Floating * (R * R)
--correct version 
areaOfCircle :: Floating a => a -> a
areaOfCircle r = pi * (r ^ 2)
test = areaOfCircle 12.3



-- add :: Int -> Int -> Int
-- add x y = x + y 

-- operation = add -- 'Operation' is now a function

-- lucky :: Int -> String
-- lucky 7 = "Lucky Number seven"
-- lucky _ = "Sorry, youre out of luck" -- _ is a wildcard pattern


-- bmiTell :: Double -> Double -> String
-- bmiTell weight heigt 
--    | bmi <= 16.5 = "You´re undeweight"
--    | bmi <= 25.0 = "You're at a normal weight"
--    | bmi <= 16.5 = "You´re overwweight"
--    | otherwise  = "You´re obese"
--    where bmi = weight / heigt ^2


triple :: Int -> Int
triple x = x * 3

myAge = 10


"GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
ghci> 15 + 8
23
ghci> (34 * 7) - 15
223
ghci> (20 - 4) * 5
80
ghci> (10 * 5) + 3
53
ghci> 10 * ( 5 + 7)
120
ghci> 10 * ( 5 + 3)
80
ghci> succ 9 * 10 - 4
96
ghci> succ 9 * (10 - 4)
60
ghci> 
"



compareThree:: Int -> Int -> Int -> Bool
compareThree = x y z = (x == y) && (y == z)