module Main where


fizzBuzz :: Int -> String 
fizzBuzz x 
    | x `mod`3 == 0 && x `mod` 5 == 0 = "Fizzbuzz"
    | x `mod`3 == 0 = "Fizz"
    | x `mod`5 == 0 = "Buzz"
    | otherwise = show $ x

main :: IO()
main = do 
    let numbers = [1..30]
    let fizzBuzzResults = map fizzBuzz numbers
    
    
