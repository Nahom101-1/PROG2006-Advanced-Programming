factorial :: Integer -> Integer
--Base case
factorial 0 = 1 
--Recursive case
factorial n = n * factorial (n - 1)

main :: IO()
main = do
    putStrLn "Factorial of your nr is : "
    print(factorial(50))
