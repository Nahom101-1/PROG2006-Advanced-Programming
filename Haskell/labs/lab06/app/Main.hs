module Main (main) where


import Decoder
-- main function to read input and decode the message
main :: IO ()
main = do 
    input <- getLine  -- read a line of input
    let numbers = map read (words input) :: [Int] -- convert input to a list of integers
    case decodeMessage numbers of -- decode the message
        Left err  -> putStrLn $ "Oh no! " ++ err  -- print error message
        Right msg -> putStrLn $ "The message is " ++ show msg ++ "!" -- print the decoded message