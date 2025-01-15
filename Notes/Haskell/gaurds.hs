bookCategory :: Int -> String 
--Gaurd clause is used for different page ranges
bookCategory pages | pages < 100 = "Short story"
                    | pages < 300 = "Novel"
                    | pages < 1000 = "SK book"
                    | otherwise = "We dont know"

                    
main :: IO()
main = do
  putStrLn "The category of your book is: "
  print (bookCategory 400)