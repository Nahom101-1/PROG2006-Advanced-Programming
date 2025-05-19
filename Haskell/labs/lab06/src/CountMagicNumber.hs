module CountMagicNumber where


import Count ( count )

countMagicNumber :: [Int] -> Int -> Either String Int
countMagicNumber xs magic = Right (count magic xs)
-- | Count how many times magic number appears
-- >>> countMagicNumber [1,2,3,4,5] 3
-- Right 1