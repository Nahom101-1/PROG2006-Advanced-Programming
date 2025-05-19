module CountMagicNumber where
import Count ( count )

-- | Count how many times a magic number appears in a list.
countMagicNumber :: [Int] -> Int -> Either String Int --
countMagicNumber xs magic = Right (count magic xs) -- uses the count function to count occurrences of magic number in the list
-- | Count how many times magic number appears
-- >>> countMagicNumber [1,2,3,4,5] 3
-- Right 1