module Decoder (decodeMessage) where
    
import UniqueMinMax ( uniqueMinMax )
import CheckEvenSum ( checkEvenSum )
import CountMagicNumber ( countMagicNumber )

-- | Decode the alien message from a list of integers.
-- The message is the count of the magic number, which is the average of the minimum and maximum values in the list.
decodeMessage :: [Int] -> Either String Int
decodeMessage xs = uniqueMinMax xs >>= checkEvenSum >>= countMagicNumber xs -- Here we use the uniqueMinMax function to get the minimum and maximum values from the list, 
--then check if their sum is even. If it is, we count how many times the average (magic number) appears in the list.
    
-- | Decode the alien message from the list of integers
-- >>> decodeMessage [5,5,5,8,1,2,3,4,9,8,2,3,4]
-- Right 3
-- >>> decodeMessage [5,5,5,8,1,2,3,4,9,8,2,3,4,1]
-- Left "Minimum not unique"