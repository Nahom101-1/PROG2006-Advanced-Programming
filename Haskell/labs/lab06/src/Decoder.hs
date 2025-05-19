module Decoder (decodeMessage) where
    
import UniqueMinMax ( uniqueMinMax )

import CheckEvenSum ( checkEvenSum )

import CountMagicNumber ( countMagicNumber )

decodeMessage :: [Int] -> Either String Int
decodeMessage xs = uniqueMinMax xs >>= checkEvenSum >>= countMagicNumber xs

-- | Decode the alien message from the list of integers
-- >>> decodeMessage [5,5,5,8,1,2,3,4,9,8,2,3,4]
-- Right 3
-- >>> decodeMessage [5,5,5,8,1,2,3,4,9,8,2,3,4,1]
-- Left "Minimum not unique"