module UniqueMinMax where
import Count ( count )

-- | Get the minimum and maximum values from a list of integers.
-- The function checks if the minimum and maximum values are unique in the list.
uniqueMinMax :: [Int] -> Either String (Int, Int)
uniqueMinMax xs  -- 
  | count minVal xs /= 1 = Left "Minimum not unique" --check if minVal is unique
  | count maxVal xs /= 1 = Left "Maximum not unique"  --check if maxVal is unique
  | otherwise            = Right (minVal, maxVal)  -- --return min and max values
  where
    minVal = minimum xs -- -- get minimum value
    maxVal = maximum xs -- -- get maximum value

-- | Get min and max if both are unique
-- >>> uniqueMinMax [1,2,3,4]
-- Right (1,4)
-- >>> uniqueMinMax [1,1,2,3,4]
-- Left "Minimum not unique"
-- >>> uniqueMinMax [1,2,3,3]  
-- Left "Maximum not unique"
