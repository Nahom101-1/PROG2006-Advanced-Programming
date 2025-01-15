popDensity :: (Float, Float) -> Float
popDensity (population, area) = density
  where
    density = population / area

main :: IO ()
main = do
    print (popDensity (10000.23, 234.4))
