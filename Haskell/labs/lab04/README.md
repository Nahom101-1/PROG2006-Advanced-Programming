# Lab 04: Haskell Age
## Tasks

1. **Task 1: Reverse a List (`mreverse`)**  
   Implement `mreverse :: [a] -> [a]` without using the built-in `reverse`.  
   - Uses an accumulator for O(n) performance  
   - Includes inline doctests

2. **Task 2: Multiplication Table (`mulTable`)**  
   Implement `mulTable :: Int -> IO ()` that prints an n×n multiplication table, with each entry right-aligned in 3 character slots.  
   - Uses `printf` for padding  
   - Outputs rows separated by newlines

3. **Task 3: Oldest Students Count (`countOldest`)**  
   Implement `countOldest :: String -> Int` that, given a block of lines `"Name Surname Age"`, returns how many students share the maximum age.  
   - Parses input with `lines` and `words` only  
   - Single traversal (fold) for O(n) time  
   - Malformed or empty lines default to age `0`

## Requirements

- **GHC** ≥ 9.0  
- **Stack** (https://docs.haskellstack.org/)



## How to Build & Test

From the `lab04/` directory, run:

```bash
stack setup
stack build
stack test
