# Lab 06: Haskell Greetings Message

This project focuses on decoding alien transmissions using Haskell's functional programming features. It involves error handling with `Maybe` and `Either` types, modular function composition, and validation checks.

## Tasks Overview

### Task 1: Unique Min-Max Validation
Ensures the minimum and maximum numbers in the transmission are unique.

### Task 2: Even Sum Validation
Checks if the sum of the minimum and maximum numbers is even.

### Task 3: Count Magic Number
Counts the occurrences of the "magic number," which is the average of the minimum and maximum values.

### Task 4: Decode Message
Combines all tasks to decode the alien message from a sequence of integers.

## Features

- **Error Handling**: Uses `Either` for detailed error messages.
- **Functional Composition**: Implements validation checks using `(>>=)` and applicative operators (`<$>`, `<*>`).
- **Testing**: Includes comprehensive `doctests` for all functions.

## Requirements

- **GHC** ≥ 9.0
- **Stack**: Install Stack for building and testing.

## How to Run

### Build the Project
```bash
stack build

### Run the Main Program
```bash
stack exec lab06-exe

### Run test
```bash
stack test