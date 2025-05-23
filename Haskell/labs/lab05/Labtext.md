# Haskell Lab 05: Advanced Functional Programming

This project explores advanced Haskell programming concepts and techniques through a series of tasks. It includes implementations of custom list operations, recursive functions, and infinite sequences, along with comprehensive test coverage using Doctest.

## Tasks Overview

### Task 1: Custom `head` Function Variants
Implements six versions of a custom `head` function (`mhead1`–`mhead6`) using different techniques:
1. Pattern matching
2. Guards
3. `if ... else ...` expressions
4. `let ... in ...` expressions
5. `where` expressions
6. `case ... of ...` expressions

### Task 2: Factorial Function
Implements a recursive factorial function (`mfact`) that calculates the factorial of a given integer.

### Task 3: Fibonacci Function
Implements a recursive Fibonacci function (`fib`) that calculates the `n-th` Fibonacci number.

### Task 4: Infinite Fibonacci Sequence
Defines an infinite Fibonacci sequence (`fibs`) using recursion and lazy evaluation. Implements a new Fibonacci function (`fib2`) that retrieves the `n-th` Fibonacci number from the sequence.

### Bonus: `zip` and `zipWith`
Rewrites the infinite Fibonacci sequence using the `zipWith` function. Explores the use of `zip` and `zipWith` for functional programming tasks.

## Features

- **Type Safety**: Demonstrates the use of type-safe functions.
- **Functional Patterns**: Implements recursion, pattern matching, guards, and lazy evaluation.
- **Infinite Sequences**: Explores Haskell's ability to define and work with infinite sequences.
- **Testing**: Comprehensive test coverage using:
  - **Doctest**: Inline examples embedded in Haddock comments.

## Requirements

- **GHC** ≥ 9.0
- **Stack**: Install Stack

## How to Run

1. **Build the project**:
   ```bash
   stack build
2. **Build the project**:
   ```bash
   stack exec lab05-exe
3. **Testing**:
   ```bash
   stack test