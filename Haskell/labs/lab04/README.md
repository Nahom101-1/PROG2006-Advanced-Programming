# Haskell Lab 04: Functional Programming Basics

This project demonstrates fundamental Haskell programming concepts and techniques through a series of tasks. It includes implementations of list operations, interactive programs, and data processing, along with comprehensive test coverage using HSpec and Doctest.


## Tasks Overview

### Task 1: Reverse a List
Implements a custom function `mreverse` to reverse a list without using the built-in `reverse` function. Includes property-based testing with QuickCheck.

### Task 2: Multiplication Table
Generates a formatted multiplication table for a given size using the `mulTable` function. Includes custom padding for alignment.

### Task 3: Oldest Students Count
Processes a dataset of students to find and count the oldest students using the `countOldest` function. Demonstrates efficient data traversal and parsing.

---

## Features

- **Type Safety**: Demonstrates the use of type-safe functions and Haskell's strong type system.
- **Functional Patterns**: Implements recursion, list processing, and higher-order functions.
- **Testing**: Comprehensive test coverage using:
  - **HSpec**: Unit tests for all tasks.
  - **Doctest**: Inline examples embedded in Haddock comments.

---

## Requirements

- **GHC** ≥ 9.0
- **Stack**: Install [Stack](https://docs.haskellstack.org/).

---

## How to Run

### Build the Project
```bash
stack build

### Run the Main Program
The main program includes interactive prompts to test all the implemented lab tasks.
```bash
stack exec lab04-exe

### Run test
```bash
stack test