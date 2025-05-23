# Haskell Lab 03: Functional Programming Basics

This project demonstrates fundamental Haskell programming concepts and techniques through a series of tasks. It includes implementations of basic functions, interactive programs, and custom list operations, along with comprehensive test coverage using HSpec and Doctest.

---

## Tasks Overview

- **Task 1: Hello World**  
  A simple program that prints `"Hello World"` to standard output.

- **Task 2: Greet User**  
  An interactive program that asks the user for their name and greets them with `"Hello <name>"`.

- **Task 3: Name and Age**  
  Prompts the user for their name and age, calculates their age in 10 years, and displays the result. Includes type-safe functions like `addNumber` and `addAge`.

- **Task 4: List Head Variants**  
  Implements five versions of a custom `head` function (`mhead1`–`mhead5`) using techniques like pattern matching, recursion, and case expressions. Includes error handling for empty lists.

---

## Features

- **Type Safety**: Demonstrates the use of custom types and type-safe functions.
- **Functional Patterns**: Implements recursion, pattern matching, and higher-order functions.
- **Testing**: Comprehensive test coverage using:
  - **HSpec**: Unit tests for all tasks.
  - **Doctest**: Inline examples embedded in Haddock comments.

---

## Requirements

- **GHC** ≥ 9.0  
- **Stack**: [Install Stack](https://docs.haskellstack.org/)

---

## How to Run

1. **Build the project**  
   ```sh
   stack build
2. **Run the main program**  
   The main program includes a menu that allows you to test all the implemented lab tasks interactively.  
   ```sh
   stack exec lab03-exe