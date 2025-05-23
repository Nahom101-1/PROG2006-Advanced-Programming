# Snake-Rust (Iced.rs)

## Project Overview

This is a classic Snake game implemented in Rust, using the [Iced.rs](https://github.com/iced-rs/iced) GUI framework for graphics rendering and event handling via its `Canvas` widget.

The project was developed based on the requirements outlined in **Lab 10**. It features both single-player and two-player modes.

## Features

* Classic Snake gameplay on a 20x20 grid.
* Single-player mode with score and high score tracking.
* Two-player competitive mode with separate scores and collision detection between players.
* Graphical interface rendered using Iced's reactive model and Canvas.
* Cross-platform compatibility (wherever Iced is supported).

## Requirements

* Rust toolchain (rustc and cargo) - Tested with Rust 1.78+ after dependency adjustments.

## Building and Running

1.  **Clone the repository** and `cd` into the project directory.
2.  **Build:** Open a terminal in the project directory and run:
    ```bash
    cargo build
    ```
    *(For an optimized build, use `cargo build --release`)*
3.  **Run:** Execute the compiled program:
    ```bash
    cargo run
    ```
    *(To run the optimized version after building it, use `cargo run --release`)*

## How to Play

1.  **Menu:**
    * Press `1` to start Single Player mode.
    * Press `2` to start Two Player mode.

2.  **Objective:**
    * Guide your snake to eat the red food items that appear on the screen.
    * Each food item consumed increases your score and the length of your snake.
    * Avoid collisions!

3.  **Controls:**
    * **Single Player:** Use either `WASD` keys or the `Arrow` keys to change the snake's direction.
    * **Two Player:**
        * Player 1 (Green): Use `WASD` keys.
        * Player 2 (Blue): Use `Arrow` keys.

4.  **Game Over:**
    * The game ends if a snake collides with:
        * The yellow walls.
        * Its own body.
        * The other player's snake (in Two Player mode).
    * In Two Player mode, the player who *doesn't* collide (or crashed last if simultaneous) is declared the winner. If both collide and scores are equal, it's a draw (shown as "GAME OVER").
    * Press `R` when the "GAME OVER" or winner message is displayed to return to the main menu.