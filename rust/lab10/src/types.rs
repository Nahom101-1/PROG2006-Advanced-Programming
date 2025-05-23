//! This module defines the core game types.

/// Represents a point on the game grid using floating-point coordinates.
/// Although the grid is discrete, using f32 matches Iced's coordinate system.
pub type Point = (f32, f32);

/// Game mode: single- or multiplayer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameMode {
    Single,
    Multi,
}

/// Movement directions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    /// Checks if a direction is the exact opposite of another to prevent immediate reversal.
    pub fn is_opposite(&self, other: Direction) -> bool {
        matches!(
            (self, other),
            (Direction::Up, Direction::Down)
                | (Direction::Down, Direction::Up)
                | (Direction::Left, Direction::Right)
                | (Direction::Right, Direction::Left)
        )
    }
}

/// Indicates the winner in multiplayer mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Winner {
    NoWinner,    // No winner yet, or a tie in multiplayer.
    Player1Wins,
    Player2Wins,
}

/// Represents the complete state of the snake game at any point in time.
#[derive(Debug, Clone)]
pub struct GameState {
    pub mode: GameMode,           // Selected game mode (Single or Multi)
    pub in_menu: bool,            // Whether the game is showing the menu
    pub snake1: Vec<Point>,       // Player 1's snake body, head at index 0
    pub snake2: Vec<Point>,       // Player 2's snake body, head at index 0 (empty if singleplayer)
    pub dir1: Direction,          // Current direction for Player 1
    pub pending_dir1: Option<Direction>, // Next direction input for Player 1
    pub dir2: Direction,          // Current direction for Player 2
    pub pending_dir2: Option<Direction>, // Next direction input for Player 2
    pub food: Point,              // Food position on the grid
    pub score1: i32,              // Player 1's current score
    pub score2: i32,              // Player 2's current score
    pub sp_high_score: i32,       // Best single-player score in this session
    pub mp_high_score1: i32,      // Best multiplayer score for Player 1
    pub mp_high_score2: i32,      // Best multiplayer score for Player 2
    pub game_over: bool,          // Whether the game is over
    pub winner: Winner,           // Winner of the current game (if multiplayer)
    pub rng: rand::rngs::ThreadRng, // Random generator used for placing food
}