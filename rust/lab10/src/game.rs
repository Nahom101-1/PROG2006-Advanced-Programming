//! Contains the core game logic, including state initialization, updates,
//! movement calculations, food consumption, and collision detection.

use crate::types::{Direction, GameMode, GameState, Point, Winner};
use rand::Rng;

// --- Grid Constants ---
pub const GRID_WIDTH: i32 = 20;
pub const GRID_HEIGHT: i32 = 20;
/// Size of each grid cell in pixels, used for coordinate calculations.
pub const CELL_SIZE: f32 = 20.0;

// Pre-calculated boundaries in pixels for efficient collision checks.
const HALF_GRID_W_PIXELS: f32 = GRID_WIDTH as f32 / 2.0 * CELL_SIZE;
const HALF_GRID_H_PIXELS: f32 = GRID_HEIGHT as f32 / 2.0 * CELL_SIZE;

impl GameState {
    /// Creates the initial game state, starting in the menu.
    pub fn new() -> Self {
        let mut rng = rand::thread_rng();
        let initial_snake1 = vec![
            (0.0, 0.0),
            (-CELL_SIZE, 0.0),
            (-2.0 * CELL_SIZE, 0.0),
        ];
        let initial_food = Self::random_food_pos(&initial_snake1, &[], &mut rng);

        GameState {
            mode: GameMode::Single,
            in_menu: true,
            snake1: initial_snake1,
            snake2: Vec::new(),
            dir1: Direction::Right,
            pending_dir1: None,
            dir2: Direction::Right,
            pending_dir2: None,
            food: initial_food,
            score1: 0,
            score2: 0,
            sp_high_score: 0,
            mp_high_score1: 0,
            mp_high_score2: 0,
            game_over: false,
            winner: Winner::NoWinner,
            rng,
        }
    }

    /// Resets the game to its initial state (main menu), preserving high scores.
    pub fn reset(&mut self) {
        let sp_hs = self.sp_high_score;
        let mp_hs1 = self.mp_high_score1;
        let mp_hs2 = self.mp_high_score2;
        let mut new_state = GameState::new();
        new_state.sp_high_score = sp_hs;
        new_state.mp_high_score1 = mp_hs1;
        new_state.mp_high_score2 = mp_hs2;
        *self = new_state;
    }

    /// The main game logic update function, called on each tick timer event.
    pub fn update(&mut self) {
        if self.in_menu || self.game_over {
            return;
        }
        self.apply_pending_directions();
        self.move_snakes();
        self.check_food();
        self.check_collisions();
    }

    fn apply_pending_directions(&mut self) {
        if let Some(pending) = self.pending_dir1 {
            if !pending.is_opposite(self.dir1) {
                self.dir1 = pending;
            }
            self.pending_dir1 = None;
        }

        if self.mode == GameMode::Multi {
            if let Some(pending) = self.pending_dir2 {
                if !pending.is_opposite(self.dir2) {
                    self.dir2 = pending;
                }
                self.pending_dir2 = None;
            }
        }
    }

    fn move_snakes(&mut self) {
        self.move_snake(1);
        if self.mode == GameMode::Multi && !self.snake2.is_empty() {
            self.move_snake(2);
        }
    }

    fn move_snake(&mut self, player_index: u8) {
        let (snake, direction) = if player_index == 1 {
            (&mut self.snake1, self.dir1)
        } else {
            (&mut self.snake2, self.dir2)
        };

        if snake.is_empty() { return; }

        let (head_x, head_y) = snake[0];
        let new_head = match direction {
            Direction::Up => (head_x, head_y - CELL_SIZE),
            Direction::Down => (head_x, head_y + CELL_SIZE),
            Direction::Left => (head_x - CELL_SIZE, head_y),
            Direction::Right => (head_x + CELL_SIZE, head_y),
        };

        snake.insert(0, new_head);
        snake.pop();
    }

    fn check_food(&mut self) {
        let mut food_eaten = false;
        let mut grow_snake1 = false;
        let mut grow_snake2 = false;

        if !self.snake1.is_empty() && self.snake1[0] == self.food {
            self.score1 += 1;
            grow_snake1 = true;
            food_eaten = true;
        }

        if self.mode == GameMode::Multi && !self.snake2.is_empty() && self.snake2[0] == self.food {
            if !food_eaten {
                self.score2 += 1;
                grow_snake2 = true;
                food_eaten = true;
            }
        }

        if grow_snake1 {
            let tail = self.snake1.last().cloned().unwrap_or((0.0, 0.0));
            self.snake1.push(tail);
        }
        if grow_snake2 {
            let tail = self.snake2.last().cloned().unwrap_or((0.0, 0.0));
            self.snake2.push(tail);
        }

        if food_eaten {
            self.food = Self::random_food_pos(&self.snake1, &self.snake2, &mut self.rng);
        }
    }

    pub fn random_food_pos(snake1: &[Point], snake2: &[Point], rng: &mut rand::rngs::ThreadRng) -> Point {
        loop {
            let rand_x = rng.gen_range((-GRID_WIDTH / 2)..(GRID_WIDTH / 2)) as f32;
            let rand_y = rng.gen_range((-GRID_HEIGHT / 2)..(GRID_HEIGHT / 2)) as f32;
            let pos = (rand_x * CELL_SIZE, rand_y * CELL_SIZE);

            let occupied_by_s1 = snake1.contains(&pos);
            let occupied_by_s2 = !snake2.is_empty() && snake2.contains(&pos);

            if !occupied_by_s1 && !occupied_by_s2 {
                return pos;
            }
        }
    }

    fn check_collisions(&mut self) {
        match self.mode {
            GameMode::Single => self.check_collisions_single(),
            GameMode::Multi => self.check_collisions_multi(),
        }
    }

    fn check_collisions_single(&mut self) {
        if self.snake1.is_empty() { return; }

        let head1 = self.snake1[0];
        let body1 = &self.snake1[1..];

        if Self::hits_wall(head1) || Self::hits_self(head1, body1) {
            self.game_over = true;
            self.winner = Winner::NoWinner;
            self.sp_high_score = self.sp_high_score.max(self.score1);
        }
    }

    fn check_collisions_multi(&mut self) {
        if self.snake1.is_empty() || self.snake2.is_empty() {
            self.game_over = true;
            self.winner = Winner::NoWinner;
            return;
        }

        let head1 = self.snake1[0];
        let body1 = &self.snake1[1..];
        let head2 = self.snake2[0];
        let body2 = &self.snake2[1..];

        let p1_collided = Self::hits_wall(head1) || Self::hits_self(head1, body1) || self.snake2.contains(&head1);
        let p2_collided = Self::hits_wall(head2) || Self::hits_self(head2, body2) || self.snake1.contains(&head2);

        if p1_collided || p2_collided {
            self.game_over = true;
            self.winner = Self::determine_winner(p1_collided, p2_collided, self.score1, self.score2);
            self.mp_high_score1 = self.mp_high_score1.max(self.score1);
            self.mp_high_score2 = self.mp_high_score2.max(self.score2);
        }
    }

    fn determine_winner(p1_collided: bool, p2_collided: bool, score1: i32, score2: i32) -> Winner {
        match (p1_collided, p2_collided) {
            (true, false) => Winner::Player2Wins,
            (false, true) => Winner::Player1Wins,
            (true, true) => {
                if score1 > score2 { Winner::Player1Wins }
                else if score2 > score1 { Winner::Player2Wins }
                else { Winner::NoWinner }
            }
            (false, false) => Winner::NoWinner,
        }
    }

    fn hits_wall(pos: Point) -> bool {
        let (x, y) = pos;
        x < -HALF_GRID_W_PIXELS || x >= HALF_GRID_W_PIXELS ||
        y < -HALF_GRID_H_PIXELS || y >= HALF_GRID_H_PIXELS
    }

    fn hits_self(head: Point, body: &[Point]) -> bool {
        body.contains(&head)
    }

    pub fn set_pending1(&mut self, dir: Direction) {
        if !self.in_menu && !self.game_over {
            if !dir.is_opposite(self.dir1) {
                self.pending_dir1 = Some(dir);
            }
        }
    }

    pub fn set_pending2(&mut self, dir: Direction) {
        if self.mode == GameMode::Multi && !self.in_menu && !self.game_over {
            if !dir.is_opposite(self.dir2) {
                self.pending_dir2 = Some(dir);
            }
        }
    }
}