mod game;
mod render;
mod types;

use crate::types::GameState;
use iced::keyboard::{self, Key, key::Named};
use iced::widget::container;
use iced::{executor, time, Application, Command, Element, Length, Settings, Subscription, Theme};
use render::GameCanvas;
use std::time::Duration;
use types::{Direction, GameMode};

const TICK_DURATION: Duration = Duration::from_millis(200); // Game update interval

fn main() -> iced::Result {
    SnakeApp::run(Settings {
        window: iced::window::Settings {
            size: iced::Size::new(1080.0, 920.0),
            ..Default::default()
        },
        ..Settings::default()
    })
}

struct SnakeApp {
    state: GameState,
}

#[derive(Debug, Clone)]
pub enum Message {
    Tick,                  // Timer tick for advancing the game state
    KeyPressed(Key),       // Input from keyboard
}

impl Application for SnakeApp {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        (
            SnakeApp {
                state: GameState::new(),
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("Snake Game - Rust/Iced")
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::Tick => {
                if !self.state.in_menu && !self.state.game_over {
                    self.state.update();
                }
            }
            Message::KeyPressed(key) => {
                self.handle_keypress(key);
            }
        }
        Command::none()
    }

    fn subscription(&self) -> Subscription<Message> {
        // Listen for keypresses always
        let keyboard_sub = keyboard::on_key_press(|key, _| Some(Message::KeyPressed(key)));

        // Start ticking only when game is active
        let timer_sub = if !self.state.in_menu && !self.state.game_over {
            time::every(TICK_DURATION).map(|_| Message::Tick)
        } else {
            Subscription::none()
        };

        Subscription::batch(vec![keyboard_sub, timer_sub])
    }

    fn view(&self) -> Element<Message> {
        // Create the canvas element and center it
        let canvas = GameCanvas::new(&self.state).view();

        container(canvas)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }

    fn theme(&self) -> Self::Theme {
        Theme::Dark
    }
}

impl SnakeApp {
    /// Handles keyboard input depending on current game state
    fn handle_keypress(&mut self, key: Key) {
        if self.state.in_menu {
            // Handle menu selection keys
            match key {
                Key::Character(c) => match c.as_str() {
                    "1" => {
                        self.state.mode = GameMode::Single;
                        self.state.in_menu = false;
                        self.state.food = GameState::random_food_pos(
                            &self.state.snake1,
                            &[],
                            &mut self.state.rng,
                        );
                    }
                    "2" => {
                        self.state.mode = GameMode::Multi;
                        self.state.in_menu = false;
                        // Initialize Snake 2 body and color
                        self.state.snake2 = vec![
                            (game::CELL_SIZE * 2.0, game::CELL_SIZE * 3.0),
                            (game::CELL_SIZE, game::CELL_SIZE * 3.0),
                            (0.0, game::CELL_SIZE * 3.0),
                        ];

                        self.state.food = GameState::random_food_pos(
                            &self.state.snake1,
                            &self.state.snake2,
                            &mut self.state.rng,
                        );
                    }
                    _ => {}
                },
                _ => {}
            }
        } else if self.state.game_over {
            // Restart game on 'R'
            match key {
                Key::Character(c) if c.to_ascii_lowercase() == "r" => {
                    self.state.reset();
                }
                _ => {}
            }
        } else {
            // Handle snake direction input (WASD or Arrows)
            match key {
                Key::Character(c) => match c.to_ascii_lowercase().as_str() {
                    "w" => self.state.set_pending1(Direction::Up),
                    "s" => self.state.set_pending1(Direction::Down),
                    "a" => self.state.set_pending1(Direction::Left),
                    "d" => self.state.set_pending1(Direction::Right),
                    _ => {}
                },
                Key::Named(named_key) => match named_key {
                    Named::ArrowUp => {
                        if self.state.mode == GameMode::Single {
                            self.state.set_pending1(Direction::Up);
                        } else {
                            self.state.set_pending2(Direction::Up);
                        }
                    }
                    Named::ArrowDown => {
                        if self.state.mode == GameMode::Single {
                            self.state.set_pending1(Direction::Down);
                        } else {
                            self.state.set_pending2(Direction::Down);
                        }
                    }
                    Named::ArrowLeft => {
                        if self.state.mode == GameMode::Single {
                            self.state.set_pending1(Direction::Left);
                        } else {
                            self.state.set_pending2(Direction::Left);
                        }
                    }
                    Named::ArrowRight => {
                        if self.state.mode == GameMode::Single {
                            self.state.set_pending1(Direction::Right);
                        } else {
                            self.state.set_pending2(Direction::Right);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }
}