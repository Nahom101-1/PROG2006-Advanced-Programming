//! Handles rendering the game state using Iced's Canvas widget.

use crate::game::{CELL_SIZE, GRID_HEIGHT, GRID_WIDTH};
use crate::types::{GameState, GameMode, Point, Winner};

use iced::widget::canvas::{self, Cache, Canvas, Frame, Geometry, Path, Stroke, Text, Fill, Style};
use iced::{Color, Element, Length, Point as IcedPoint, Rectangle, Theme, Vector, alignment};
use iced::mouse;
use iced::Renderer;

// Calculated grid dimensions in pixels, relative to the center origin (0,0).
const GRID_WIDTH_PIXELS: f32 = GRID_WIDTH as f32 * CELL_SIZE;
const GRID_HEIGHT_PIXELS: f32 = GRID_HEIGHT as f32 * CELL_SIZE;
const HALF_GRID_W_PIXELS: f32 = GRID_WIDTH_PIXELS / 2.0;
const HALF_GRID_H_PIXELS: f32 = GRID_HEIGHT_PIXELS / 2.0;

/// Structure holding the state needed for rendering via Iced's Canvas.
pub struct GameCanvas<'a> {
    state: &'a GameState, // Reference to the current game state
    cache: Cache,         // Drawing cache to avoid redrawing static elements
}

impl<'a> GameCanvas<'a> {
    pub fn new(state: &'a GameState) -> Self {
        Self {
            state,
            cache: Cache::new(),
        }
    }

    pub fn view(self) -> Element<'a, crate::Message> {
        Canvas::new(self)
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }

    // --- Color Helpers ---
    fn text_color() -> Color { Color::WHITE }
    fn snake_color_1() -> Color { Color::from_rgb8(0, 255, 0) }
    fn snake_color_2() -> Color { Color::from_rgb8(0, 0, 255) }
    fn food_color() -> Color { Color::from_rgb8(255, 0, 0) }
    fn wall_color() -> Color { Color::from_rgb8(255, 255, 0) }
    fn gameover_color() -> Color { Color::from_rgb8(255, 0, 0) }
}

impl<'a> canvas::Program<crate::Message> for GameCanvas<'a> {
    type State = (); // No internal mutable state needed

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        let geometry = self.cache.draw(renderer, bounds.size(), |frame| {
            // Translate origin to canvas center
            let canvas_center = frame.center();
            frame.translate(Vector::new(canvas_center.x, canvas_center.y));

            if self.state.in_menu {
                self.render_menu(frame);
            } else {
                self.render_walls(frame);
                self.render_snake(frame, &self.state.snake1, Self::snake_color_1());

                if self.state.mode == GameMode::Multi && !self.state.snake2.is_empty() {
                    self.render_snake(frame, &self.state.snake2, Self::snake_color_2());
                }

                self.render_food(frame, self.state.food);

                match self.state.mode {
                    GameMode::Single => self.render_score_single(frame),
                    GameMode::Multi => self.render_score_multi(frame),
                }

                if self.state.game_over {
                    self.render_game_over(frame);
                }
            }
        });

        vec![geometry]
    }
}

impl<'a> GameCanvas<'a> {
    fn render_menu(&self, frame: &mut Frame) {
        let text_size = 20.0;
        let color = Self::text_color();
        let default_text = Text { color, size: text_size.into(), ..Text::default() };

        // Menu Instructions
        frame.fill_text(Text { content: "Press 1: Single Player".to_string(), position: IcedPoint::new(-170.0, 70.0), ..default_text });
        frame.fill_text(Text { content: "Press 2: Multiplayer".to_string(), position: IcedPoint::new(-170.0, 30.0), ..default_text });
        frame.fill_text(Text { content: "P1: WASD; P2: Arrow Keys".to_string(), position: IcedPoint::new(-170.0, -10.0), size: (text_size * 0.75).into(), ..default_text });
        frame.fill_text(Text { content: "Press R to retry when dead".to_string(), position: IcedPoint::new(-170.0, -50.0), size: (text_size * 0.75).into(), ..default_text });
    }

    fn render_walls(&self, frame: &mut Frame) {
        let wall_stroke = Stroke { style: Style::Solid(Self::wall_color()), width: 2.0, ..Stroke::default() };

        let wall_path = Path::rectangle(
            IcedPoint::new(-HALF_GRID_W_PIXELS, -HALF_GRID_H_PIXELS),
            iced::Size::new(GRID_WIDTH_PIXELS, GRID_HEIGHT_PIXELS),
        );

        frame.stroke(&wall_path, wall_stroke);
    }

    fn render_snake(&self, frame: &mut Frame, segments: &[Point], color: Color) {
        for (x, y) in segments {
            // Convert segment center to top-left origin for drawing
            let top_left_x = x - CELL_SIZE / 2.0;
            let top_left_y = y - CELL_SIZE / 2.0;

            let segment_path = Path::rectangle(
                IcedPoint::new(top_left_x, top_left_y),
                iced::Size::new(CELL_SIZE * 0.9, CELL_SIZE * 0.9),
            );

            frame.fill(&segment_path, Fill::from(color));
        }
    }

    fn render_food(&self, frame: &mut Frame, position: Point) {
        let (x, y) = position;
        let food_path = Path::circle(IcedPoint::new(x, y), CELL_SIZE * 0.4);
        frame.fill(&food_path, Fill::from(Self::food_color()));
    }

    fn render_score_single(&self, frame: &mut Frame) {
        let text = format!("Score: {} High: {}", self.state.score1, self.state.sp_high_score);
        let pos = IcedPoint::new(-HALF_GRID_W_PIXELS + 10.0, -HALF_GRID_H_PIXELS - 30.0);

        frame.fill_text(Text {
            content: text,
            position: pos,
            color: Self::text_color(),
            size: (CELL_SIZE * 0.75).into(),
            ..Text::default()
        });
    }

    fn render_score_multi(&self, frame: &mut Frame) {
        let score1 = format!("P1: {} High: {}", self.state.score1, self.state.mp_high_score1);
        let score2 = format!("P2: {} High: {}", self.state.score2, self.state.mp_high_score2);

        let p1_pos = IcedPoint::new(-HALF_GRID_W_PIXELS + 10.0, -HALF_GRID_H_PIXELS - 30.0);
        let p2_pos = IcedPoint::new(HALF_GRID_W_PIXELS - 150.0, -HALF_GRID_H_PIXELS - 30.0);

        let size = (CELL_SIZE * 0.75).into();
        let color = Self::text_color();
        let base = Text { color, size, ..Text::default() };

        frame.fill_text(Text { content: score1, position: p1_pos, ..base });
        frame.fill_text(Text { content: score2, position: p2_pos, ..base });
    }

    fn render_game_over(&self, frame: &mut Frame) {
        // Show appropriate game over message depending on winner
        let msg = match self.state.winner {
            Winner::Player1Wins => "Player 1 Wins! (Press R)",
            Winner::Player2Wins => "Player 2 Wins! (Press R)",
            Winner::NoWinner => "GAME OVER (Press R)",
        };

        frame.fill_text(Text {
            content: msg.to_string(),
            color: Self::gameover_color(),
            size: (CELL_SIZE * 1.5).into(),
            horizontal_alignment: alignment::Horizontal::Center,
            vertical_alignment: alignment::Vertical::Center,
            position: IcedPoint::new(0.0, 0.0),
            ..Text::default()
        });
    }
}
