pub mod hello;
pub mod greet;
pub mod announcements;
// pub mod render_html;

use axum::Router; // Import Axum Router.

/// Combines routes from sub-modules.
pub fn routes() -> Router {
    Router::new() // New router.
        .merge(hello::routes()) // Merge 'hello' routes.
        .merge(greet::routes()) // Merge 'greet' routes.
        .merge(announcements::routes()) // Merge 'announcements' routes.
    // Merge other route modules here.
}
