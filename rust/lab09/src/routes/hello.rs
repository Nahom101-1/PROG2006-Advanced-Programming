// Import Axum components for web server.
use axum::{
    response::{IntoResponse, Response}, // For creating HTTP responses.
    routing::get, // For GET request routes.
    http::StatusCode, // For HTTP status codes (e.g., 200, 404).
    Router, // The main router for defining routes.
};

/// Handles `GET /hello`. Returns "Hello, World!".
pub async fn hello_handler() -> &'static str {
    "Hello, World!"
}

/// Handles `GET /`. Returns a 404 Not Found response.
pub async fn not_found_handler() -> Response {
    // Returns 404 status with a message.
    (StatusCode::NOT_FOUND, "404 Page Not Found").into_response()
}

/// Sets up and returns the Axum router for basic 'hello' endpoints.
pub fn routes() -> Router {
    Router::new() // Create new router.
        // Route for "/hello".
        .route("/hello", get(hello_handler))
        // Route for "/". Returns 404.
        .route("/", get(not_found_handler))
    // Other routes (like /greet/:name) will be added elsewhere.
}
