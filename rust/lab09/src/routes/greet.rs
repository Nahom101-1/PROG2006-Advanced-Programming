use axum::{
    routing::{get, post}, // For GET and POST methods.
    extract::Path, // For path parameters.
    Json,          // For JSON handling.
    Router,        // For router creation.
};
use serde::{Deserialize, Serialize}; // For JSON serialization/deserialization.

// Response struct for GET /greet/<name>.
#[derive(Debug, Serialize)]
pub struct GreetNameResponse {
    pub greet: String,
    pub name: String,
}

// Input struct for POST /greetme.
#[derive(Debug, Deserialize)]
pub struct GreetMeInput {
    pub input: String,
    pub name: String,
}

// Response struct for POST /greetme.
#[derive(Debug, Serialize)]
pub struct GreetMeResponse {
    pub msg: String,
}

// Handler for GET /greet/:name.
pub async fn greet_name_handler(Path(name): Path<String>) -> Json<GreetNameResponse> {
    Json(GreetNameResponse {
        greet: "Hello".to_string(),
        name,
    })
}

// Handler for GET /greet if no name is specified.
pub async fn greet_default_handler() -> Json<GreetNameResponse> {
    Json(GreetNameResponse {
        greet: "Hello".to_string(),
        name: "YOu😈".to_string(),
    })
}

// Handler for POST /greetme.
pub async fn greet_me_handler(Json(payload): Json<GreetMeInput>) -> Json<GreetMeResponse> {
    let message = format!("{} {}", payload.input, payload.name);
    let response_data = GreetMeResponse {
        msg: message,
    };
    Json(response_data)
}

// Defines and returns the router for 'greet' endpoints.
pub fn routes() -> Router {
    Router::new()
        .route("/greet/{name}", get(greet_name_handler))
        .route("/greet", get(greet_default_handler))
        .route("/greet/", get(greet_default_handler))
        .route("/greetme", post(greet_me_handler))
}