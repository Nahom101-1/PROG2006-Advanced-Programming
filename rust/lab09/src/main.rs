pub mod routes; 
use axum::Router;
use dotenvy::dotenv;
use tokio::net::TcpListener;
use tower_http::services::ServeDir;
mod render_html;  

// This function simply calls the top-level routes function from our routes module
// which in turn merges all the sub-module routes.
pub fn create_routes() -> Router {
    routes::routes() // Call the combined routes from src/routes/mod.rs
}


#[tokio::main]
async fn main() {
    // 1. load .env (safe to ignore if the file is missing)
    dotenv().ok();

    let statics = ServeDir::new("src/static");

    let app: Router = routes::routes()
    .nest_service("/static", statics);

    // 3. bind & serve
    let addr = "0.0.0.0:3000";
    let listener = TcpListener::bind(addr).await.expect("bind TCP");
    println!("Listening on http://{}", addr);

    axum::serve(listener, app.into_make_service())
        .await
        .expect("server error");
}