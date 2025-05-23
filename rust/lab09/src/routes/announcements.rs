use axum::{
    extract::Query,
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use chrono::{DateTime, Utc};
use reqwest::header::HeaderValue;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, env};
use crate::render_html::render_html;

/// Represents a GitLab issue, used for announcements.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct GitLabIssue {
    pub id: i32,
    pub iid: i32,
    pub project_id: i32,
    pub title: String,
    pub description: Option<String>,
    pub state: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub closed_at: Option<DateTime<Utc>>,
    pub labels: Vec<String>,
    pub author: GitLabAuthor,
    pub web_url: String,
}

/// Represents the author of a GitLab issue.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct GitLabAuthor {
    pub id: i32,
    pub username: String,
    pub name: String,
    pub avatar_url: Option<String>,
    pub web_url: String,
}

/// URL for fetching opened announcements from GitLab.
const GITLAB_URL: &str =
    "https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened";

/// Fetches announcements from the GitLab API.
async fn fetch_gitlab_announcements() -> Result<Vec<GitLabIssue>, reqwest::Error> {
    let client = reqwest::Client::new();
    let mut req = client.get(GITLAB_URL);

    // Add private token if available.
    if let Ok(token) = env::var("GITTOKEN") {
        let hv = HeaderValue::from_str(&token).expect("GITTOKEN contains invalid chars");
        req = req.header("PRIVATE-TOKEN", hv);
    }

    req.send().await?.error_for_status()?.json::<Vec<GitLabIssue>>().await
}

/// Handler for the `/announcements` endpoint.
pub async fn announcements_handler(
    Query(params): Query<HashMap<String, String>>,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    // Determine output format, defaults to "html".
    let format = params.get("format").map(|s| s.as_str()).unwrap_or("html");

    // Fetch announcements and handle errors.
    let mut ann = fetch_gitlab_announcements()
        .await
        .map_err(|e| {
            eprintln!("GitLab fetch failed: {e}");
            (StatusCode::BAD_GATEWAY, "GitLab fetch failed".into())
        })?;

    // Sort announcements by creation date (earliest to latest).
    ann.sort_by(|a, b| a.created_at.cmp(&b.created_at));

    // Return announcements in the requested format.
    match format {
        "json" => Ok(Json(ann).into_response()),
        "html" => Ok(render_html(ann).await?.into_response()),
        _ => Err((StatusCode::BAD_REQUEST, "format must be json or html".into())),
    }
}

/// Defines the Axum router for the announcements module.
pub fn routes() -> Router {
    Router::new().route("/announcements", get(announcements_handler))
}