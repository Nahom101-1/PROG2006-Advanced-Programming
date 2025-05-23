// src/render_html.rs
use axum::{http::StatusCode, response::Html};
use html_escape::encode_text;
use regex::Regex;
use std::env;

// Bring in the GitLabIssue struct.
use crate::routes::announcements::GitLabIssue;

// Path to the HTML template file.
const TPL_PATH: &str =
    concat!(env!("CARGO_MANIFEST_DIR"), "/src/templates/announcements.html");

/// Renders a vector of `GitLabIssue` structs into an HTML page.
pub async fn render_html(ann: Vec<GitLabIssue>)
    -> Result<Html<String>, (StatusCode, String)>
{
    // Read the HTML template file.
    let mut tpl = tokio::fs::read_to_string(TPL_PATH)
        .await
        .map_err(|_| (StatusCode::INTERNAL_SERVER_ERROR, "Template not found".into()))?;

    // Build the HTML content for announcement cards.
    let body = if ann.is_empty() {
        "<p class=\"no-results-message\">No announcements found.</p>".to_owned()
    } else {
        let mut out = String::new();
        // Format each announcement into an HTML div.
        for a in ann {
            out.push_str(&format!(
                r#"<div class="announcement">
                       <h3>{}</h3>
                       <p class="date">{}</p>
                       <div class="content">{}</div>
                   </div>"#,
                &a.title, // Displaying title directly. For production, consider HTML escaping.
                a.created_at.format("%Y-%m-%d %H:%M:%S UTC"),
                encode_text(a.description.as_deref().unwrap_or(""))
            ));
        }
        out
    };

    // Replace the placeholder div in the template with the generated announcement HTML.
    let re = Regex::new(r#"(?s)<div id="announcements-list".*?</div>"#).unwrap();
    tpl = re
        .replace(
            &tpl,
            format!(
                r#"<div id="announcements-list" class="grid-container">{body}</div>"#
            ),
        )
        .into_owned();

    // Return the modified HTML template.
    Ok(Html(tpl))
}
