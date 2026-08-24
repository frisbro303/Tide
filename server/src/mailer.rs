use serde_json::json;
use std::env;

pub async fn send_email(to: &str, subject: &str, html: &str) -> Result<(), String> {
    let api_key = env::var("RESEND_API_KEY").map_err(|_| "RESEND_API_KEY not set".to_string())?;
    let from =
        env::var("RESEND_FROM_EMAIL").map_err(|_| "RESEND_FROM_EMAIL not set".to_string())?;

    let client = reqwest::Client::new();
    let res = client
        .post("https://api.resend.com/emails")
        .bearer_auth(api_key)
        .json(&json!({
            "from": from,
            "to": [to],
            "subject": subject,
            "html": html,
        }))
        .send()
        .await
        .map_err(|e| e.to_string())?;

    if !res.status().is_success() {
        let body = res.text().await.unwrap_or_default();
        return Err(format!("resend error: {}", body));
    }

    Ok(())
}
