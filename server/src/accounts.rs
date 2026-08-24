use argon2::{
    Argon2,
    password_hash::{PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
};
use axum::{
    Json, Router,
    extract::{Query, State},
    routing::{get, post},
};
use chrono::{DateTime, Duration, Utc};
use rand_core::OsRng as ArgonOsRng;
use serde::{Deserialize, Serialize};
use sqlx::PgPool;
use std::env;
use uuid::Uuid;

use crate::error::AppError;
use crate::mailer::send_email;
use crate::tokens::generate_token;

#[derive(Deserialize)]
struct CreateAccountRequest {
    email: String,
    password: String,
}

#[derive(Deserialize)]
struct LoginRequest {
    email: String,
    password: String,
}

#[derive(Deserialize)]
struct VerifyQuery {
    token: String,
}

#[derive(Deserialize)]
struct PasswordResetRequest {
    email: String,
}

#[derive(Deserialize)]
struct PasswordResetConfirm {
    token: String,
    new_password: String,
}

#[derive(Serialize)]
struct AccountResponse {
    id: Uuid,
    email: String,
    created_at: DateTime<Utc>,
}

fn hash_password(password: &str) -> Result<String, AppError> {
    let salt = SaltString::generate(&mut ArgonOsRng);
    Argon2::default()
        .hash_password(password.as_bytes(), &salt)
        .map(|h| h.to_string())
        .map_err(|e| AppError::Internal(e.to_string()))
}

async fn create_account(
    State(pool): State<PgPool>,
    Json(req): Json<CreateAccountRequest>,
) -> Result<Json<AccountResponse>, AppError> {
    let password_hash = hash_password(&req.password)?;

    let row = sqlx::query_as!(
        AccountResponse,
        r#"INSERT INTO accounts (email, password_hash) VALUES ($1, $2)
           RETURNING id, email, created_at"#,
        req.email,
        password_hash
    )
    .fetch_one(&pool)
    .await?;

    let token = generate_token();
    let expires_at = Utc::now() + Duration::hours(24);

    sqlx::query!(
        r#"INSERT INTO verification_tokens (account_id, token, expires_at) VALUES ($1, $2, $3)"#,
        row.id,
        token,
        expires_at
    )
    .execute(&pool)
    .await?;

    let base_url = env::var("APP_BASE_URL").unwrap_or_default();
    let link = format!("{}/verify?token={}", base_url, token);
    let _ = send_email(
        &row.email,
        "Verify your account",
        &format!("<p>Click to verify: <a href=\"{}\">{}</a></p>", link, link),
    )
    .await;

    Ok(Json(row))
}

async fn verify(
    State(pool): State<PgPool>,
    Query(params): Query<VerifyQuery>,
) -> Result<&'static str, AppError> {
    let row = sqlx::query!(
        r#"SELECT account_id, expires_at FROM verification_tokens WHERE token = $1"#,
        params.token
    )
    .fetch_optional(&pool)
    .await?
    .ok_or(AppError::NotFound)?;

    if row.expires_at < Utc::now() {
        return Err(AppError::NotFound);
    }

    sqlx::query!(
        r#"UPDATE accounts SET email_verified = true WHERE id = $1"#,
        row.account_id
    )
    .execute(&pool)
    .await?;

    sqlx::query!(
        r#"DELETE FROM verification_tokens WHERE token = $1"#,
        params.token
    )
    .execute(&pool)
    .await?;

    Ok("verified")
}

async fn login(
    State(pool): State<PgPool>,
    Json(req): Json<LoginRequest>,
) -> Result<Json<AccountResponse>, AppError> {
    let row = sqlx::query!(
        r#"SELECT id, email, password_hash, email_verified, created_at FROM accounts WHERE email = $1"#,
        req.email
    )
    .fetch_optional(&pool)
    .await?
    .ok_or(AppError::NotFound)?;

    let parsed_hash =
        PasswordHash::new(&row.password_hash).map_err(|e| AppError::Internal(e.to_string()))?;

    Argon2::default()
        .verify_password(req.password.as_bytes(), &parsed_hash)
        .map_err(|_| AppError::NotFound)?;

    if !row.email_verified {
        return Err(AppError::Internal("email not verified".to_string()));
    }

    Ok(Json(AccountResponse {
        id: row.id,
        email: row.email,
        created_at: row.created_at,
    }))
}

async fn request_password_reset(
    State(pool): State<PgPool>,
    Json(req): Json<PasswordResetRequest>,
) -> Result<&'static str, AppError> {
    let account = sqlx::query!(r#"SELECT id FROM accounts WHERE email = $1"#, req.email)
        .fetch_optional(&pool)
        .await?;

    if let Some(account) = account {
        let token = generate_token();
        let expires_at = Utc::now() + Duration::hours(1);

        sqlx::query!(
            r#"INSERT INTO password_reset_tokens (account_id, token, expires_at) VALUES ($1, $2, $3)"#,
            account.id,
            token,
            expires_at
        )
        .execute(&pool)
        .await?;

        let base_url = env::var("APP_BASE_URL").unwrap_or_default();
        let link = format!("{}/password_reset/confirm?token={}", base_url, token);
        let _ = send_email(
            &req.email,
            "Reset your password",
            &format!("<p>Click to reset: <a href=\"{}\">{}</a></p>", link, link),
        )
        .await;
    }

    // Always return the same response, whether or not the email exists,
    // so the endpoint doesn't leak which emails are registered.
    Ok("if that email exists, a reset link has been sent")
}

async fn confirm_password_reset(
    State(pool): State<PgPool>,
    Json(req): Json<PasswordResetConfirm>,
) -> Result<&'static str, AppError> {
    let row = sqlx::query!(
        r#"SELECT account_id, expires_at FROM password_reset_tokens WHERE token = $1"#,
        req.token
    )
    .fetch_optional(&pool)
    .await?
    .ok_or(AppError::NotFound)?;

    if row.expires_at < Utc::now() {
        return Err(AppError::NotFound);
    }

    let password_hash = hash_password(&req.new_password)?;

    sqlx::query!(
        r#"UPDATE accounts SET password_hash = $1, updated_at = now() WHERE id = $2"#,
        password_hash,
        row.account_id
    )
    .execute(&pool)
    .await?;

    sqlx::query!(
        r#"DELETE FROM password_reset_tokens WHERE token = $1"#,
        req.token
    )
    .execute(&pool)
    .await?;

    Ok("password updated")
}

async fn list_accounts() -> &'static str {
    "accounts"
}

pub fn router() -> Router<PgPool> {
    Router::new()
        .route("/accounts", get(list_accounts).post(create_account))
        .route("/verify", get(verify))
        .route("/login", post(login))
        .route("/password_reset/request", post(request_password_reset))
        .route("/password_reset/confirm", post(confirm_password_reset))
}
