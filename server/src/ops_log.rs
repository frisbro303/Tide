use axum::{
    Json, Router,
    extract::{Path, State},
    routing::post,
};
use serde::Serialize;
use serde_json::Value;
use sqlx::PgPool;
use uuid::Uuid;

use crate::error::AppError;

#[derive(Serialize)]
struct OpsLogEntry {
    id: Uuid,
    entry: Value,
    created_at: chrono::DateTime<chrono::Utc>,
}

async fn append_entry(
    State(pool): State<PgPool>,
    Path(account_id): Path<Uuid>,
    Json(entry): Json<Value>,
) -> Result<Json<OpsLogEntry>, AppError> {
    let row = sqlx::query_as!(
        OpsLogEntry,
        r#"INSERT INTO ops_log (account_id, entry) VALUES ($1, $2)
           RETURNING id, entry, created_at"#,
        account_id,
        entry
    )
    .fetch_one(&pool)
    .await?;

    Ok(Json(row))
}

async fn list_entries(
    State(pool): State<PgPool>,
    Path(account_id): Path<Uuid>,
) -> Result<Json<Vec<OpsLogEntry>>, AppError> {
    let rows = sqlx::query_as!(
        OpsLogEntry,
        r#"SELECT id, entry, created_at FROM ops_log
           WHERE account_id = $1 ORDER BY created_at ASC"#,
        account_id
    )
    .fetch_all(&pool)
    .await?;

    Ok(Json(rows))
}

pub fn router() -> Router<PgPool> {
    Router::new().route(
        "/accounts/{id}/ops_log",
        post(append_entry).get(list_entries),
    )
}
