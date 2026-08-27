use rusqlite::{params, Connection};
use tauri::Manager;

fn get_connection(app: &tauri::AppHandle) -> Result<Connection, String> {
    let dir = app.path().app_data_dir().map_err(|e| e.to_string())?;
    std::fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let conn = Connection::open(dir.join("app.db")).map_err(|e| e.to_string())?;
    conn.execute(
        "CREATE TABLE IF NOT EXISTS ops_log (
            id TEXT PRIMARY KEY,
            created_at TEXT NOT NULL,
            entry TEXT NOT NULL
        )",
        [],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn)
}

#[tauri::command]
pub fn db_insert_ops(app: tauri::AppHandle, ops: Vec<serde_json::Value>) -> Result<(), String> {
    let mut conn = get_connection(&app)?;
    let tx = conn.transaction().map_err(|e| e.to_string())?;
    {
        let mut stmt = tx
            .prepare("INSERT OR IGNORE INTO ops_log (id, created_at, entry) VALUES (?1, ?2, ?3)")
            .map_err(|e| e.to_string())?;
        for op in &ops {
            let id = op.get("id").and_then(|v| v.as_str()).ok_or("missing id")?;
            let created_at = op
                .get("created_at")
                .and_then(|v| v.as_str())
                .ok_or("missing created_at")?;
            let entry = op.get("entry").ok_or("missing entry")?;
            let entry_str = serde_json::to_string(entry).map_err(|e| e.to_string())?;
            stmt.execute(params![id, created_at, entry_str])
                .map_err(|e| e.to_string())?;
        }
    }
    tx.commit().map_err(|e| e.to_string())?;
    Ok(())
}

#[tauri::command]
pub fn db_clear_ops(app: tauri::AppHandle) -> Result<(), String> {
    let conn = get_connection(&app)?;
    conn.execute("DELETE FROM ops_log", [])
        .map_err(|e| e.to_string())?;
    Ok(())
}

#[tauri::command]
pub fn db_get_ops(app: tauri::AppHandle) -> Result<Vec<serde_json::Value>, String> {
    let conn = get_connection(&app)?;
    let mut stmt = conn
        .prepare("SELECT id, created_at, entry FROM ops_log ORDER BY created_at")
        .map_err(|e| e.to_string())?;

    let rows = stmt
        .query_map([], |row| {
            let id: String = row.get(0)?;
            let created_at: String = row.get(1)?;
            let entry_str: String = row.get(2)?;
            Ok((id, created_at, entry_str))
        })
        .map_err(|e| e.to_string())?;

    let mut ops = Vec::new();
    for row in rows {
        let (id, created_at, entry_str) = row.map_err(|e| e.to_string())?;
        let entry: serde_json::Value =
            serde_json::from_str(&entry_str).map_err(|e| e.to_string())?;
        ops.push(serde_json::json!({
            "id": id,
            "created_at": created_at,
            "entry": entry
        }));
    }
    Ok(ops)
}
