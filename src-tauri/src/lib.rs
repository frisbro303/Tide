mod local_db;
mod typst_highlight;
mod typst_renderer;

use local_db::{db_clear_ops, db_get_ops, db_insert_ops};
use tauri_plugin_store::StoreExt;
use typst_highlight::highlight_typst;
use typst_renderer::{render_as_svg, RenderOptions};

#[tauri::command]
fn render_typst(
    raw_typst: &str,
    ink: &str,
    preamble: &str,
    images: Vec<(String, String)>,
) -> (u32, String) {
    use base64::Engine;

    let attachments: Vec<(String, Vec<u8>)> = images
        .into_iter()
        .filter_map(|(name, data)| {
            base64::engine::general_purpose::STANDARD
                .decode(data)
                .ok()
                .map(|bytes| (name, bytes))
        })
        .collect();

    match render_as_svg(
        raw_typst,
        RenderOptions {
            ink,
            preamble,
            attachments: &attachments,
            ..RenderOptions::default()
        },
    ) {
        Ok(svg) => (0, svg),
        Err(err) => (1, err),
    }
}

#[tauri::command]
fn store_set(app: tauri::AppHandle, key: String, value: serde_json::Value) -> Result<(), String> {
    let store = app.store("app.json").map_err(|e| e.to_string())?;
    store.set(key, value);
    store.save().map_err(|e| e.to_string())?;
    Ok(())
}

#[tauri::command]
fn store_get(app: tauri::AppHandle, key: String) -> Result<Option<serde_json::Value>, String> {
    let store = app.store("app.json").map_err(|e| e.to_string())?;
    Ok(store.get(key))
}

#[tauri::command]
fn store_delete(app: tauri::AppHandle, key: String) -> Result<(), String> {
    let store = app.store("app.json").map_err(|e| e.to_string())?;
    store.delete(key);
    store.save().map_err(|e| e.to_string())?;
    Ok(())
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_store::Builder::new().build())
        .invoke_handler(tauri::generate_handler![
            render_typst,
            store_set,
            store_get,
            store_delete,
            db_insert_ops,
            db_get_ops,
            db_clear_ops,
            highlight_typst
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
