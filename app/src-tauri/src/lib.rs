mod typst_renderer;

use tauri_plugin_store::StoreExt;
use typst_renderer::{render_as_svg, RenderOptions};

#[tauri::command]
fn render_typst(raw_typst: &str) -> (u32, String) {
    match render_as_svg(raw_typst, RenderOptions::default()) {
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
            store_delete
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
