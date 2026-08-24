mod typst_renderer;

use typst_renderer::{render_as_svg, RenderOptions};

#[tauri::command]
fn render_typst(raw_typst: &str) -> (u32, String) {
    match render_as_svg(raw_typst, RenderOptions::default()) {
        Ok(svg) => (0, svg),
        Err(err) => (1, err),
    }
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .invoke_handler(tauri::generate_handler![render_typst])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
