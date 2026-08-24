export function setupTauri(app) {
  app.ports.compileTypst.subscribe(async (rawTypst) => {
    const result = await window.__TAURI__.core.invoke("render_typst", {
      rawTypst,
    });

    app.ports.rawTypstCompiled.send(result);
  });
}
