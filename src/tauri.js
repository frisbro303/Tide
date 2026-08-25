export function setupTauri(app) {
  app.ports.compileTypst.subscribe(async (rawTypst) => {
    const result = await window.__TAURI__.core.invoke("render_typst", {
      rawTypst,
    });
    app.ports.rawTypstCompiled.send(result);
  });

  app.ports.setPort.subscribe(async ({ key, value }) => {
    await window.__TAURI__.core.invoke("store_set", { key, value });
  });

  app.ports.deletePort.subscribe(async (key) => {
    await window.__TAURI__.core.invoke("store_delete", { key });
  });

  app.ports.getPort.subscribe(async (key) => {
    const value = await window.__TAURI__.core.invoke("store_get", { key });
    app.ports.loadedPort.send({ key, value: value ?? null });
  });
}
