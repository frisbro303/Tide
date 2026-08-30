const inkFor = (darkMode) => (darkMode ? "#e8e9ec" : "#1c1e21");

function isDarkMode() {
  const theme = document.documentElement.getAttribute("data-theme");
  if (theme === "light") return false;
  if (theme === "dark") return true;
  return window.matchMedia("(prefers-color-scheme: dark)").matches;
}

export function setupTauri(app) {
  app.ports.compileTypstPort.subscribe(async ({ requestId, source: rawTypst, preamble, images }) => {
    const [status, output] = await window.__TAURI__.core.invoke("render_typst", {
      rawTypst,
      ink: inkFor(isDarkMode()),
      preamble,
      images,
    });
    app.ports.rawTypstCompiledPort.send([requestId, status, output]);
  });

  app.ports.highlightTypstPort.subscribe(async ([requestId, source]) => {
    const tree = await window.__TAURI__.core.invoke("highlight_typst", { source });
    app.ports.typstHighlightedPort.send([requestId, tree]);
  });

  app.ports.focusField.subscribe((id) => {
    // The target textarea doesn't exist yet at dispatch time (it appears
    // only once Elm's own re-render has patched it in); rAF pushes past that.
    requestAnimationFrame(() => {
      const el = document.getElementById(id);
      if (el) {
        el.focus();
        el.setSelectionRange(el.value.length, el.value.length);
      }
    });
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

  app.ports.insertOpsPort.subscribe(async (ops) => {
    await window.__TAURI__.core.invoke("db_insert_ops", { ops });
  });

  app.ports.requestOpsPort.subscribe(async () => {
    const ops = await window.__TAURI__.core.invoke("db_get_ops");
    app.ports.opsLoadedPort.send(ops);
  });

  app.ports.clearOpsPort.subscribe(async () => {
    await window.__TAURI__.core.invoke("db_clear_ops");
  });

  app.ports.setThemePort.subscribe((theme) => {
    document.documentElement.setAttribute("data-theme", theme);
  });

  app.ports.exportDataPort.subscribe((json) => {
    const blob = new Blob([json], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "tide-backup.json";
    a.click();
    URL.revokeObjectURL(url);
  });

  app.ports.requestImportPort.subscribe(() => {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "application/json";
    input.onchange = () => {
      const file = input.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = () => {
        app.ports.importLoadedPort.send(reader.result);
      };
      reader.readAsText(file);
    };
    input.click();
  });
}
