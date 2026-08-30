const OPEN_TO_CLOSE = {
  "(": ")",
  "[": "]",
  "{": "}",
};

const FENCE_PAIRS = {
  '"': '"',
  "`": "`",
  $: "$",
};

const CLOSE_CHARS = new Set(Object.values(OPEN_TO_CLOSE));

function isEditorField(el) {
  return el instanceof HTMLTextAreaElement && el.classList.contains("note-editor-field");
}

// Deliberately not `el.value = ...`: a direct property assignment doesn't
// join the textarea's native undo stack (and can silently clear it), so
// Cmd+Z wouldn't be able to step back through auto-inserted brackets/fences/
// indentation. `execCommand` is deprecated but still fully supported by
// WebKit (what Tauri uses on macOS) for exactly this purpose — it's treated
// as a real edit, so undo/redo keeps working.
function insertText(el, start, end, text) {
  el.setSelectionRange(start, end);
  document.execCommand("insertText", false, text);
}

function deleteRange(el, start, end) {
  el.setSelectionRange(start, end);
  document.execCommand("delete");
}

function moveCursor(el, position) {
  el.setSelectionRange(position, position);
}

function currentLineIndent(value, pos) {
  const lineStart = value.lastIndexOf("\n", pos - 1) + 1;
  const match = value.slice(lineStart, pos).match(/^[ \t]*/);
  return match ? match[0] : "";
}

function focusField(el) {
  el.focus();
  el.setSelectionRange(el.value.length, el.value.length);
}

const ADD_FIELD_CHAIN = {
  "editable-typst-add-front": "editable-typst-add-back",
};

const MAX_IMAGE_BASE64_LENGTH = 2 * 1024 * 1024; // ~1.5MB of actual image bytes

function blobToPngBase64(blob) {
  return new Promise((resolve, reject) => {
    const objectUrl = URL.createObjectURL(blob);
    const img = new Image();
    img.onload = () => {
      const canvas = document.createElement("canvas");
      canvas.width = img.naturalWidth;
      canvas.height = img.naturalHeight;
      canvas.getContext("2d").drawImage(img, 0, 0);
      URL.revokeObjectURL(objectUrl);
      const dataUrl = canvas.toDataURL("image/png");
      const base64 = dataUrl.slice(dataUrl.indexOf(",") + 1);
      if (base64.length > MAX_IMAGE_BASE64_LENGTH) {
        reject(new Error("too-large"));
        return;
      }
      resolve(base64);
    };
    img.onerror = () => {
      URL.revokeObjectURL(objectUrl);
      reject(new Error("Could not load image"));
    };
    img.src = objectUrl;
  });
}

function insertImageAtCursor(el, base64) {
  const id = crypto.randomUUID();
  const { selectionStart: start, selectionEnd: end } = el;
  const reference = `#image("${id}.png", width: 100%)`;
  insertText(el, start, end, reference);
  el.dispatchEvent(new CustomEvent("tide-image-added", { detail: { id, data: base64 }, bubbles: true }));
}

async function convertAndInsertImage(el, file) {
  try {
    const base64 = await blobToPngBase64(file);
    insertImageAtCursor(el, base64);
  } catch (err) {
    if (err instanceof Error && err.message === "too-large") {
      alert("That image is too large to add to a card (limit ~1.5MB). Try a smaller image or a screenshot of just the relevant part.");
    }
  }
}

function imageFileFromClipboard(clipboardData) {
  if (!clipboardData) return null;
  for (const item of clipboardData.items) {
    if (item.kind === "file" && item.type.startsWith("image/")) {
      return item.getAsFile();
    }
  }
  return null;
}

function imageFileFromDataTransfer(dataTransfer) {
  if (!dataTransfer) return null;
  for (const file of dataTransfer.files) {
    if (file.type.startsWith("image/")) return file;
  }
  return null;
}

function openImagePicker(el) {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = "image/*";
  input.onchange = async () => {
    const file = input.files[0];
    if (!file) return;
    await convertAndInsertImage(el, file);
  };
  input.click();
}

function setupImageInsertion() {
  document.addEventListener(
    "paste",
    async (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;
      const file = imageFileFromClipboard(event.clipboardData);
      if (!file) return;
      event.preventDefault();
      await convertAndInsertImage(el, file);
    },
    true
  );

  // Always prevent the default so a drop that misses a field by a few
  // pixels can't fall through to the browser's default behavior, which is
  // to navigate the whole webview to the dropped file.
  document.addEventListener("dragover", (event) => {
    event.preventDefault();
  });

  document.addEventListener("drop", async (event) => {
    event.preventDefault();
    const el = event.target;
    if (!isEditorField(el)) return;
    const file = imageFileFromDataTransfer(event.dataTransfer);
    if (!file) return;
    focusField(el);
    await convertAndInsertImage(el, file);
  });

  document.addEventListener("contextmenu", (event) => {
    const el = event.target;
    if (isEditorField(el)) {
      event.preventDefault();
      focusField(el);
      openImagePicker(el);
      return;
    }
    // Suppress WKWebView's default context menu everywhere else (it
    // includes browser chrome like "Reload" that has no place in a
    // packaged desktop app) — but leave it alone on plain text inputs
    // (email/password fields) so cut/copy/paste still works there.
    if (el instanceof HTMLInputElement) return;
    event.preventDefault();
  });
}

function setupWindowBlurGuard() {
  let fieldToRefocus = null;

  document.addEventListener(
    "blur",
    (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;
      // The OS took focus away from the whole window (Cmd+Tab, clicking
      // another app, a system dialog) rather than focus moving to something
      // else on the page — don't let that read as "the user clicked away",
      // which would commit the draft and collapse the field back to its
      // preview. Common when copying from a PDF viewer alongside the app.
      if (!document.hasFocus()) {
        event.stopImmediatePropagation();
        fieldToRefocus = el;
      }
    },
    true
  );

  window.addEventListener("focus", () => {
    if (fieldToRefocus) {
      const el = fieldToRefocus;
      fieldToRefocus = null;
      el.focus();
    }
  });
}

export function setupEditorNiceties() {
  setupImageInsertion();
  setupWindowBlurGuard();
  document.addEventListener(
    "keydown",
    (event) => {
      const el = event.target;
      if (!isEditorField(el)) return;

      const { value, selectionStart: start, selectionEnd: end } = el;
      const key = event.key;

      if (key === "Escape") {
        event.preventDefault();
        event.stopPropagation();
        el.blur();
        return;
      }

      if (key === "Enter" && event.shiftKey) {
        const nextId = ADD_FIELD_CHAIN[el.id];
        if (nextId) {
          event.preventDefault();
          const nextEl = document.getElementById(nextId);
          if (nextEl) focusField(nextEl);
          return;
        }
        if (el.id === "editable-typst-add-back") {
          event.preventDefault();
          const submitButton = document.getElementById("add-submit-button");
          if (submitButton) submitButton.click();
          return;
        }
      }

      if (OPEN_TO_CLOSE[key] || FENCE_PAIRS[key]) {
        const close = OPEN_TO_CLOSE[key] || FENCE_PAIRS[key];

        if (start !== end) {
          event.preventDefault();
          const selected = value.slice(start, end);
          insertText(el, start, end, key + selected + close);
          el.setSelectionRange(start + 1, start + 1 + selected.length);
          return;
        }

        if (FENCE_PAIRS[key] && value[start] === key) {
          event.preventDefault();
          moveCursor(el, start + 1);
          return;
        }

        event.preventDefault();
        insertText(el, start, start, key + close);
        moveCursor(el, start + 1);
        return;
      }

      if (CLOSE_CHARS.has(key) && start === end && value[start] === key) {
        event.preventDefault();
        moveCursor(el, start + 1);
        return;
      }

      if (key === "Backspace" && start === end && start > 0) {
        const before = value[start - 1];
        const after = value[start];
        const isPair = OPEN_TO_CLOSE[before] === after || (FENCE_PAIRS[before] && FENCE_PAIRS[before] === after);
        if (isPair) {
          event.preventDefault();
          deleteRange(el, start - 1, start + 1);
          return;
        }
      }

      if (key === "Tab") {
        event.preventDefault();
        if (start !== end && value.slice(start, end).includes("\n")) {
          const lineStart = value.lastIndexOf("\n", start - 1) + 1;
          const selected = value.slice(lineStart, end);
          const newSelected = event.shiftKey
            ? selected.replace(/^ {1,2}/gm, "")
            : selected.replace(/^/gm, "  ");
          insertText(el, lineStart, end, newSelected);
          el.setSelectionRange(lineStart, lineStart + newSelected.length);
          return;
        }
        insertText(el, start, end, "  ");
        return;
      }

      if (key === "Enter" && start === end) {
        const indent = currentLineIndent(value, start);
        const before = value[start - 1];
        const after = value[start];

        if (OPEN_TO_CLOSE[before] === after || (FENCE_PAIRS[before] && FENCE_PAIRS[before] === after)) {
          event.preventDefault();
          const innerIndent = indent + "  ";
          insertText(el, start, end, "\n" + innerIndent + "\n" + indent);
          moveCursor(el, start + 1 + innerIndent.length);
          return;
        }

        if (indent) {
          event.preventDefault();
          insertText(el, start, end, "\n" + indent);
        }
      }
    },
    true
  );
}
