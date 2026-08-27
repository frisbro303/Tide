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

function setValue(el, value, selectionStart, selectionEnd) {
  el.value = value;
  el.selectionStart = selectionStart;
  el.selectionEnd = selectionEnd;
  el.dispatchEvent(new Event("input", { bubbles: true }));
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

export function setupEditorNiceties() {
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
          const newValue = value.slice(0, start) + key + selected + close + value.slice(end);
          setValue(el, newValue, start + 1, end + 1);
          return;
        }

        if (FENCE_PAIRS[key] && value[start] === key) {
          event.preventDefault();
          setValue(el, value, start + 1, start + 1);
          return;
        }

        event.preventDefault();
        const newValue = value.slice(0, start) + key + close + value.slice(start);
        setValue(el, newValue, start + 1, start + 1);
        return;
      }

      if (CLOSE_CHARS.has(key) && start === end && value[start] === key) {
        event.preventDefault();
        setValue(el, value, start + 1, start + 1);
        return;
      }

      if (key === "Backspace" && start === end && start > 0) {
        const before = value[start - 1];
        const after = value[start];
        const isPair = OPEN_TO_CLOSE[before] === after || (FENCE_PAIRS[before] && FENCE_PAIRS[before] === after);
        if (isPair) {
          event.preventDefault();
          const newValue = value.slice(0, start - 1) + value.slice(start + 1);
          setValue(el, newValue, start - 1, start - 1);
          return;
        }
      }

      if (key === "Tab") {
        event.preventDefault();
        if (start !== end && value.slice(start, end).includes("\n")) {
          const lineStart = value.lastIndexOf("\n", start - 1) + 1;
          const before = value.slice(0, lineStart);
          const selected = value.slice(lineStart, end);
          const after = value.slice(end);
          const newSelected = event.shiftKey
            ? selected.replace(/^ {1,2}/gm, "")
            : selected.replace(/^/gm, "  ");
          setValue(el, before + newSelected + after, lineStart, lineStart + newSelected.length);
          return;
        }
        const newValue = value.slice(0, start) + "  " + value.slice(end);
        setValue(el, newValue, start + 2, start + 2);
        return;
      }

      if (key === "Enter" && start === end) {
        const indent = currentLineIndent(value, start);
        if (indent) {
          event.preventDefault();
          const newValue = value.slice(0, start) + "\n" + indent + value.slice(end);
          setValue(el, newValue, start + 1 + indent.length, start + 1 + indent.length);
        }
      }
    },
    true
  );
}
