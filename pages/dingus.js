// The conversion runs in a worker, so typing is never held up by it. Only one
// conversion is in flight at a time and, while it runs, only the latest text is
// kept, so the preview catches up in one step rather than falling behind.
const worker = new Worker(new URL("./worker.js" + new URL(import.meta.url).search, import.meta.url), {
  type: "module",
});

let busy = false;
let pending = null;
let handler = null;

function send(input) {
  busy = true;
  worker.postMessage({ id: 0, input });
}

worker.addEventListener("message", (event) => {
  busy = false;
  const res = event.data;
  if (pending !== null) {
    // The text has changed since this conversion began, so show the next one.
    const input = pending;
    pending = null;
    send(input);
    return;
  }
  handler(res);
});

worker.addEventListener("error", (event) => {
  console.error("worker failed:", event.message);
});

function convert(input, callback) {
  handler = callback;
  if (busy) {
    pending = input;
  } else {
    send(input);
  }
}

function update(element, value, error) {
  if (!element) {
    console.error("cannot update non-existent element");
    return;
  }
  convert(value, (res) => {
    if (0 === res.status) {
      error.style.display = "none";
      element.style.opacity = "1";
      element.innerHTML = res.stdout;
    } else {
      const message = res.stderr.split("\n")[0].replace(/^error: /, "");
      element.style.opacity = "0.25";
      error.textContent = `Error: ${message || "could not parse input"}`;
      error.style.display = "block";
    }
  });
}

document.addEventListener("DOMContentLoaded", async () => {
  const inputEl = document.getElementById("input");
  const outputEl = document.getElementById("output");
  const errorEl = document.getElementById("error");

  if (!inputEl) {
    console.error("#input not found");
    return;
  }

  if (!outputEl) {
    console.error("#output not found");
    return;
  }

  inputEl.addEventListener("input", (event) => {
    update(outputEl, event.target.value, errorEl);
  });

  const example_url = new URL("./example.predoc?202509071700", import.meta.url);
  const example_resp = await fetch(example_url);
  if (!example_resp.ok) {
    throw new Error(`HTTP error! status: ${example_resp.status}`);
  }
  const example_text = await example_resp.text();

  inputEl.value = example_text;
  update(outputEl, inputEl.value, errorEl);
});
