import init from "./janet.js?202509061955";

let vm;

function convert(input) {
  const source = input
    .replace(/\\/g, '\\\\')  // backslashes
    .replace(/"/g, '\\"')    // double quotes
    .replace(/\n/g, '\\n');   // newlines
  try {
    const res = vm.ccall(
      "run_janet",
      "string",
      ["string"],
      [`(convert "predoc" "${source}")`]
    );
    console.log(res);
    return res;
  } catch (e) {
    console.error("vm.ccall failed:", e);
    return "";
  }
}

function update(element, value) {
  if (!element) {
    console.error("cannot update non-existent element");
    return;
  }
  const res = convert(value);
  element.innerHTML = res;
}

document.addEventListener("DOMContentLoaded", async () => {
  const inputEl = document.getElementById("input");
  const outputEl = document.getElementById("output");

  try {
    vm = await init();
    console.log("WASM init OK");
  } catch (e) {
    console.error("init() failed:", e);
    return;
  }

  if (!inputEl) {
    console.error("#input not found");
    return;
  }

  if (!outputEl) {
    console.error("#output not found");
    return;
  }

  inputEl.addEventListener("input", (event) => {
    update(outputEl, event.target.value);
  });

  const example_url = new URL("./example.predoc", import.meta.url);
  const example_resp = await fetch(example_url);
  if (!example_resp.ok) {
    throw new Error(`HTTP error! status: ${example_resp.status}`);
  }
  const example_text = await example_resp.text();

  inputEl.value = example_text;
  console.log(inputEl.value);
  update(outputEl, inputEl.value);
});
