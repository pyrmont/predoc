import init from "./janet.282246c4.js";

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

function update(element, value, error) {
  if (!element) {
    console.error("cannot update non-existent element");
    return;
  }
  const res = convert(value);
  if("" !== res) {
    error.style.display = "none";
    element.style.opacity = "1";
    element.innerHTML = res;
  } else {
    element.style.opacity = "0.25";
    error.textContent = "Error: could not parse input";
    error.style.display = "block";
  }
}

document.addEventListener("DOMContentLoaded", async () => {
  const inputEl = document.getElementById("input");
  const outputEl = document.getElementById("output");
  const errorEl = document.getElementById("error");

  try {
    vm = await init();
    console.log("Wasm init OK");
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
    update(outputEl, event.target.value, errorEl);
  });

  const example_url = new URL("./example.predoc?202509071700", import.meta.url);
  const example_resp = await fetch(example_url);
  if (!example_resp.ok) {
    throw new Error(`HTTP error! status: ${example_resp.status}`);
  }
  const example_text = await example_resp.text();

  inputEl.value = example_text;
  console.log(inputEl.value);
  update(outputEl, inputEl.value, errorEl);
});
