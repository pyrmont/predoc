import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

import { run } from "../../pages/wasi.js";

const pagesUrl = new URL("../../pages/", import.meta.url);
const module = await WebAssembly.compile(
  await readFile(new URL("predoc.wasm", pagesUrl)),
);

const html = ["--no-ad", "--name", "predoc", "--format", "html", "--output", "-", "-"];

const version = await run(module, ["--version"], "");
assert.equal(version.status, 0, "--version failed");
assert.match(version.stdout, /^\S+\n$/, "--version printed unexpected output");

const converted = await run(module, html, "Load the **jump** program.");
assert.equal(converted.status, 0, converted.stderr);
assert.equal(
  converted.stdout,
  '<div class="manpage">\n' +
    '<p>Load the <span class="command">jump</span> program.</p>\n' +
    "</div>\n",
  "Predoc conversion returned unexpected HTML",
);

const bad = await run(module, ["--name", "x", "--output", "-", "-"], "---\nTitle: foobar(1)\n---\n");
assert.equal(bad.status, 1, "bad input should fail");
assert.match(bad.stderr, /^error: could not parse date in frontmatter/);

console.log(`Wasm smoke test passed (predoc ${version.stdout.trim()})`);
