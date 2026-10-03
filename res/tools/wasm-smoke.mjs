import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

// Node's fetch does not read file: URLs, which is how the loader finds its files.
const fetchUrl = globalThis.fetch;
globalThis.fetch = async (url, ...rest) =>
  new URL(url).protocol === "file:"
    ? new Response(await readFile(new URL(url)))
    : fetchUrl(url, ...rest);

import { load } from "../../pages/predoc/dingus.js";

const program = await load();
const run = (args, stdin) => program.run({ args: ["predoc", ...args], stdin });

const html = ["--no-ad", "--name", "predoc", "--format", "html", "--output", "-", "-"];

const version = await run(["--version"], "");
assert.equal(version.status, 0, "--version failed");
assert.match(version.stdout, /^\S+\n$/, "--version printed unexpected output");

const converted = await run(html, "Load the **jump** program.");
assert.equal(converted.status, 0, converted.stderr);
assert.equal(
  converted.stdout,
  '<div class="manpage">\n' +
    '<p>Load the <span class="command">jump</span> program.</p>\n' +
    "</div>\n",
  "Predoc conversion returned unexpected HTML",
);

const bad = await run(["--name", "x", "--output", "-", "-"], "---\nTitle: foobar(1)\n---\n");
assert.equal(bad.status, 1, "bad input should fail");
assert.match(bad.stderr, /^error: could not parse date in frontmatter/);

// The instance survives the failure and starts the next conversion afresh.
const again = await run(html, "Load the **jump** program.");
assert.equal(again.stdout, converted.stdout, "a conversion after an error differed");

console.log(`Wasm smoke test passed (predoc ${version.stdout.trim()})`);
