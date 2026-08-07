import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const expectedVersion = process.argv[2];

if (!expectedVersion) {
  console.error("usage: node res/tools/wasm-smoke.mjs <janet-version>");
  process.exit(2);
}

assert.match(
  expectedVersion,
  /^\d+\.\d+\.\d+$/,
  `invalid Janet version: ${expectedVersion}`,
);

const pagesUrl = new URL("../../pages/", import.meta.url);
const dingus = await readFile(new URL("dingus.js", pagesUrl), "utf8");
const moduleMatches = [
  ...dingus.matchAll(/^import init from "\.\/(janet\.[0-9a-f]{12}\.js)";$/gm),
];

assert.equal(
  moduleMatches.length,
  1,
  "pages/dingus.js must import exactly one content-addressed Janet module",
);

const moduleName = moduleMatches[0][1];
const { default: init } = await import(new URL(moduleName, pagesUrl));
const vm = await init();

const runJanet = (source) =>
  vm.ccall("run_janet", "string", ["string"], [source]);

assert.equal(
  runJanet("(string janet/version)"),
  expectedVersion,
  "the embedded Janet version does not match",
);

assert.equal(
  runJanet('(convert "predoc" "Load the **jump** program.")'),
  '<div class="manpage">\n' +
    '<p>Load the <span class="command">jump</span> program.</p>\n' +
    "</div>",
  "Predoc conversion returned unexpected HTML",
);

console.log(`Wasm smoke test passed (${moduleName}, Janet ${expectedVersion})`);
