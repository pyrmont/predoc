// Converts in a worker so that a conversion never blocks the page, whatever
// the length of the document. The runtime and the image are loaded once and
// the loader reuses one instance for every conversion.
const search = new URL(import.meta.url).search;

const ready = import("./predoc/dingus.js" + search).then((m) => m.load());

// The program's name comes first, as main takes it before the options.
const args = ["predoc", "--no-ad", "--name", "predoc", "--format", "html", "--output", "-", "-"];

self.addEventListener("message", async (event) => {
  const { id, input } = event.data;
  try {
    const program = await ready;
    self.postMessage({ id, ...(await program.run({ args, stdin: input })) });
  } catch (e) {
    self.postMessage({ id, status: 1, stdout: "", stderr: String(e) });
  }
});
