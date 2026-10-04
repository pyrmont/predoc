import { start } from "./wasi-10d007777ae16156.js";

export async function load({
  wasm = new URL("./wattle-10d007777ae16156.wasm", import.meta.url),
  image = new URL("./dingus-10d007777ae16156.wimage", import.meta.url),
} = {}) {
  const [binary, bytes] = await Promise.all([
    fetch(wasm).then((response) => response.arrayBuffer()),
    fetch(image).then((response) => response.arrayBuffer()),
  ]);
  const module = await WebAssembly.compile(binary);
  const program = new Uint8Array(bytes);
  let instance = null;
  return {
    async run({ args, stdin, fresh = false } = {}) {
      if (fresh || !instance) instance = await start(module);
      const result = instance.runImage(program, { args, stdin });
      if (result.error) instance = null;
      return result;
    },
  };
}

export async function run(options = {}) {
  return (await load(options)).run(options);
}
