// The WASI imports `wattle-web.wasm` needs, and the calls a host makes to run
// Janet source in it.
//
// A browser has no WASI, so the page supplies `wasi_snapshot_preview1` as a
// JavaScript object. It provides the 32 functions a Debug or ReleaseSafe
// build imports. ReleaseSmall and ReleaseFast builds import 26 of them, all
// but `clock_res_get`, `fd_filestat_set_size`, `fd_filestat_set_times`,
// `fd_pread`, `fd_pwrite` and `fd_sync`. `test.js` fails when the binary
// imports a function missing here. Nothing here reaches a file
// system, an environment or a clock beyond the page's own: descriptors 0, 1
// and 2 are the only open ones, output on 1 and 2 is collected per call, and
// standard input is at end of file.
//
// The same file runs under Node, which is how `test.js` exercises what the
// page runs. It uses only `WebAssembly`, `TextEncoder`, `TextDecoder`,
// `performance` and `crypto.getRandomValues`, which both provide.

// WASI errno values.
const SUCCESS = 0;
const EBADF = 8;
const EINVAL = 28;
const ENOSYS = 52;
const ESPIPE = 70;

// WASI clock ids.
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 1;
const CLOCK_PROCESS_CPUTIME = 2;
const CLOCK_THREAD_CPUTIME = 3;

// The file type and the two rights `fd_fdstat_get` reports.
const FILETYPE_CHARACTER_DEVICE = 2;
const RIGHT_FD_READ = 1n << 1n;
const RIGHT_FD_WRITE = 1n << 6n;

// `crypto.getRandomValues` refuses more than this many bytes per call.
const RANDOM_CHUNK = 65536;

// Thrown by `proc_exit`, which must not return. `code` is the exit status.
export class WasiExit extends Error {
  constructor(code) {
    super(`exited with status ${code}`);
    this.name = "WasiExit";
    this.code = code;
  }
}

// Returns the import object and the three things the host does with it:
// `bind`, given the instance's memory once it exists, `take`, which returns and
// clears the output collected since the last `take`, and `setStdin`, which
// replaces what standard input holds with a string or a `Uint8Array` and
// starts reading it from the beginning.
export function createWasi() {
  let memory = null;
  let stdin = new Uint8Array(0);
  let stdinOffset = 0;
  const decoders = { 1: new TextDecoder(), 2: new TextDecoder() };
  const chunks = { 1: [], 2: [] };

  // A fresh view per call, because growing the memory replaces its buffer.
  const view = () => new DataView(memory.buffer);
  const bytes = (ptr, len) => new Uint8Array(memory.buffer, ptr, len);
  const isStdio = (fd) => fd === 0 || fd === 1 || fd === 2;

  const wasi = {
    // No environment variables: a count of zero and a size of zero.
    environ_sizes_get(countPtr, sizePtr) {
      view().setUint32(countPtr, 0, true);
      view().setUint32(sizePtr, 0, true);
      return SUCCESS;
    },
    environ_get() {
      return SUCCESS;
    },

    // Nanoseconds. Real time from `Date.now`, and the monotonic clock from
    // `performance.now`. A page has no CPU-time clock, so the two CPU-time
    // clocks read the monotonic one. `precision` is a BigInt and is ignored.
    clock_time_get(id, precision, timePtr) {
      let ns;
      if (id === CLOCK_REALTIME) {
        ns = BigInt(Date.now()) * 1000000n;
      } else if (id === CLOCK_MONOTONIC || id === CLOCK_PROCESS_CPUTIME || id === CLOCK_THREAD_CPUTIME) {
        ns = BigInt(Math.round(performance.now() * 1e6));
      } else {
        return EINVAL;
      }
      view().setBigUint64(timePtr, ns, true);
      return SUCCESS;
    },
    // A millisecond for `Date.now`, and a microsecond for `performance.now`,
    // which a browser may coarsen further.
    clock_res_get(id, resolutionPtr) {
      let ns;
      if (id === CLOCK_REALTIME) {
        ns = 1000000n;
      } else if (id === CLOCK_MONOTONIC || id === CLOCK_PROCESS_CPUTIME || id === CLOCK_THREAD_CPUTIME) {
        ns = 1000n;
      } else {
        return EINVAL;
      }
      view().setBigUint64(resolutionPtr, ns, true);
      return SUCCESS;
    },

    // Descriptors 0, 1 and 2 report a character device that cannot seek,
    // which is what wasi-libc's `isatty` tests for, so standard output is
    // line-buffered as it is on a terminal. The rights are the ones `fcntl`
    // reads the access mode from. Any error here would also start the
    // runtime, with standard output fully buffered instead.
    fd_fdstat_get(fd, statPtr) {
      if (!isStdio(fd)) return EBADF;
      const v = view();
      v.setUint8(statPtr, FILETYPE_CHARACTER_DEVICE);
      v.setUint16(statPtr + 2, 0, true);
      v.setBigUint64(statPtr + 8, fd === 0 ? RIGHT_FD_READ : RIGHT_FD_WRITE, true);
      v.setBigUint64(statPtr + 16, 0n, true);
      return SUCCESS;
    },
    fd_fdstat_set_flags(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },
    fd_filestat_get(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },

    // No descriptor is a preopened directory. wasi-libc's constructor, run by
    // `_initialize`, asks from descriptor 3 upwards and stops at EBADF. Any
    // other error, ENOSYS included, makes it exit with status 71 before
    // `wattle_web_init` is called. With no preopens, wasi-libc fails every
    // relative path itself, so the `path_*` calls below are not reached.
    fd_prestat_get() {
      return EBADF;
    },
    fd_prestat_dir_name() {
      return EBADF;
    },

    // Standard input is what `setStdin` last gave, and then end of file. A
    // read fills the buffers in order and reports how many bytes it wrote.
    fd_read(fd, iovsPtr, iovsLen, readPtr) {
      if (fd !== 0) return EBADF;
      const v = view();
      let read = 0;
      for (let i = 0; i < iovsLen && stdinOffset < stdin.length; i++) {
        const ptr = v.getUint32(iovsPtr + i * 8, true);
        const len = v.getUint32(iovsPtr + i * 8 + 4, true);
        const count = Math.min(len, stdin.length - stdinOffset);
        bytes(ptr, count).set(stdin.subarray(stdinOffset, stdinOffset + count));
        stdinOffset += count;
        read += count;
      }
      v.setUint32(readPtr, read, true);
      return SUCCESS;
    },

    // Descriptors 1 and 2 append to the output `take` returns. Nothing
    // touches the page here: a stack trace can run to hundreds of thousands
    // of writes.
    fd_write(fd, iovsPtr, iovsLen, writtenPtr) {
      if (fd !== 1 && fd !== 2) return EBADF;
      const v = view();
      let written = 0;
      for (let i = 0; i < iovsLen; i++) {
        const ptr = v.getUint32(iovsPtr + i * 8, true);
        const len = v.getUint32(iovsPtr + i * 8 + 4, true);
        chunks[fd].push(decoders[fd].decode(bytes(ptr, len), { stream: true }));
        written += len;
      }
      v.setUint32(writtenPtr, written, true);
      return SUCCESS;
    },

    // A terminal cannot seek, and the rest do not apply to one.
    fd_seek(fd) {
      return isStdio(fd) ? ESPIPE : EBADF;
    },
    fd_pread(fd) {
      return isStdio(fd) ? ESPIPE : EBADF;
    },
    fd_pwrite(fd) {
      return isStdio(fd) ? ESPIPE : EBADF;
    },
    fd_filestat_set_size(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },
    fd_filestat_set_times(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },
    fd_sync(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },
    fd_readdir(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },
    fd_close(fd) {
      return isStdio(fd) ? ENOSYS : EBADF;
    },

    // There is no file system. wasi-libc does not reach these (see
    // `fd_prestat_get`); they are here because the binary imports them.
    path_create_directory: () => ENOSYS,
    path_filestat_get: () => ENOSYS,
    path_filestat_set_times: () => ENOSYS,
    path_link: () => ENOSYS,
    path_open: () => ENOSYS,
    path_readlink: () => ENOSYS,
    path_remove_directory: () => ENOSYS,
    path_rename: () => ENOSYS,
    path_symlink: () => ENOSYS,
    path_unlink_file: () => ENOSYS,

    // Returns at once with every subscription reported as having fired, so
    // `os/sleep` does not wait: a page cannot block. A subscription is 48
    // bytes with its userdata at 0 and its type at 8; an event is 32 bytes
    // with the userdata at 0, the error at 8 and the type at 10.
    poll_oneoff(inPtr, outPtr, count, eventsPtr) {
      if (count === 0) return EINVAL;
      const v = view();
      for (let i = 0; i < count; i++) {
        const sub = inPtr + i * 48;
        const event = outPtr + i * 32;
        bytes(event, 32).fill(0);
        v.setBigUint64(event, v.getBigUint64(sub, true), true);
        v.setUint16(event + 8, SUCCESS, true);
        v.setUint8(event + 10, v.getUint8(sub + 8));
      }
      v.setUint32(eventsPtr, count, true);
      return SUCCESS;
    },

    // Unwinds the call into the instance, which cannot continue.
    proc_exit(code) {
      throw new WasiExit(code);
    },

    random_get(bufPtr, len) {
      for (let at = 0; at < len; at += RANDOM_CHUNK) {
        crypto.getRandomValues(bytes(bufPtr + at, Math.min(RANDOM_CHUNK, len - at)));
      }
      return SUCCESS;
    },
  };

  return {
    imports: { wasi_snapshot_preview1: wasi },
    bind(instanceMemory) {
      memory = instanceMemory;
    },
    setStdin(data) {
      stdin = typeof data === "string" ? new TextEncoder().encode(data) : data;
      stdinOffset = 0;
    },
    take() {
      const out = { stdout: "", stderr: "" };
      for (const [fd, key] of [[1, "stdout"], [2, "stderr"]]) {
        chunks[fd].push(decoders[fd].decode());
        out[key] = chunks[fd].join("");
        chunks[fd] = [];
      }
      return out;
    },
  };
}

// Instantiates `module`, a compiled `wattle-web.wasm`, and starts the runtime.
//
// A build with a compiler returns an object whose `eval(source)` runs one
// submission and returns `{ status, stdout, stderr, error }`: `status` is 0, or
// 1 when the submission failed, and `error` is null, or the exception that
// stopped the instance (a `WebAssembly.RuntimeError` for a trap, a `WasiExit`
// for `os/exit`). The `status` of a call that ended in `os/exit` is the exit
// code, so 0 for a clean exit, and `error` is the `WasiExit`. After an `error`, the instance is unusable and `eval`
// throws; the host starts a new one.
//
// A build made with `-Dwasm-image` has no compiler and returns an object whose
// `runImage(bytes, { args, stdin })` takes the bytes of an image, as a
// `Uint8Array`, loads it and calls its `main`. `args` is an array of strings
// that `main` receives as they are, so the first is the program's name by the
// convention of `wattle -i`. `stdin` is a string or a `Uint8Array` that
// standard input reads from the start. Both default to nothing. The result is
// the same as `eval`'s. The instance stays usable after a call that returns
// status 1, and the host calls `runImage` again on it. After an `error` it does
// not, as for `eval`.
//
// Throws when `wattle_web_init` fails.
export async function start(module) {
  const wasi = createWasi();
  const instance = await WebAssembly.instantiate(module, wasi.imports);
  const exports = instance.exports;
  wasi.bind(exports.memory);
  exports._initialize();
  const initStatus = exports.wattle_web_init();
  const initOutput = wasi.take();
  if (initStatus !== 0) {
    throw new Error(`wattle_web_init returned ${initStatus}: ${initOutput.stderr}`);
  }

  const encoder = new TextEncoder();
  let stopped = null;

  // Copies `bytes` into wasm memory, calls the export `name` on them and
  // collects what it wrote.
  function submit(name, ...buffers) {
    if (stopped) throw new Error("the instance has stopped", { cause: stopped });
    const pointers = buffers.map((buffer) => {
      const ptr = exports.wattle_web_alloc(buffer.length);
      if (ptr === 0 && buffer.length > 0) throw new Error(`could not allocate ${buffer.length} bytes`);
      new Uint8Array(exports.memory.buffer, ptr, buffer.length).set(buffer);
      return ptr;
    });
    let status = 1;
    try {
      status = exports[name](...buffers.flatMap((buffer, i) => [pointers[i], buffer.length]));
      buffers.forEach((buffer, i) => exports.wattle_web_free(pointers[i], buffer.length));
    } catch (error) {
      stopped = error;
      // An exit reports its own status, so `os/exit 0` is a success. Any other
      // exception, a trap included, is status 1.
      if (error instanceof WasiExit) status = error.code;
    }
    return { status, ...wasi.take(), error: stopped };
  }

  if (exports.wattle_web_run_image) {
    return {
      runImage(bytes, { args = [], stdin = "" } = {}) {
        wasi.setStdin(stdin);
        // Each argument is followed by a NUL, which is how the runtime splits them.
        const joined = encoder.encode(args.map((arg) => `${arg}\0`).join(""));
        return submit("wattle_web_run_image", bytes, joined);
      },
    };
  }
  return { eval: (source) => submit("wattle_web_eval", encoder.encode(source)) };
}
