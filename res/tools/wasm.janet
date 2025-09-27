(import ../../lib/util)

(def- s util/sep)

(defn build-image
  [bdir]
  (def env (require "../../init"))
  (spit (string bdir s "predoc.jimage") (make-image env)))

(defn build
  ```
  Build wasm module.  Emsdk must be installed and runnable in the current
  shell -- on linux, for example, `source emsdk_env.sh` in the emsdk directory.
  ```
  [root]
  (def pwd (string (os/cwd) "/"))
  (def bdir (string root s "res" s "wasm"))
  (def pdir (string root s "pages"))
  (build-image bdir)
  (os/cd bdir)
  (print "Building WebAssembly module...")
  (def result
    (try
      (os/execute
        ["container" "run" "--rm"
         "-v" (string (os/cwd) ":/src")
         "-w" "/src"
         "emscripten/emsdk:4.0.14-arm64"
         "emcc"
         "-O3" # replace with "-O0" during debugging
         "-o" "janet.js"
         "janet.c"
         "dingus.c"
         "-Ijanet"
         "--embed-file" "predoc.jimage"
         # Module/exports
         "-s" "EXPORT_ES6=1"
         "-s" "EXPORT_NAME=init"
         "-s" "EXPORTED_FUNCTIONS=['_run_janet']"
         "-s" "EXPORTED_RUNTIME_METHODS=['ccall']"
         # Memory & stack
         "-s" "STACK_SIZE=1048576" # 1 MiB stack
         # "-s" "INITIAL_MEMORY=67108864" # 64 MiB initial heap
         "-s" "ALLOW_MEMORY_GROWTH=1"
         # Table growth for dynamic callbacks
         "-s" "ALLOW_TABLE_GROWTH=1"
         # Diagnostics for finding the real source
         # "-gsource-map"
         # "-s" "EXCEPTION_STACK_TRACES=1"
         # "-fexceptions"
         # "-s" "DISABLE_EXCEPTION_CATCHING=0"
         # "-s" "EXCEPTION_STACK_TRACES=1"
         # "-s" "ASSERTIONS=2"
         # "-s" "SAFE_HEAP=1"
         # "-s" "STACK_OVERFLOW_CHECK=2"
         # "-Wframe-larger-than=16384" # warn if a function uses >16 KiB stack
         # comment the below out during debugging
         "-s" "AGGRESSIVE_VARIABLE_ELIMINATION=1"
         ]
        :p)
      ([err] (eprint "Can't run emcc. Ensure emcc is installed and in your path, and emsdk_env.sh has been sourced into your current environment"))))
  (unless (zero? result)
    (eprint "Error running emcc. Build failed.")
    (os/exit result))
  (def wasm-src (string bdir s "janet.wasm"))
  (def wasm-dest (string pdir s "janet.wasm"))
  (print "Moving " (string/replace pwd "" wasm-src) " to " (string/replace pwd "" wasm-dest))
  (os/rename wasm-src wasm-dest)
  (def js-src (string bdir s "janet.js"))
  (def js-dest (string pdir s "janet.js"))
  (print "Moving " (string/replace pwd "" js-src) " to " (string/replace pwd "" js-dest))
  (os/rename js-src js-dest))

(defn main [&]
  (def threeup (comp util/parent util/parent util/parent))
  (def bundle-root (-> (dyn :current-file) util/abspath threeup))
  (build bundle-root))
