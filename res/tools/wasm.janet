(import ../../lib/util)

(def- s util/sep)
(def- default-cmd "docker")

(defn build-image
  [bdir]
  (def env (require "../../init"))
  (spit (string bdir s "predoc.jimage") (make-image env)))

(defn update-refs
  [name root]
  (def html-file (string root s "pages" s "index.html"))
  (def html (slurp html-file))
  (def dingus-pos (+ 10 (string/find "dingus.js?" html)))
  (spit html-file (string (string/slice html 0 dingus-pos)
                          (os/strftime "%Y%m%d%H%M")
                          (string/slice html (+ 12 dingus-pos))))
  (def js-file (string root s "pages" s "dingus.js"))
  (def js (slurp js-file))
  (def janet-pos (+ 2 (string/find "./janet." js)))
  (spit js-file (string (string/slice js 0 janet-pos)
                        (string name ".js")
                        (string/slice js (+ 17 janet-pos)))))

(defn sha
  []
  (def [r w] (os/pipe))
  (def [ok? res] (protect (os/execute ["git" "rev-parse" "HEAD"] :px {:out w})))
  (:close w)
  (if ok?
    (-> (ev/read r :all) (string/trim) (string/slice 0 8))
    (error res)))

(defn build
  [cmd root emsdk-tag]
  (def name (string "janet." (sha)))
  (def pwd (string (os/cwd) "/"))
  (def bdir (string root s "res" s "wasm"))
  (def pdir (string root s "pages"))
  (build-image bdir)
  (os/cd bdir)
  (print "Building WebAssembly module...")
  (def result
    (try
      (os/execute
        [cmd "run" "--rm"
         "-v" (string (os/cwd) ":/src")
         "-w" "/src"
         (string "emscripten/emsdk:" emsdk-tag)
         "emcc"
         "-O3" # replace with "-O0" during debugging
         "-o" (string name ".js")
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
        :px)
      ([err]
       (eprint "Cannot run emcc using the Docker container."))))
  (unless (zero? result)
    (os/exit result))
  (def wasm-src (string bdir s name ".wasm"))
  (def wasm-dest (string pdir s name ".wasm"))
  (print "Moving " (string/replace pwd "" wasm-src) " to " (string/replace pwd "" wasm-dest))
  (os/rename wasm-src wasm-dest)
  (def js-src (string bdir s name ".js"))
  (def js-dest (string pdir s name ".js"))
  (print "Moving " (string/replace pwd "" js-src) " to " (string/replace pwd "" js-dest))
  (os/rename js-src js-dest)
  (update-refs name root))

(defn main
  ```
  This script uses a Docker container to run Emscripten. By default it will try
  to run the container using `docker`. A different command (e.g. `podman`) can
  be specified by calling the script with the name of the command as the second
  argument (e.g. `janet res/tools/wasm.janet podman`).
  ```
  [& args]
  (def cmd (get args 1 default-cmd))
  (def emsdk-tag (get args 2 "latest"))
  (def threeup (comp util/parent util/parent util/parent))
  (def bundle-root (-> (dyn :current-file) util/abspath threeup))
  (build cmd bundle-root emsdk-tag))
