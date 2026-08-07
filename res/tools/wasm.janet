(import ../../lib/util)

(def- s util/sep)
(def- default-cmd "docker")
(def- default-emsdk-tag "6.0.4")
(def- janet-repo "https://github.com/janet-lang/janet.git")

(defn- path
  [& parts]
  (string/join parts s))

(defn- directory?
  [name]
  (= :directory (os/stat name :mode)))

(defn- file?
  [name]
  (= :file (os/stat name :mode)))

(defn- ensure-directory
  [name]
  (unless (directory? name)
    (os/mkdir name)))

(defn- spit-if-changed
  [name contents]
  (def [exists? current] (protect (slurp name)))
  (unless (and exists? (= contents (string current)))
    (spit name contents)))

(defn- valid-version?
  [version]
  (peg/match '(* :d+ "." :d+ "." :d+ -1) version))

(defn- run
  [args message]
  (def [ok? result] (protect (os/execute args :px)))
  (unless ok?
    (error (string message ": " result)))
  (unless (zero? result)
    (error (string message " (exit status " result ")"))))

(defn- run-output
  [args message]
  (def [reader writer] (os/pipe))
  (def [ok? result] (protect (os/execute args :px {:out writer})))
  (:close writer)
  (def output (-> (ev/read reader :all) string/trim))
  (unless ok?
    (error (string message ": " result)))
  (unless (zero? result)
    (error (string message " (exit status " result ")")))
  output)

(defn- hash-file
  [name]
  (run-output ["git" "hash-object" name] (string "cannot hash " name)))

(defn- emcc-args
  [output]
  ["emcc"
   "-Oz" # replace with "-O0" during debugging
   "-flto"
   "-o" output
   "janet.c"
   "dingus.c"
   "-I."
   # Module/exports
   "-s" "EXPORT_ES6=1"
   "-s" "EXPORT_NAME=init"
   "-s" "EXPORTED_FUNCTIONS=['_run_janet']"
   "-s" "EXPORTED_RUNTIME_METHODS=['ccall']"
   "-s" "ENVIRONMENT=web,node"
   "-s" "FILESYSTEM=0"
   # Memory & stack
   "-s" "STACK_SIZE=1048576" # 1 MiB stack
   # "-s" "INITIAL_MEMORY=67108864" # 64 MiB initial heap
   "-s" "ALLOW_MEMORY_GROWTH=1"
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
   ])

(defn- image-header
  [image]
  (def output (buffer/new 0))
  (buffer/push-string output
                      "#ifndef PREDOC_IMAGE_H\n#define PREDOC_IMAGE_H\n\n")
  (buffer/push-string output "static const uint8_t predoc_image[] = {\n")
  (eachp [i byte] image
    (when (zero? (% i 12))
      (buffer/push-string output "    "))
    (buffer/push-string output (string/format "0x%02x," byte))
    (if (= 11 (% i 12))
      (buffer/push-byte output 10)
      (buffer/push-byte output 32)))
  (unless (zero? (% (length image) 12))
    (buffer/push-byte output 10))
  (buffer/push-string output
                      "};\n\nstatic const size_t predoc_image_len = "
                      "sizeof(predoc_image);\n\n#endif\n")
  output)

(defn- fingerprint
  [bdir emsdk-tag]
  (def contents (buffer/new 0))
  (each [label value] [["emsdk" emsdk-tag]
                       ["emcc" (string/join (emcc-args "OUTPUT.js") "\0")]]
    (buffer/push-string contents label)
    (buffer/push-byte contents 0)
    (buffer/push-string contents value)
    (buffer/push-byte contents 0))
  (each name ["janet.c" "janet.h" "dingus.c" "predoc-image.h"]
    (buffer/push-string contents name)
    (buffer/push-byte contents 0)
    (buffer/push-string contents (slurp (path bdir name)))
    (buffer/push-byte contents 0))
  (def manifest (path bdir "fingerprint.input"))
  (spit manifest contents)
  (-> (hash-file manifest) (string/slice 0 12)))

(defn- prepare-janet
  [cmd root version emsdk-tag]
  (unless (valid-version? version)
    (error (string "invalid Janet version '" version
                   "' (expected a version such as 1.39.1)")))
  (def build-root (path root "_build" "wasm"))
  (ensure-directory (path root "_build"))
  (ensure-directory build-root)
  (def source-dir (path build-root (string "janet-" version)))
  (unless (directory? source-dir)
    (print "Fetching Janet " version "...")
    (run ["git" "clone" "--depth" "1" "--branch" (string "v" version)
          "--single-branch" janet-repo source-dir]
         (string "cannot fetch Janet " version)))
  (def upstream-config (path source-dir "src" "conf" "janetconf.h"))
  (def saved-config (path source-dir "janetconf.upstream.h"))
  (unless (file? saved-config)
    (spit saved-config (slurp upstream-config)))
  (def upstream-header (path source-dir "src" "include" "janet.h"))
  (def expected-version (string `#define JANET_VERSION "` version `"`))
  (unless (string/find expected-version (slurp saved-config))
    (error (string source-dir " is not the Janet " version " source tree")))
  (def predoc-config (path root "res" "wasm" "janetconf.h"))
  (def config (string (slurp saved-config) "\n" (slurp predoc-config)))
  (def emsdk-image (string "emscripten/emsdk:" emsdk-tag))
  (def container-platform
    (run-output [cmd "run" "--rm" emsdk-image "uname" "-m"]
                (string "cannot determine the platform of " emsdk-image)))
  (def image-builder (path source-dir "build" "janet-image"))
  (def image-builder-key-file (path source-dir "janet-image.key"))
  (def image-builder-key (string emsdk-tag ":" container-platform))
  (def [has-key? cached-key] (protect (slurp image-builder-key-file)))
  (unless (and (file? image-builder)
               has-key?
               (= image-builder-key (string/trim cached-key)))
    (spit-if-changed upstream-config (string (slurp saved-config)))
    (print "Building Janet " version " image compiler for "
           container-platform "...")
    (run [cmd "run" "--rm"
          "-v" (string source-dir ":/src")
          "-w" "/src"
          emsdk-image
          "make"
          "clean"]
         (string "cannot clean the Janet " version " build"))
    (run [cmd "run" "--rm"
          "-v" (string source-dir ":/src")
          "-w" "/src"
          emsdk-image
          "make"
          "build/janet"]
         (string "cannot build Janet " version " image compiler"))
    (when (file? image-builder)
      (os/rm image-builder))
    (os/rename (path source-dir "build" "janet") image-builder)
    (spit image-builder-key-file image-builder-key))
  (def stage-dir (path build-root (string "predoc-" version)))
  (ensure-directory stage-dir)
  (def image-path (string "_build/wasm/predoc-" version "/predoc.jimage"))
  (print "Building Predoc image with Janet " version "...")
  (run [cmd "run" "--rm"
        "-v" (string root ":/predoc")
        "-v" (string source-dir ":/janet")
        "-w" "/predoc"
        emsdk-image
        "/janet/build/janet-image"
        "-e" (string `(def env (require "./init")) (spit "`
                     image-path `" (make-image env))`)]
       (string "cannot build the Predoc image with Janet " version))
  (spit (path stage-dir "predoc-image.h")
        (image-header (slurp (path stage-dir "predoc.jimage"))))
  (spit-if-changed upstream-config config)
  (print "Generating Janet " version " amalgamation...")
  (run [cmd "run" "--rm"
        "-v" (string source-dir ":/src")
        "-w" "/src"
        emsdk-image
        "make"
        `JANET_BUILD=\"predoc\"`
        "build/c/janet.c"]
       (string "cannot generate the Janet " version " amalgamation"))
  (spit (path stage-dir "janet.c")
        (slurp (path source-dir "build" "c" "janet.c")))
  (def header (slurp upstream-header))
  (def config-include `#include "janetconf.h"`)
  (unless (string/find config-include header)
    (error (string upstream-header " does not include janetconf.h")))
  (spit (path stage-dir "janet.h")
        (string config "\n"
                (string/replace config-include "" header)))
  (spit (path stage-dir "dingus.c")
        (slurp (path root "res" "wasm" "dingus.c")))
  stage-dir)

(defn- between-one
  [contents prefix suffix description]
  (def prefix-pos (string/find prefix contents))
  (when (nil? prefix-pos)
    (error (string description " is missing")))
  (def start (+ prefix-pos (length prefix)))
  (def end (string/find suffix contents start))
  (when (nil? end)
    (error (string description " is malformed")))
  (when (string/find prefix contents start)
    (error (string description " occurs more than once")))
  (string/slice contents start end))

(defn- replace-one
  [contents old new description]
  (def first-pos (string/find old contents))
  (when (nil? first-pos)
    (error (string description " is missing")))
  (when (string/find old contents (+ first-pos (length old)))
    (error (string description " occurs more than once")))
  (string/replace old new contents))

(defn- prepare-ref-updates
  [name fingerprint root]
  (def pages-dir (path root "pages"))
  (def html-file (path pages-dir "index.html"))
  (def html (slurp html-file))
  (def html-prefix `src="dingus.js?`)
  (def old-cache-key
    (between-one html html-prefix `"` "dingus.js cache reference"))
  (def old-html-ref (string html-prefix old-cache-key `"`))
  (def new-html-ref (string html-prefix fingerprint `"`))
  (def updated-html
    (replace-one html old-html-ref new-html-ref "dingus.js cache reference"))
  (def js-file (path pages-dir "dingus.js"))
  (def js (slurp js-file))
  (def import-prefix `import init from "./`)
  (def old-js-name
    (between-one js import-prefix `";` "Janet module import"))
  (unless (and (string/has-prefix? "janet." old-js-name)
               (string/has-suffix? ".js" old-js-name))
    (error (string "unexpected Janet module import '" old-js-name "'")))
  (def old-import (string import-prefix old-js-name `";`))
  (def new-import (string import-prefix name ".js" `";`))
  (def updated-js
    (replace-one js old-import new-import "Janet module import"))
  {:html-file html-file
   :html updated-html
   :js-file js-file
   :js updated-js})

(defn- publish-asset
  [source destination]
  (if (file? destination)
    (unless (deep= (slurp source) (slurp destination))
      (error (string destination " exists with different content")))
    (os/rename source destination)))

(defn- old-asset?
  [entry current]
  (and (not= entry current)
       (string/has-prefix? "janet." entry)
       (or (string/has-suffix? ".js" entry)
           (string/has-suffix? ".wasm" entry))))

(defn- remove-old-assets
  [pages-dir current-js current-wasm]
  (each entry (os/dir pages-dir)
    (when (and (old-asset? entry current-js)
               (old-asset? entry current-wasm))
      (def old-file (path pages-dir entry))
      (print "Removing " old-file)
      (os/rm old-file))))

(defn- publish
  [name fingerprint bdir root]
  (def updates (prepare-ref-updates name fingerprint root))
  (def pages-dir (path root "pages"))
  (def current-js (string name ".js"))
  (def current-wasm (string name ".wasm"))
  (def temp-js (path pages-dir (string ".dingus." fingerprint ".tmp")))
  (def temp-html (path pages-dir (string ".index." fingerprint ".tmp")))
  (spit temp-js (updates :js))
  (spit temp-html (updates :html))
  (publish-asset (path bdir current-js) (path pages-dir current-js))
  (publish-asset (path bdir current-wasm) (path pages-dir current-wasm))
  (os/rename temp-js (updates :js-file))
  (os/rename temp-html (updates :html-file))
  (remove-old-assets pages-dir current-js current-wasm))

(defn build
  [cmd root janet-version emsdk-tag]
  (def bdir (prepare-janet cmd root janet-version emsdk-tag))
  (def digest (fingerprint bdir emsdk-tag))
  (def name (string "janet." digest))
  (os/cd bdir)
  (print "Building WebAssembly module...")
  (run (array/concat @[cmd "run" "--rm"
                       "-v" (string (os/cwd) ":/src")
                       "-w" "/src"
                       (string "emscripten/emsdk:" emsdk-tag)]
                     (emcc-args (string name ".js")))
       (string "cannot run emcc using " cmd))
  (publish name digest bdir root))

(defn main
  ```
  Build the WebAssembly module using the requested Janet release. This script
  uses a Docker container to generate Janet's amalgamation and run Emscripten.
  By default it will try to run the container using `docker`. A different
  command (e.g. `podman`) and Emscripten image tag can be specified after the
  Janet version (e.g. `janet res/tools/wasm.janet 1.41.2 podman 6.0.4`).
  ```
  [command janet-version & args]
  (def cmd (get args 0 default-cmd))
  (def emsdk-tag (get args 1 default-emsdk-tag))
  (def threeup (comp util/parent util/parent util/parent))
  (def bundle-root (-> (dyn :current-file) util/abspath threeup))
  (build cmd bundle-root janet-version emsdk-tag))
