/* Predoc's Janet configuration for the WebAssembly build. */

#define JANET_BUILD "predoc"

/* These settings affect linking. */
#define JANET_SINGLE_THREADED
#define JANET_NO_DYNAMIC_MODULES

/* Remove facilities that the browser-based converter does not use. */
#define JANET_NO_DOCSTRINGS
#define JANET_NO_SOURCEMAPS
#define JANET_REDUCED_OS
#define JANET_NO_PROCESSES
#define JANET_NO_ASSEMBLER
#define JANET_NO_NET
#define JANET_NO_INT_TYPES
#define JANET_NO_EV
#define JANET_NO_FILEWATCH
#define JANET_NO_REALPATH
#define JANET_NO_SYMLINKS
#define JANET_NO_UMASK
#define JANET_NO_THREADS
#define JANET_NO_FFI
#define JANET_NO_FFI_JIT
