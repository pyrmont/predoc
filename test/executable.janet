(use ../deps/testament)

## Helpers

(defn- lines-to-stream [lines]
  (def [r w] (os/pipe))
  (:write w lines)
  (:close w)
  r)

(defn- rmrf
  [path]
  (case (os/lstat path :mode)
    :directory (do
                 (each subpath (os/dir path)
                   (rmrf (string path "/" subpath)))
                 (os/rmdir path))
    nil nil # do nothing if file does not exist
    (os/rm path)))

(defn- shell-capture [cmd test-stdin]
  (let [x (os/spawn cmd : {:in test-stdin :out :pipe :err :pipe})
        o (:read (x :out) :all)
        e (:read (x :err) :all)]
    (:wait x)
    [(get x :return-code) o e]))

(def- test-version "DEVEL-test")

## Tests

(deftest cli-no-args
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc"] stdin))
  (def msg
    ``
    predoc: path is required
    Try 'predoc --help' for more information.
    ``)
  (is (== 1 exit-code))
  (is (== nil test-out))
  (is (== (string msg "\n") test-err)))

(deftest cli-bad-option
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "--bad-option"] stdin))
  (def msg
    ``
    predoc: unrecognized option '--bad-option'
    Try 'predoc --help' for more information.
    ``)
  (is (== 1 exit-code))
  (is (== nil test-out))
  (is (== (string msg "\n") test-err)))

(deftest cli-short-version
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "-v"] stdin))
  (is (== 0 exit-code))
  (is (== (string test-version "\n") test-out))
  (is (== nil test-err)))

(deftest cli-long-version
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "--version"] stdin))
  (is (== 0 exit-code))
  (is (== (string test-version "\n") test-out))
  (is (== nil test-err)))

(deftest cli-good-input
  (def input
    ``
    NAME
    ===

    **foobar** - putting the bar in your foo
    ``)
  (def output
    ``
    .
    .Sh NAME
    .Nm foobar
    .Nd putting the bar in your foo
    ``)
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "--no-ad" "--name" "foobar" "--output" "-" "-"]
                   (lines-to-stream input)))
  (is (== 0 exit-code))
  (is (== (string output "\n\n") test-out))
  (is (== nil test-err)))

(deftest cli-bad-input
  (def input
    ``
    ---
    Title: foobar(1)
    ---
    ``)
  (def output "error: could not parse date in frontmatter\n")
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "--name" "foobar" "--output" "-" "-"]
                   (lines-to-stream input)))
  (is (== 1 exit-code))
  (is (== nil test-out))
  (is (string/has-prefix? output test-err)))

(defer (rmrf "tmp")
  (os/mkdir "tmp")
  (print "building ./tmp/predoc...")
  (def info (-> (slurp "info.jdn") parse))
  (def executable
    (merge (get-in info [:artifacts :executables 0]) {:name "predoc-test"}))
  (def artifacts (merge (get info :artifacts) {:executables [executable]}))
  (def test-info (merge info {:artifacts artifacts}))
  (def bundle (require "../bundle"))
  (def previous-version (os/getenv "PREDOC_BUILD_VERSION"))
  (defer (os/setenv "PREDOC_BUILD_VERSION" previous-version)
    (os/setenv "PREDOC_BUILD_VERSION" test-version)
    (with-dyns [:out @"" :err @""]
      (def build (module/value bundle 'build))
      (build {:info test-info})))
  (os/rename "_build/predoc-test" "tmp/predoc")
  (os/chmod "tmp/predoc" 8r755)
  (run-tests!))
