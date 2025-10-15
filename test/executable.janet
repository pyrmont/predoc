(use ../deps/testament)

(defn- rmrf
  [path]
  (def sep (get {:windows "\\" :cygwin "\\" :mingw "\\"} (os/which) "/"))
  (case (os/lstat path :mode)
    :directory (do
                 (each subpath (os/dir path)
                   (rmrf (string path sep subpath)))
                 (os/rmdir path))
    nil nil # do nothing if file does not exist
    (os/rm path)))

(defn- devnull
  []
  (os/open (if (= :windows (os/which)) "NUL" "/dev/null") :rw))

(defn- lines-to-stream [lines]
  (def [r w] (os/pipe))
  (:write w lines)
  (:close w)
  r)

(defn- shell-capture [cmd test-stdin]
  (let [x (os/spawn cmd : {:in test-stdin :out :pipe :err :pipe})
        o (:read (x :out) :all)
        e (:read (x :err) :all)]
    (:wait x)
    [(get x :return-code) o e]))

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
  (def output
    ``
    error: could not parse date in frontmatter
      in parse-date [lib/mdoc.janet] on line 133, column 6
      in render-prologue [lib/mdoc.janet] (tail call) on line 430, column 13
      in render-doc [lib/mdoc.janet] (tail call) on line 567, column 5
      in run [lib/cli.janet] (tail call) on line 116, column 21
    ``)
  (def [exit-code test-out test-err]
    (shell-capture ["./tmp/predoc" "--name" "foobar" "--output" "-" "-"]
                   (lines-to-stream input)))
  (is (== 1 exit-code))
  (is (== nil test-out))
  (is (== (string output "\n") test-err)))

(defer (rmrf "tmp")
  (os/mkdir "tmp")
  (with [null (devnull)]
    (print "building ./tmp/predoc...")
    (os/execute ["jeep" "quickbin" "lib/cli.janet" "tmp/predoc"] :px {:out null :err null}))
  (run-tests!))
