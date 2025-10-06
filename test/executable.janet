(use ../deps/testament)

(defn str-to-stream
  [str]
  (def [r w] (os/pipe))
  (:write w str)
  (:close w)
  r)

(defn shell-capture
  [cmd test_stdin]
  (let [x (os/spawn cmd : {:in test_stdin :out :pipe :err :pipe})
        o (:read (x :out) :all)
        e (:read (x :err) :all)]
    (:wait x)
    [(get x :return-code) o e]))

(deftest cli-no-args
  (is (deep=
    [1 nil @"predoc: path is required\nTry 'predoc --help' for more information.\n"]
    (shell-capture ["./predoc"] stdin))))

(deftest cli-bad-option
  (is (deep=
    [1 nil @"predoc: unrecognized option '--bad-option'\nTry 'predoc --help' for more information.\n"]
    (shell-capture ["./predoc" "--bad-option"] stdin))))

(deftest cli-good-input
  (def [exit_code test_out test_err] 
    (shell-capture ["./predoc" "--output" "-" "--name" "Testing" "-"]
                   (str-to-stream "NAME\n====\n\n**predoc** - converter from Predoc to mdoc\n")))
  (is (= 0 exit_code))
  (is (deep=
"
.\n
.Sh NAME\n
.Ic predoc\n
.Nd converter from Predoc to mdoc\n
\n"
    (string/slice test_out -58))) # Strip off beginning time stamp section
  (is (deep= @"stty: 'standard input': Not a tty\n" test_err)))

(deftest cli-bad-input
  (def [exit_code test_out test_err] 
    (shell-capture ["./predoc" "--output" "-" "--name" "Testing" "-"]
                   (str-to-stream "---\nTitle: Testing(1)\n---")))
  (is (= 1 exit_code))
  (is (= nil test_out))
  (is (deep=
@"stty: 'standard input': Not a tty\n
error: could not parse date in frontmatter\n
  in parse-date [lib/mdoc.janet] on line 133, column 6\n
  in render-prologue [lib/mdoc.janet] (tail call) on line 430, column 13\n
  in render-doc [lib/mdoc.janet] (tail call) on line 567, column 5\n
  in run [lib/cli.janet] (tail call) on line 116, column 21\n"
    test_err)))

(run-tests!)
