(use ../deps/testament)

(defn lines-to-stream
  [lines]
  (def [r w] (os/pipe))
  (each line lines (:write w (string line "\n")))
  (:close w)
  r)

(defn shell-capture
  [cmd test_stdin]
  (let [x (os/spawn cmd : {:in test_stdin :out :pipe :err :pipe})
        o (:read (x :out) :all)
        e (:read (x :err) :all)]
    (:wait x)
    [(get x :return-code)
     (if o (string/split "\n" o))
     (if e (string/split "\n" e))]))

(deftest cli-no-args
  (def [exit_code test_out test_err] 
    (shell-capture ["./predoc"] stdin))
  (is (= 1 exit_code))
  (is (= nil test_out))
  (is (deep=
    @["predoc: path is required"
      "Try 'predoc --help' for more information."
      ""]
    test_err)))

(deftest cli-bad-option
  (def [exit_code test_out test_err] 
    (shell-capture ["./predoc" "--bad-option"] stdin))
  (is (= 1 exit_code))
  (is (= nil test_out))
  (is (deep=
    @["predoc: unrecognized option '--bad-option'"
      "Try 'predoc --help' for more information."
      ""]
    test_err)))

(deftest cli-good-input
  (def [exit_code test_out test_err] 
    (shell-capture
      ["./predoc" "--output" "-" "--name" "Testing" "-"]
      (lines-to-stream
        @["NAME"
          "===="
          ""
          "**predoc** - converter from Predoc to mdoc"])))
  (is (= 0 exit_code))
  (is (deep=
    @["."
      ".Sh NAME"
      ".Ic predoc"
      ".Nd converter from Predoc to mdoc"
      ""
      ""]
    (array/slice test_out 3))) # strip first 3 lines containing timestamp
  (is (deep= @["stty: 'standard input': Not a tty" ""] test_err)))

(deftest cli-bad-input
  (def [exit_code test_out test_err] 
    (shell-capture
      ["./predoc" "--output" "-" "--name" "Testing" "-"]
      (lines-to-stream
        @["---"
          "Title: Testing(1)"
          "---"])))
  (is (= 1 exit_code))
  (is (= nil test_out))
  (is (deep=
    @["stty: 'standard input': Not a tty"
      "error: could not parse date in frontmatter"
      "  in parse-date [lib/mdoc.janet] on line 133, column 6"
      "  in render-prologue [lib/mdoc.janet] (tail call) on line 430, column 13"
      "  in render-doc [lib/mdoc.janet] (tail call) on line 567, column 5"
      "  in run [lib/cli.janet] (tail call) on line 116, column 21"
      ""]
    test_err)))

(run-tests!)
