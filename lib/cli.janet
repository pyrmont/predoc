(import ../deps/argy-bargy/argy-bargy :as argy)
(import ../lib/predoc :as p)

(def config
  ```
  The configuration for Argy-Bargy
  ```
  {:rules [:input     {:help  "The path for the input file. (Default: stdin)"
                       :proxy "path"}
           "--name"   {:help  `The name of the program. If no name is provided,
                              use the first part of the input path.`
                       :kind  :single
                       :proxy "program"
                       :short "n"}
           "--output" {:help  `The path for the output file. If no output path
                              is provided but an input path is provided, use
                              the input path (minus the file extension).
                              Otherwise, use stdout. (Default: stdout)`
                       :kind  :single
                       :proxy "path"
                       :short "o"}
           "-------------------------------------------"]
   :info {:about "Convert a document in Predoc to a manpage in mdoc."}})

(defn run []
  (def parsed (argy/parse-args "predoc" config))
  (def err (parsed :err))
  (def help (parsed :help))
  (cond
    # print help message
    (not (empty? help))
    (do
      (prin help)
      (os/exit (if (get-in parsed [:opts "help"]) 0 1)))
    # print error message
    (not (empty? err))
    (do
      (eprin err)
      (os/exit 1))
    # run command
    (do
      (def opts (parsed :opts))
      (def params (parsed :params))
      (def i-path (params :input))
      (def input (if (= :stdin i-path)
                   (file/read stdin :all)
                   (slurp i-path)))
      (def name (or (opts "name")
                    (do
                      (def len (length i-path))
                      (def r-path (string/reverse i-path))
                      (def begin (- len (or (string/find "/" r-path) len)))
                      (def end (string/find "." i-path begin))
                      (if (and begin end)
                        (string/slice i-path begin end)
                        "program"))
                    ))
      (def output (p/predoc->mdoc name input))
      (def o-path (or (opts "output")
                      (if (def pos (string/find "." (string/reverse i-path)))
                        (string/slice i-path 0 (- -2 pos))
                        :stdout)))
      (if (= :stdout o-path)
        (print output)
        (spit o-path output)))))

(defn- main [& args] (run))
