(import ../deps/argy-bargy/argy-bargy :as argy)
(import ../init :as p)


(def config
  ```
  The configuration for Argy-Bargy
  ```
  {:rules [:name      {:help  "The name of the program."
                       :proxy "program"
                       :req?  true}
           :input     {:default :stdin
                       :proxy   "path"
                       :help    "The path for the input file."}
           "--output" {:default :stdout
                       :help    "The path for the output file."
                       :kind    :single
                       :proxy   "path"
                       :short   "o"}
           "-------------------------------------------"]
   :info {:about "Convert a document in Predoc to a manpage in mdoc."}})


(defn run []
  (def parsed (argy/parse-args "predoc" config))
  (def err (parsed :err))
  (def help (parsed :help))

  (cond
    (not (empty? help))
    (do
      (prin help)
      (os/exit (if (get-in parsed [:opts "help"]) 0 1)))

    (not (empty? err))
    (do
      (eprin err)
      (os/exit 1))

    (do
      (def opts (parsed :opts))
      (def params (parsed :params))
      (def i-path (params :input))
      (def input (if (= :stdin i-path)
                   (file/read stdin :all)
                   (slurp i-path)))
      (def name (params :name))
      (def output (p/predoc->mdoc name input))
      (def o-path (opts "output"))
      (if (= :stdout o-path)
        (print output)
        (spit o-path output)))))


(defn- main [& args] (run))
