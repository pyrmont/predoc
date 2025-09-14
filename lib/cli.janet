(import ../deps/argy-bargy/argy-bargy :as argy)
(import ./predoc :as p)

(def config
  ```
  The configuration for Argy-Bargy
  ```
  {:rules [:input     {:help  `The path for the input file. To read from stdin,
                              use '-'.`
                       :proxy "path"
                       :req?  true}
           "--format" {:help    `The format to use for the output. Valid values
                                are html, jdn and mdoc.`
                       :default "mdoc"
                       :kind    :single
                       :short   "f"}
           "--name"   {:help  `The name of the program. If a value is not set
                              and input is a file path, use the portion of the
                              file name before the first '.'. If reading from
                              stdin, a value must be provided or will error.`
                       :kind  :single
                       :short "n"}
           "--no-ad"  {:help  `Disable insertion of generated-by message at the
                              top of mdoc-formatted output.`
                       :kind  :flag
                       :short "A"}
           "--output" {:help  `The path for the output file. If value is not set
                              and input is a file path, use the portion of the
                              file name before the file extension. To output to
                              stdout, use '-'.`
                       :kind  :single
                       :proxy "path"
                       :short "o"}
           "-------------------------------------------"]
   :info {:about "Convert a document in Predoc to a manpage in mdoc."}})

(defn- build-str [src begin end &opt append]
  (string (string/slice src begin end) append))

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
      (def formats {"html" :html "jdn" :jdn "mdoc" :mdoc})
      (def opts (parsed :opts))
      (def params (parsed :params))
      # set format
      (def format (get formats (opts "format")))
      (if (nil? format)
        (error "unrecognised format"))
      # set input path
      (def i-path (params :input))
      # set intermediate name
      (def name (or (opts "name")
                    (if (= "-" i-path)
                      (error "must set --name when reading from stdin"))))
      # set no ad
      (def no-ad? (opts "no-ad"))
      # set intermediate output path
      (def o-path (opts "output"))
      # set final name and output path
      (def [name* o-path*]
        (if (and (not (nil? name)) (not (nil? o-path)))
          [name o-path]
          (do
            (def begin (or (-?> (string/find-all "/" i-path) last inc) 0))
            (def ends (string/find-all "." i-path begin))
            (def ext (unless (= :mdoc format) (string "." format)))
            (if (< (length ends) 2)
              (cond
                (and (nil? name) (nil? o-path))
                (error "cannot guess name and output path using input path")
                (nil? name)
                (error "cannot guess name using input path")
                (nil? o-path)
                (error "cannot guess output path using input path")))
            (def end-o (array/pop ends))
            (def end-n (array/pop ends))
            (cond
              (and (nil? name) (nil? o-path))
              [(build-str i-path begin end-n)
               (build-str i-path begin end-o ext)]
              (nil? name)
              [(build-str i-path begin end-n) o-path]
              (nil? o-path)
              [name (build-str i-path begin end-o ext)]))))
      # read input
      (def input (if (= "-" i-path)
                   (file/read stdin :all)
                   (slurp i-path)))
      # determine render function
      (def render (case format
                    :html
                    (fn [r] (p/predoc->html name* r :no-ad? no-ad?))
                    :jdn
                    (partial p/predoc->jdn)
                    :mdoc
                    (fn [r] (p/predoc->mdoc name* r :no-ad? no-ad?))
                    (error "format renderer not implemented")))
      # render document
      (def document (render input))
      # output document
      (if (= "-" o-path*)
        (print document)
        (spit o-path* document)))))

(defn main [& args] (run))
