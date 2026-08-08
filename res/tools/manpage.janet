(import ../../lib/util)
(import ../../lib/cli)

(def- s util/sep)

(def- paths
  ["examples"
   "man"])

(defn- parse-args
  [args]
  (def force? (= "-f" (get args 1)))
  (def begin (if force? 2 1))
  (def pages (array/slice args begin))
  [force? pages])

(defn- special?
  [entry]
  (or (= "." entry) (= ".." entry)))

(defn- stale?
  [src dest force?]
  (def modified (os/stat dest :modified))
  (or force?
      (nil? modified)
      (< modified (os/stat src :modified))))

(defn- convert
  [src dest force? &opt format]
  (when (stale? src dest force?)
    (def prefix (string (os/cwd) s))
    (def rel-src (string/replace prefix "" src))
    (def rel-dest (string/replace prefix "" dest))
    (print "converting " rel-src " to " rel-dest)
    (setdyn :predoc-file src)
    (setdyn :args (if (nil? format)
                    ["predoc" src "-o" dest]
                    ["predoc" src "-f" format "-o" dest]))
    (cli/run)))

(defn main
  [& args]
  (def [force? pages] (parse-args args))
  (def threeup (comp util/parent util/parent util/parent))
  (def bundle-root (-> (dyn :current-file) util/abspath threeup))
  (def entries (map (partial string bundle-root s) paths))
  # the examples are also rendered to HTML so that the output of the html
  # format can be viewed without needing to run predoc
  (def examples (string bundle-root s "examples" s))
  (each entry entries
    (if (= :directory (os/stat entry :mode))
      (->> (os/dir entry)
           (filter (comp not special?))
           (map (partial string entry s))
           (array/concat entries))
      (when (and (string/has-suffix? ".predoc" entry)
                 (or (empty? pages)
                     (find (fn [p] (string/has-suffix? p entry)) pages)))
        (def src entry)
        (def dest (string/slice src 0 -8))
        (convert src dest force?)
        (when (string/has-prefix? examples src)
          (convert src (string dest ".html") force? "html"))))))
