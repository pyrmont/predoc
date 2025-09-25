(import ../../lib/util)
(import ../../lib/cli)

(def- s util/sep)

(def- paths
  ["examples"
   "man"])

(defn- special?
  [entry]
  (or (= "." entry) (= ".." entry)))

(defn main
  [& args]
  (def pages (array/slice args 1))
  (def threeup (comp util/parent util/parent util/parent))
  (def bundle-root (-> (dyn :current-file) util/abspath threeup))
  (def entries (map (partial string bundle-root s) paths))
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
        (def prefix (string (os/cwd) "/"))
        (def rel-src (string/replace prefix "" src))
        (def rel-dest (string/replace prefix "" dest))
        (print "converting " rel-src " to " rel-dest)
        (setdyn :predoc-file src)
        (setdyn :args ["predoc" src "-o" dest])
        (cli/run)))))
