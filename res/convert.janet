(import ../lib/predoc)
(import ../lib/util)

(defn- convert [dir]
  (each entry (os/dir dir)
    (when (string/has-suffix? ".predoc" entry)
      (def src (string dir entry))
      (def dest (string/slice src 0 -8))
      (def name (string/slice entry 0 -10))
      (def prefix (string (os/cwd) "/"))
      (def rel-src (string/replace prefix "" src))
      (def rel-dest (string/replace prefix "" dest))
      (print "converting " rel-src " to " rel-dest)
      (setdyn :predoc-file src)
      (def predoc (slurp src))
      (def mdoc (predoc/predoc->mdoc name predoc))
      (spit dest mdoc))))

(defn main
  [& args]
  (def parent (-> (dyn :current-file) util/abspath util/parent util/parent))
  (convert (string parent "/examples/"))
  (convert (string parent "/man/man1/"))
  (convert (string parent "/man/man7/")))
