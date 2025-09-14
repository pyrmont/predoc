(import ../lib/util)
(import ../lib/cli)

(def- paths
  ["info.jdn"
   "man/man1"
   "man/man7"])

(defn main
  [& args]
  (def parent (-> (dyn :current-file) util/abspath util/parent util/parent))
  (def to-convert (map (fn [x] (string parent util/sep x)) paths))
  (each path to-convert
    (unless (or (= "." path) (= ".." path))
      (def dir? (= :directory (os/stat path :mode)))
      (if dir?
        (array/concat to-convert (map (fn [x] (string path util/sep x)) (os/dir path)))
        (when (string/has-suffix? ".predoc" path)
          (def src path)
          (def dest (string/slice src 0 -8))
          (def prefix (string (os/cwd) "/"))
          (def rel-src (string/replace prefix "" src))
          (def rel-dest (string/replace prefix "" dest))
          (print "converting " rel-src " to " rel-dest)
          (setdyn :args ["predoc" src "-o" dest])
          (cli/run))))))
