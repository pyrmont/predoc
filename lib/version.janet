(import ./util)

(def value
  (comptime
    (do
      (def root (-> (dyn :current-file) util/abspath util/parent util/parent))
      (def info (-> (string root util/sep "info.jdn") slurp parse))
      (or (os/getenv "PREDOC_BUILD_VERSION")
          (get info :version)
          (error "info.jdn missing version")))))
