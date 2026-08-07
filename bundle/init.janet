(import ./spork/declare-cc :as declare)

(def- seps {:windows "\\" :mingw "\\" :cygwin "\\"})
(def- s (get seps (os/which) "/"))

(defn- build-version [manifest]
  (def version (get-in manifest [:info :version]))
  (if (not= "DEVEL" version)
    version
    (do
      (def root (get manifest :local-source (os/cwd)))
      (def [r w] (os/pipe))
      (def [ok? _result]
        (protect
          (os/execute ["git" "-C" root "describe" "--always" "--dirty"]
                      :px
                      {:out w :err w})))
      (:close w)
      (if ok?
        (string version "-" (string/trim (ev/read r :all)))
        version))))

(defn build [manifest &]
  (def exes (get-in manifest [:info :artifacts :executables] []))
  (def previous-version (os/getenv "PREDOC_BUILD_VERSION"))
  (defer (os/setenv "PREDOC_BUILD_VERSION" previous-version)
    (os/setenv "PREDOC_BUILD_VERSION" (build-version manifest))
    (each exe exes
      (when (get exe :quickbin?)
        (declare/quickbin (get exe :entry) (get exe :name))))))

(defn install [manifest &]
  (def manpages (get-in manifest [:info :artifacts :manpages] []))
  (os/mkdir (string (dyn :syspath) s "man"))
  (os/mkdir (string (dyn :syspath) s "man" s "man1"))
  (os/mkdir (string (dyn :syspath) s "man" s "man7"))
  (each mp manpages
    (bundle/add-file manifest mp))
  (def libs (get-in manifest [:info :artifacts :libraries] []))
  (each lib libs
    (def prefix (get lib :prefix))
    (def paths (get lib :paths []))
    (when prefix
      (bundle/add-directory manifest prefix))
    (each path paths
      (bundle/add manifest path (string (when prefix (string prefix s)) path))))
  (def exes (get-in manifest [:info :artifacts :executables] []))
  (each exe exes
    (bundle/add-bin manifest (get exe :name))))
