(import ./spork/declare-cc :as declare)

(def- seps {:windows "\\" :mingw "\\" :cygwin "\\"})
(def- s (get seps (os/which) "/"))

(defn build [manifest &]
  (def quickbins (get-in manifest [:info :quickbin] []))
  (each [exe script] (pairs quickbins)
    (declare/quickbin script exe)))

(defn install [manifest &]
  (def manpages (get-in manifest [:info :manpage] []))
  (os/mkdir (string (dyn :syspath) s "man"))
  (os/mkdir (string (dyn :syspath) s "man" s "man1"))
  (os/mkdir (string (dyn :syspath) s "man" s "man7"))
  (each mp manpages
    (bundle/add-file manifest mp))
  (def prefix (get-in manifest [:info :source :prefix]))
  (def srcs (get-in manifest [:info :source :files] []))
  (bundle/add-directory manifest prefix)
  (each src srcs
    (bundle/add manifest src (string prefix s src)))
  (def bins (get-in manifest [:info :executable] []))
  (each bin bins
    (bundle/add-bin manifest bin))
  (def quickbins (get-in manifest [:info :quickbin] []))
  (each [exe _] (pairs quickbins)
    (bundle/add-bin manifest exe)))
