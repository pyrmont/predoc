(use ../deps/testament)

(import ../lib/util)

(def- s util/sep)

(defn- path [& parts]
  (string/join parts s))

(deftest relpath-same-directory
  (is (== "predoc.css"
          (util/relpath (path "" "a" "b") (path "" "a" "b" "predoc.css")))))

(deftest relpath-child-directory
  (is (== (path "css" "predoc.css")
          (util/relpath (path "" "a" "b") (path "" "a" "b" "css" "predoc.css")))))

(deftest relpath-parent-directory
  (is (== (path ".." "predoc.css")
          (util/relpath (path "" "a" "b") (path "" "a" "predoc.css")))))

(deftest relpath-sibling-directory
  (is (== (path ".." "css" "predoc.css")
          (util/relpath (path "" "a" "b") (path "" "a" "css" "predoc.css")))))

(deftest relpath-no-common-prefix
  (is (== (path ".." ".." "c" "predoc.css")
          (util/relpath (path "" "a" "b") (path "" "c" "predoc.css")))))

(deftest relpath-relative-args-resolved-against-cwd
  (is (== "predoc.css" (util/relpath (os/cwd) "predoc.css"))))

(run-tests!)
