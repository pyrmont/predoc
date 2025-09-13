(def sep (get {:windows "\\" :cygwin "\\" :mingw "\\"} (os/which) "/"))

(def pathg ~{:main (* (+ :abspath :relpath) -1)
             :abspath (* :root (any :relpath))
             :relpath (* :part (any (* :sep :part)))
             :root '(+ "/" (* (? (* :a ":")) `\`))
             :sep  ,sep
             :part (* (+ :quoted :unquoted) (> (+ :sep -1)))
             :quoted (* `"` '(some (+ `\\` `\"` (* (! `"`) 1))) `"`)
             :unquoted '(some (+ :escaped (* (! (set `"\/ `)) 1)))
             :escaped (* `\` 1)})

(defn abspath?
  [path]
  (if (= :windows (os/which))
    (peg/match '(* (? (* :a ":")) `\`) path)
    (string/has-prefix? "/" path)))

(defn abspath
  [path]
  (if (abspath? path)
    path
    (string (os/cwd) sep path)))

(defn apart
  [path]
  (if (empty? path)
    []
    (or (peg/match pathg path)
        (error "invalid path"))))

(defn parent
  [path]
  (def parts (apart path))
  (if (empty? parts)
    parts
    (do
      (put parts 0 (string/replace sep "" (first parts)))
      (string/join (array/slice parts 0 -2) sep))))
