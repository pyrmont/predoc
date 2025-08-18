(import ./parser :as p)
(import ./mdoc)
(import ./jdn)

(defn predoc->ast [predoc]
  (def root (-?> (peg/match p/grammar predoc) first))
  (or root (error "could not parse input")))

(defn predoc->jdn [predoc]
  (->> (predoc->ast predoc)
       (jdn/render-doc)))

(defn predoc->mdoc [program predoc]
  (->> (predoc->ast predoc)
       (mdoc/render-doc program)))
