(import ./parser :as p)
(import ./renderer :as r)

(defn predoc->mdoc [program predoc]
  (def root (-?> (peg/match p/grammar predoc) first))
  (unless root
    (error "could not parse input"))
  (r/render-doc program root))
