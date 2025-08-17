(import ./parser :as p)
(import ./mdoc :as m)

(defn predoc->mdoc [program predoc]
  (def root (-?> (peg/match p/grammar predoc) first))
  (unless root
    (error "could not parse input"))
  (m/render-doc program root))
