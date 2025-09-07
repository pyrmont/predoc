(import ./parser :as p)
(import ./html)
(import ./jdn)
(import ./mdoc)

(defn predoc->ast [predoc]
  (def root (-?> (peg/match p/grammar predoc) first))
  (or root (error "could not parse input")))

(defn predoc->html [program predoc &named no-ad?]
  (def ast (predoc->ast predoc))
  (html/render-doc program ast :no-ad? no-ad?))

(defn predoc->jdn [predoc]
  (->> (predoc->ast predoc)
       (jdn/render-doc)))

(defn predoc->mdoc [program predoc &named no-ad?]
  (def ast (predoc->ast predoc))
  (mdoc/render-doc program ast :no-ad? no-ad?))
