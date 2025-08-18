(var- level 0)
(var- indent "")

(defn- indent+ [&opt amt]
  (default amt 2)
  (+= level amt)
  (set indent (string/repeat " " level)))

(defn- indent- [&opt amt]
  (default amt 2)
  (-= level amt)
  (set indent (string/repeat " " level)))

(varfn render [b value])

(defn- render-dictionary [b value &opt immutable?]
  (unless immutable?
    (buffer/push b "@"))
  (buffer/push b "{")
  (indent+ (if immutable? 1 2))
  (var first? true)
  (each [k v] (pairs value)
    (if first?
      (set first? false)
      (buffer/push b "\n" indent))
    (render b k)
    (def indent?
      (if (bytes? k)
        (indent+ (inc (length (string/format "%m" k))))))
    (if indent?
      (buffer/push b " ")
      (buffer/push b "\n"))
    (render b v)
    (if indent?
      (indent- (inc (length (string/format "%m" k))))))
  (buffer/push b "}")
  (indent- (if immutable? 1 2)))

(defn- render-indexed [b value &opt immutable?]
  (unless immutable?
    (buffer/push b "@"))
  (buffer/push b "[")
  (indent+ (if immutable? 1 2))
  (var first? true)
  (each el value
    (if first?
      (set first? false)
      (buffer/push b "\n" indent))
    (render b el))
  (buffer/push b "]")
  (indent- (if immutable? 1 2)))

(defn- render-other [b value]
  (buffer/push b (string/format "%m" value)))

(varfn render [b value]
  (case (type value)
    :array
    (render-indexed b value)
    :buffer
    (render-other b value)
    :keyword
    (render-other b value)
    :number
    (render-other b value)
    :string
    (render-other b value)
    :struct
    (render-indexed b value true)
    :symbol
    (render-other b value)
    :table
    (render-dictionary b value)
    :tuple
    (render-indexed b value true)
    # oh no
    (error (string/format "unrecognised type %s" (type value)))))

(defn render-doc [root]
  (def b @"")
  (render b root)
  (string b))
