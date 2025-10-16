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

(defn- render-dictionary [b value]
  (buffer/push b "{")
  (indent+ 1)
  (var first? true)
  (each [k v] (pairs value)
    (assertf (bytes? k) "keys in JSON objects must be strings, got: %m" k)
    (def str-k (string k))
    (if first?
      (set first? false)
      (buffer/push b ",\n" indent))
    (render b k)
    (buffer/push b ":")
    (def indent?
      (if (bytes? k)
        (indent+ (inc (length (string/format "%m:" str-k))))))
    (if indent?
      (buffer/push b " ")
      (buffer/push b "\n"))
    (render b v)
    (if indent?
      (indent- (inc (length (string/format "%m:" str-k))))))
  (buffer/push b "}")
  (indent- 1))

(defn- render-indexed [b value]
  (buffer/push b "[")
  (indent+ 1)
  (var first? true)
  (each el value
    (if first?
      (set first? false)
      (buffer/push b ",\n" indent))
    (render b el))
  (buffer/push b "]")
  (indent- 1))

(defn- render-string [b s]
  (buffer/push b "\"")
  (var i 0)
  (def n (length s))
  (while (< i n)
    (def c (get s i))
    (cond
      (= c 0x08)
      (buffer/push b "\\b")
      (= c 0x09)
      (buffer/push b "\\t")
      (= c 0x0A)
      (buffer/push b "\\n")
      (= c 0x0C)
      (buffer/push b "\\f")
      (= c 0x0D)
      (buffer/push b "\\r")
      (= c 0x22)
      (buffer/push b "\\\"")
      (= c 0x5C)
      (buffer/push b "\\\\")
      (= c 0x7F)
      (buffer/format b "\\u%04x" c)
      (< c 0x20)
      (buffer/format b "\\u%04x" c)
      # 1-byte variant (0xxxxxxx)
      (< c 0x80)
      (buffer/push-byte b c)
      # 2-byte variant (110xxxxx 10xxxxxx), valid lead 0xC2..0xDF
      (<= 0xC2 c 0xDF)
      (buffer/format b "\\u%04x"
                     (bor (blshift (band c 0x1F) 6)
                          (band (get s (++ i)) 0x3F)))
      # 3-byte variant (1110xxxx 10xxxxxx 10xxxxxx), lead 0xE0..0xEF
      (<= 0xE0 c 0xEF)
      (let [cp (bor (blshift (band c 0x0F) 12)
                    (blshift (band (get s (++ i)) 0x3F) 6)
                    (band (get s (++ i)) 0x3F))]
        # JSON cannot contain raw UTF-16 surrogate code points
        (if (and (>= cp 0xD800) (<= cp 0xDFFF))
          (buffer/push b "\\uFFFD")
          (buffer/format b "\\u%04x" cp)))
      # 4-byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx), lead 0xF0..0xF4
      (<= 0xF0 c 0xF4)
      (let [cp1 (bor (blshift (band c 0x07) 18)
                     (blshift (band (get s (++ i)) 0x3F) 12)
                     (blshift (band (get s (++ i)) 0x3F) 6)
                     (band (get s (++ i)) 0x3F))
            cp2 (- cp1 0x10000)
            hi  (+ 0xD800 (band (brshift cp2 10) 0x3FF))
            lo  (+ 0xDC00 (band cp2 0x3FF))]
        # emit JSON-valid UTF-16 surrogate pair
        (buffer/format b "\\u%04x\\u%04x" hi lo))
      (error (string "invalid byte in input:" c)))
    (++ i))
  (buffer/push b "\""))

(defn- render-nil [b]
  (buffer/push b "null"))

(defn- render-other [b value]
  (buffer/push b (string/format "%m" value)))

(varfn render [b value]
  (case (type value)
    :array
    (render-indexed b value)
    :struct
    (render-indexed b value)
    :table
    (render-dictionary b value)
    :tuple
    (render-indexed b value)
    :keyword
    (render-string b value)
    :string
    (render-string b value)
    :symbol
    (render-string b value)
    :nil
    (render-nil b)
    # default
    (render-other b value)))

(defn render-doc [root]
  (def b @"")
  (render b root)
  (string b))
