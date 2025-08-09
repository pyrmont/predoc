# helpers

(defn- calc-width [value]
  (case (type value)
    :string
    (length value)
    :array
    (reduce (fn [acc v] (+ acc (calc-width v))) 0 value)
    :table
    (calc-width (get value :value))
    (error "oops")))

# make functions

(defn- make-arg-opt [name]
  @{:type :arg
    :kind :opt
    :value name})

(defn- make-arg-param [name]
  (def kind (if (= "..." name) :etc :param))
  @{:type :arg
    :kind kind
    :value name})

(defn- make-args [kind & captures]
  (def args @[])
  (var ns? false)
  (each c captures
    (when ns?
      (put c :ns? true)
      (set ns? false))
    (cond
      (table? c)
      (array/push args c)
      (= "|" c)
      (array/push args @{:type :arg
                         :kind :alt
                         :value c})
      (= :ns c)
      (set ns? true)))
  @{:type :args
    :kind kind
    :value args})

(defn- make-em [text & args]
  @{:type :emphasis
    :value text})

(defn- make-env-var [name]
  @{:type :env-var
    :value name})

(defn- make-cmd [name]
  @{:type :command
    :value name})

(defn- make-path [path]
  @{:type :path
    :value path})

(defn- make-raw [text]
  @{:type :verbatim
    :value text})

(defn- make-str [text]
  @{:type :strong
    :value text})

(defn- make-xref-man [name sec]
  @{:type :xref
    :kind :manual
    :value [name sec]})

(defn- make-xref-sec [heading]
  @{:type :xref
    :kind :section
    :value heading})

(def- i-grammar*
  ~{:main (* (some (+ :inline :plain)) -1)
    # helpers
    :ch (+ (* "\\" 1) 1)
    :hs " "
    :hs+ (some :hs)
    # inlines
    :inline (+ :cmd :arg :ev :path :xref :em :str :raw)
    # command
    :cmd (/ (* "**" '(some (if-not "*" 1)) "**") ,make-cmd)
    # arguments
    :arg-o (/ (* "**" "-" '(some (if-not "**" :ch)) "**") ,make-arg-opt)
    :arg-p (/ (* "_" '(some (if-not "_" :ch)) "_") ,make-arg-param)
    :arg-i (+ (* :arg-o (constant :ns) (+ :args-o :arg-p))
              (* :arg-o (some (* :hs+ (+ :args-o :arg-p))))
              (* :arg-p (some (* :hs+ (+ :args-o :arg-p))))
              :arg-o
              :arg-p)
    :args-a (/ (* (constant :alternate)
                  (+ :args-o :arg-i)
                  (some (* :hs+ '"|" :hs+ (+ :args-o :arg-i)))) ,make-args)
    :args-o (/ (* (constant :optional) "[" (+  :arg-i) "]") ,make-args)
    :arg (+ :args-a :args-o :arg-i)
    # environment variable
    :ev-s '(* (? "$") (range "AZ") (any (+ (range "AZ") "_")))
    :ev (/ (* "`" :ev-s "`") ,make-env-var)
    # path
    :path-s '(* (any (if-not (set "/`") 1)) "/" (any (if-not "`" 1)))
    :path (/ (* "`" :path-s "`") ,make-path)
    # cross-references
    :xref-m (/ (* '(* :w (to "(")) "(" ':d ")") ,make-xref-man)
    :xref-s (/ (* "<" '(some (if-not ">" 1)) ">") ,make-xref-sec)
    :xref (* "`" (+ :xref-m :xref-s) "`")
    # emphasis text
    :em (/ (* "*" '(some (if-not "*" (+ :str :ch))) "*") ,make-em)
    # strong text
    :str-a (* "**" '(some (if-not "**" (+ :em :ch))) "**")
    :str-u (* "__" '(some (if-not "__" (+ :em :ch))) "__")
    :str (/ (+ :str-a :str-u) ,make-str)
    # verbatim text
    :raw-d (at-least 1 "`")
    :raw (/ (* :raw-d '(some (if-not :raw-d 1)) :raw-d) ,make-raw)
    # plain text
    :plain '(some (if-not :inline :ch))
    })

(def- i-grammar (peg/compile i-grammar*))

(defn- join [sep indent lines]
  (when (and (zero? indent) (one? lines))
    (break (first lines)))
  (def res @"")
  (var first? true)
  (each line lines
    (if first?
      (set first? false)
      (buffer/push res sep))
    (var i -1)
    (while (def c (get line (++ i)))
      (unless (= 32 c)
        (break))
      (if (= indent i)
        (break)))
    (buffer/push res (string/slice line i)))
  (string res))

(defn- parse-inline [s]
  (or (peg/match i-grammar s)
      (error "invalid text")))

(defn- make-code [indent & lines]
  @{:type :code
    :indent indent
    :value (join "\n" indent lines)})

(defn- make-fm [& keyvals]
  (def res @{:type :frontmatter
             :indent 0})
  (var i -1)
  (while (def k (get keyvals (++ i)))
    (def v (get keyvals (++ i)))
    (put res (keyword (string/ascii-lower k)) v))
  res)

(defn- make-h [indent line divider]
  @{:type :heading
    :indent indent
    :level (if (= "=" divider) 1 2)
    :value line})

(defn- make-li [indent marker & lines]
  (def kind (get {42 :ul 45 :ul} (first marker) :ol))
  (def para-indent (+ indent (length marker)))
  (def para @{:type :paragraph
              :indent para-indent
              :value (-> (join " " para-indent lines)
                         (parse-inline))})
  @{:type :list-item
    :kind kind
    :hang para-indent
    :indent indent
    :value @[para]})

(defn- make-list [child]
  @{:type :list
    :kind (get child :kind)
    :indent (get child :indent)
    :loose? (get child :loose?)
    :value @[child]})

(defn- make-para [indent & lines]
  @{:type :paragraph
    :indent indent
    :value (-> (join " " indent lines)
               (parse-inline))})

(defn- make-tag [indent marker firstl lines loose?]
  (def para-indent (+ indent (length marker)))
  (def proto @{:type :paragraph
               :indent para-indent
               :value (parse-inline firstl)})
  (def notes @{:type :paragraph
               :indent para-indent
               :value (-> (join " " para-indent lines)
                          (parse-inline))})
  @{:type :list-item
    :kind :tag
    :hang para-indent
    :indent indent
    :loose? loose?
    :value @[proto notes]})

(defn- make-tbl [indent & lines]
  (def rows @[])
  (var cols nil)
  (var widths @[])
  (var i 0)
  (while (def line (get lines i))
    (unless (= cols (length line))
      (unless (nil? cols)
        (error (string "unequal number of cells in row " (inc i))))
      (set cols (length line)))
    (var row @[])
    (var j 0)
    (while (def cell (get line j))
      (def value @{:type :paragraph
                   :indent 0
                   :value (parse-inline cell)})
      (def cw (calc-width value))
      (def mw (get widths j 0))
      (when (> cw mw)
        (put widths j cw))
      (array/push row value)
      (++ j))
    (array/push rows row)
    (++ i))
  @{:type :table-pipe
    :indent indent
    :cols cols
    :widths widths
    :value rows})

(defn- nest-blocks [& nodes]
  (def res @[])
  (var siblings res)
  (var indent 0)
  (def trail @[])
  (each n nodes
    (def n-type (get n :type))
    (def n-indent (get n :indent))
    # use infinite loop to make break work like continue
    (while true
      # node is indented >= last node
      (when (<= indent n-indent)
        (when (= :list-item n-type)
          (def new-list (make-list n))
          (array/push siblings new-list)
          (array/push trail new-list)
          (set siblings (get n :value))
          (set indent (+ n-indent (get n :hang)))
          (break))
        (array/push siblings n)
        (break))
      # node is indented < last node
      (def list (array/pop trail))
      (if (= :list-item n-type)
        # node is a child of list
        (when (<= (get list :indent) n-indent)
          (array/push (get list :value) n)
          (array/push trail list)
          (set siblings (get n :value))
          (set indent (+ n-indent (get n :hang)))
          (break)))
      # check whether list has any loose children
      (def children (get list :value))
      (var i 0)
      (while (def child (get children i))
        # last one doesn't count
        (if (and (not= (++ i) (length children))
                 (get child :loose?))
          (put list :loose? true))
        (put child :loose? nil))
      # prepare to check next outer level
      (if (def outer (last trail))
        (do
          (def prev (last (get outer :value)))
          (set siblings (get prev :value))
          (set indent (+ (get prev :indent) (get prev :hang))))
        (do
          (set siblings res)
          (set indent 0)))))
  res)

(def grammar
  ~{:main (/ (* (? :fm)
                :block
                (any (* (some :nl) (? :block))) -1) ,nest-blocks)
    # helpers
    :nl "\n"
    :hs " "
    :hs+ (some :hs)
    :eof -1
    :break (2 :nl)
    :indent (/ '(any :hs) ,length)
    :line (* '(some (if-not (+ :nl :eof) 1)) (? (if-not :break :nl)))
    # front matter
    :fm (/ (* (at-least 3 "-")
              (some (* :nl ':w+ ": " '(to :nl))) :nl
              (at-least 3 "-") (some :nl)) ,make-fm)
    # blocks
    :block (+ :tbl :code :h :tag :li :para)
    # table
    :tbl-o (* (3 "`") :nl (at-least 3 (+ "-" "|")))
    :tbl-c (* (? (* (at-least 3 (+ "-" "|")) :nl)) (3 "`"))
    :tbl-cd " | "
    :tbl-cc (* '(some (if-not (+ :tbl-cd :nl) 1)) (? :tbl-cd))
    :tbl-r (group (* (? "| ")
                     (some (if-not (+ :tbl-c :nl) :tbl-cc))
                     (? " |")
                     :nl))
    :tbl (/ (* :indent :tbl-o :nl
               (some :tbl-r)
               :tbl-c) ,make-tbl)
    # code
    :code-f (* (any :hs) (at-least 4 "`"))
    :code (/ (* :indent :code-f :nl
                (some (if-not :code-f :line))
                :code-f) ,make-code)
    # heading
    :h-l (+ (* '"=" (at-least 2 "="))
            (* '"-" (at-least 2 "-")))
    :h (/ (* :indent :line :h-l) ,make-h)
    # tag item
    :tag-m (* :indent '(* "-" :hs+))
    :tag-d (* ":" :nl)
    :tag-p (* '(some (if-not :tag-d 1)) :tag-d)
    :tag (some (/ (* :tag-m
                  :tag-p
                  (group (some (if-not :tag-m :line)))
                  (+ (if (> :nl) (constant true))
                     (constant false))) ,make-tag))
    # list item
    :li-m (* :indent '(+ (* :d+ "." (some " ")) (* (set "-*") :hs+)))
    :li (/ (* :li-m :line (any (if-not :li-m :line))) ,make-li)
    # para block
    :para (/ (* :indent (some :line)) ,make-para)
    })
