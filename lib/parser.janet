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

(defn- get-node-t [t]
  (case t
    :em* :emphasis
    :em_ :emphasis
    :st* :strong
    :st_ :strong
    t))

(defn- match-delim [opens close]
  (var i (length opens))
  (while (def open (get opens (-- i)))
    (when (and (= (first open) (first close))
               (not= (get open 3) (get close 2)))
      (put open 1 true)
      (break)))
  (if (= -1 i)
    (if (nil? (get close 1))
      :maybe
      (array/clear close))
    (do
      (def stop (- (length opens) i))
      (set i 1)
      (while (< i stop)
        (def open (array/pop opens))
        (array/clear open)
        (++ i))
      (array/pop opens))))

(defn- remove-keys [ks arr]
  (each node arr
    (when (table? node)
      (each k ks
        (put node k nil)
        (remove-keys ks (get node :value))))))

(defn- match-delims [& args]
  (def res @[])
  (def s (last args))
  (def matches (array/slice args 0 -2))
  (def opens @[])
  (each m matches
    (if (array? m)
      (if (nil? (get m 1))
        (when (= :maybe (match-delim opens m))
          (array/push opens m))
        (if (get m 1)
          (array/push opens m)
          (match-delim opens m)))))
  (each open opens
    (array/clear open))
  (var s-begin 0)
  (var trail @[])
  (var parent res)
  (each m matches
    (cond
      # delim
      (array? m)
      (unless (empty? m)
        (def [d-type open? begin end] m)
        (if (< s-begin begin)
          (array/push parent (string/slice s s-begin begin)))
        (set s-begin end)
        (if open?
          (do
            (def node @{:type (get-node-t d-type) :value @[]})
            (array/push parent node)
            (array/push trail parent)
            (set parent (get node :value)))
          (do
            (when (= :link d-type)
              (array/insert parent 0 (string/slice s (+ 2 begin) (dec end))))
            (array/pop trail)
            (set parent (if (empty? trail) res (get (last trail) :value))))))
      # node
      (table? m)
      (do
        (def begin (get m :begin))
        (def end (get m :end))
        (if (< s-begin begin)
          (array/push parent (string/slice s s-begin begin)))
        (set s-begin end)
        (array/push parent m))
      ))
  (if (< s-begin (length s))
    (array/push res (string/slice s s-begin)))
  (remove-keys [:begin :end] res)
  res)

(def i-grammar*
  ~{:main (/ '(* (any (+ :predoc :delim :ln :raw :ch)) -1) ,match-delims)
    # helpers
    :ch (+ (* "\\" 1) 1)
    :hs " "
    :hs* (any :hs)
    :hs+ (some :hs)
    :ws (+ :s -1)
    :ds (+ "**" "__" "*" "_" "`")
    :type (constant :type)
    :kind (constant :kind)
    :begin (constant :begin)
    :end (constant :end)
    :value (constant :value)
    # predoc
    :predoc (+ :cmd :args :arg :ev :path :xref)
    # predoc: command
    :cmd (/ (* :type (constant :command)
               :begin ($)
               "**"
               :value (group '(some (if-not "*" 1)))
               "**"
               :end ($)) ,table)
    # predoc: arguments
    :args (+ :args-alt :args-opt :args-seq)
    :args-alt (/ (* :type (constant :args)
                    :kind (constant :alternate)
                    :begin ($)
                    :value (group (* (+ :args-opt :args-seq :arg)
                                     (some (* :hs+ "|" :hs+ (+ :args-opt :args-seq :arg :arg-e)))))
                    :end ($))
                 ,table)
    :args-opt (/ (* :type (constant :args)
                    :kind (constant :optional)
                    :begin ($)
                    "[" :hs*
                    :value (group (+ :args-alt :args-seq :arg :arg-e))
                    :hs* "]"
                    :end ($))
                 ,table)
    :args-seq (/ (* :type (constant :args)
                    :kind (constant :sequence)
                    :begin ($)
                    :value (group (+ (* (+ (* :arg-o :args-opt) :arg)
                                        (some (* ':hs :hs* (+ (* :arg-o :args-opt) :arg :arg-e))))
                                     (* :arg-o :args-opt)))
                    :end ($))
                 ,table)
    # predoc: argument
    :arg (+ :arg-o :arg-p)
    :arg-o (/ (* :type (constant :arg)
                 :kind (constant :opt)
                 :begin ($)
                 "**-"
                 :value (group '(some (if-not (set " *") 1)))
                 "**"
                 :end ($))
              ,table)
    :arg-p (/ (* :type (constant :arg)
                 :kind (constant :param)
                 :begin ($)
                 "_"
                 :value (group '(some (if-not (set " _") 1)))
                 "_"
                 :end ($))
              ,table)
    :arg-e (/ (* :type (constant :arg)
                 :kind (constant :etc)
                 :begin ($)
                 :value (group '"...")
                 :end ($))
              ,table)
    # predoc: environment variable
    :ev (/ (* :type (constant :env-var)
              :begin ($)
              "`"
              :value (group '(* (? "$") (range "AZ") (any (+ (range "AZ") "_"))))
              "`"
              :end ($)) ,table)
    # predoc: path
    :path (/ (* :type (constant :path)
                :begin ($)
                "`"
                :value (group '(* (any (if-not (set "/`") 1)) "/" (any (if-not "`" 1))))
                "`"
                :end ($)) ,table)
    # predoc: cross-reference
    :xref (/ (* :type (constant :xref)
                (+ :xref-man :xref-sec)) ,table)
    :xref-man (* :kind (constant :manual)
                 :begin ($)
                 "`"
                 :value (group (* '(* :w (to "(")) "(" ':d ")"))
                 "`"
                 :end ($))
    :xref-sec (* :kind (constant :section)
                 :begin ($)
                 "`<"
                 :value (group '(some (if-not ">" 1)))
                 ">`"
                 :end ($))
    # delims
    :delim (group (+ :st-delim :em-delim :ln-delim))
    # delims: strong
    :st-delim (+ (* (constant :st*)
                    (+ (* (constant false) ($) (> -1 (not :ws)) "**" (> 0 :ws))
                       (* (constant true) ($) (> -1 :ws) "**" (> 0 (not :ws)))
                       (* (constant nil) ($) "__"))
                    ($))
                 (* (constant :st_)
                    (+ (* (constant false) ($) (> -1 (not :ws)) "__" (> 0 :ws))
                       (* (constant true) ($) (> -1 :ws) "__" (> 0 (not :ws)))
                       (* (constant nil) ($) "__"))
                    ($)))
    # delims: emphasis
    :em-delim (+ (* (constant :em*)
                    (+ (* (constant false) ($) (> -1 (not :ws)) "*" (> 0 :ws))
                       (* (constant true) ($) (> -1 :ws) "*" (> 0 (not :ws)))
                       (* (constant nil) ($) "*"))
                    ($))
                 (* (constant :em_)
                    (+ (* (constant false) ($) (> -1 (not :ws)) "_" (> 0 :ws))
                       (* (constant true) ($) (> -1 :ws) "_" (> 0 (not :ws)))
                       (* (constant nil) ($) "_"))
                    ($)))
    # delims: links
    :ln-delim (* (constant :link)
                 (+ (* (constant true) ($) "[")
                    (* (constant false) ($) (* "](" :ln-url ")")))
                 ($))
    :ln-paren (* "(" (any (if-not ")" :ln-url)) ")")
    :ln-url (some (+ :ln-paren (if-not ")" :ch)))
    # link
    :ln (/ (* :type (constant :link)
              :begin ($)
              "<"
              :value (group '(* (thru "://") (to ">")))
              ">"
              :end ($)) ,table)
    # raw
    :raw (/ (* :type (constant :raw)
               :begin ($)
               (only-tags (<- (some "`") :rd))
               :value (group '(to (backmatch :rd)))
               (backmatch :rd)
               :end ($)) ,table)
    })

(def i-grammar (peg/compile i-grammar*))

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

(defn- parse-inlines [s]
  (or (first (peg/match i-grammar s))
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
                         (parse-inlines))})
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
               (parse-inlines))})

(defn- make-tag [indent marker firstl lines loose?]
  (def para-indent (+ indent (length marker)))
  (def proto @{:type :paragraph
               :indent para-indent
               :value (parse-inlines firstl)})
  (def notes @{:type :paragraph
               :indent para-indent
               :value (-> (join " " para-indent lines)
                          (parse-inlines))})
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
                   :value (parse-inlines cell)})
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
