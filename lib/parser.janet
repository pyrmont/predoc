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
  (var parent @{:value res})
  (var siblings res)
  (each m matches
    (cond
      # delim
      (array? m)
      (unless (empty? m)
        (def [d-type open? begin end] m)
        (if (< s-begin begin)
          (array/push siblings (string/slice s s-begin begin)))
        (set s-begin end)
        (if open?
          (do
            (array/push trail parent)
            (set parent @{:type (get-node-t d-type) :value @[]})
            (array/push siblings parent)
            (set siblings (get parent :value)))
          (do
            (when (= :link d-type)
              (array/insert siblings 0 (string/slice s (+ 2 begin) (dec end))))
            (set parent (array/pop trail))
            (set siblings (get parent :value)))))
      # node
      (table? m)
      (do
        (def begin (get m :begin))
        (def end (get m :end))
        (if (< s-begin begin)
          (array/push siblings (string/slice s s-begin begin)))
        (set s-begin end)
        (array/push siblings m))
      ))
  (if (< s-begin (length s))
    (array/push res (string/slice s s-begin)))
  (remove-keys [:begin :end] res)
  res)

(def- i-grammar*
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
               :value (group '(* (if-not (set "-*") 1) (any (if-not "*" 1))))
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

(defn- join [sep indent lines &opt no-trim?]
  (when (and (zero? indent) (one? lines))
    (break (first lines)))
  (def res @"")
  (var first? true)
  (each line lines
    (if first?
      (set first? false)
      (buffer/push res sep))
    (if (not no-trim?)
      (buffer/push res (string/trim line))
      (do
        (var i 0)
        (while (def c (get line i))
          (unless (= 32 c) (break))
          (if (= indent i) (break))
          (++ i))
        (buffer/push res (string/slice line i)))))
  (string res))

(defn- parse-inlines [s]
  (or (first (peg/match i-grammar s))
      (error "invalid text")))

(defn- make-code [indent & lines]
  @{:type :code
    :indent indent
    :value (join "\n" indent lines true)})

(defn- make-fm [& keyvals]
  (def value @{})
  (each [k v] (partition 2 keyvals)
    (put value (keyword (string/ascii-lower k)) v))
  @{:type :frontmatter
    :indent 0
    :value value})

(defn- make-h [indent line divider]
  @{:type :heading
    :indent indent
    :level (if (= "=" divider) 1 2)
    :value (parse-inlines line)})

(defn- make-li [loose? indent marker lines]
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
    :loose? loose?
    :value @[para]})

(defn- make-list [child]
  (put child :loose? nil)
  @{:type :list
    :kind (get child :kind)
    :indent (get child :indent)
    :loose? false
    :value @[child]})

(defn- make-para [indent & lines]
  @{:type :paragraph
    :indent indent
    :value (-> (join " " indent lines)
               (parse-inlines))})

(defn- make-ti [loose? indent marker f-line lines]
  (def para-indent (+ indent (length marker)))
  (def tag @{:type :paragraph
             :indent para-indent
             :value (parse-inlines f-line)})
  (def body @{:type :paragraph
              :indent para-indent
              :value (-> (join " " para-indent lines)
                         (parse-inlines))})
  @{:type :list-item
    :kind :tl
    :hang para-indent
    :indent indent
    :loose? loose?
    :value @[tag body]})

(defn- make-tblp [indent & lines]
  (def rows @[])
  (var cols nil)
  (each line lines
    (if (nil? cols)
      (set cols (length line))
      (unless (= cols (length line))
        (error "columns must be equal across rows")))
    (array/push rows
      (map (fn [x] @{:type :td
                     :indent 0
                     :value (parse-inlines (string/trim x))}) line)))
  @{:type :table
    :indent indent
    :cols cols
    :value @[nil rows]})

(defn- nest-blocks [& nodes]
  (def res @[])
  (defn close-list [list]
    (def children (get list :value))
    (var i 0)
    (while (def child (get children i))
      (if (get child :loose?)
        (put list :loose? true))
      (put child :loose? nil)
      (++ i)))
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
          (set indent (get n :hang))
          (break))
        (array/push siblings n)
        (break))
      # node is indented < last node
      (def parent (array/pop trail))
      # node is a child of the parent
      (when (and (= :list-item n-type)
                 (= :list (get parent :type))
                 (= (get parent :kind) (get n :kind))
                 (<= (get parent :indent) n-indent))
        (array/push trail parent)
        (array/push (get parent :value) n)
        (set siblings (get n :value))
        (set indent (get n :hang))
        (break))
      # close parent if it's a list
      (if (= :list (get parent :type))
        (close-list parent))
      # prepare to check next outer level
      (if (def outer (last trail))
        (do
          (def prev (last (get outer :value)))
          (set siblings (get prev :value))
          (set indent (get prev :hang))) # nodes that don't have :hang?
        (do
          (set siblings res)
          (set indent 0)))))
  (if (= :list (get (last res) :type))
    (close-list (last res)))
  res)

(def grammar
  ~{:main (/ (* (? :fm) (? :block) (any (* (some :nl) (? :block))) :eof) ,nest-blocks)
    # helpers
    :t (constant true)
    :f (constant false)
    :nl "\n"
    :hs " "
    :hs* (any :hs)
    :hs+ (some :hs)
    :eof -1
    :eol (+ :nl :eof)
    :break (2 :nl)
    :indent (/ ':hs* ,length)
    :line (* (! :eol) '(to :eol) (? (if-not :break :nl)))
    :cont (if-not :block-m :line)
    :cont+ (some :cont)
    :loose? (+ (if (> -2 :break) :t) :f)
    # front matter
    :fm (/ (* (at-least 3 "-")
              (some (* :nl ':w+ ": " '(to :nl))) :nl
              (at-least 3 "-")) ,make-fm)
    # blocks
    :block (+ :tblp :code :ti :li :h :para)
    :block-m (+ :pre-m :code-o :ti-m :li-m)
    # predoc
    :pre-m (* :hs* (3 "`"))
    # pipe table
    :tblp (/ (* :indent :tblp-o (some :tblp-tr) :tblp-c) ,make-tblp)
    :tblp-o (* :pre-m :nl :hs* (at-least 3 (+ "-" "|")) :nl)
    :tblp-c (* :hs* (at-least 3 (+ "-" "|")) :nl :pre-m)
    :tblp-tr (group (* :tblp-tr-o (some :tblp-td) :tblp-tr-c))
    :tblp-tr-o (* (! :tblp-c) :hs* (? "| "))
    :tblp-tr-c (* (? " |") :nl)
    :tblp-td (* (! :tblp-tr-c) '(to (+ :tblp-td-d :tblp-tr-c)) (? :tblp-td-d))
    :tblp-td-d " | "
    # code
    :code (/ (* :code-o
                (any (if-not :code-c :line))
                :code-c) ,make-code)
    :code-o (* :indent (only-tags (<- (at-least 4 "`") :cf)) :nl)
    :code-c (* :hs* (backmatch :cf))
    # tag list item
    :ti (some (/ (* :loose? :ti-m :ti-t (group :cont+)) ,make-ti))
    :ti-m (* :indent '(* "-" :hs+))
    :ti-t (* (! :ti-d) '(to :ti-d) :ti-d)
    :ti-d (* ":" :nl)
    # list item
    :li (some (/ (* :loose? :li-m (group :cont+)) ,make-li))
    :li-m (* :indent '(+ (* :d+ "." (some " ")) (* (set "-*") :hs+)))
    # heading
    :h (/ (* :indent :line :h-l) ,make-h)
    :h-l (+ (* '"=" (at-least 2 "="))
            (* '"-" (at-least 2 "-")))
    # para block
    :para (/ (* :indent (some :line)) ,make-para)
    })
