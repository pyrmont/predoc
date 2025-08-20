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
    # check if open delimiter matches
    # match should be of the same type (first check)
    # and delimiters should not be immediately next
    # to each other
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
    :t (constant true)
    :f (constant false)
    :ch (+ (* "\\" (set "\\`*_{}[]()#+-.!")) 1)
    :hs " "
    :hs* (any :hs)
    :hs+ (some :hs)
    :ws (+ :s -1)
    :ds (+ "**" "__" "*" "_" "`")
    :nil (constant nil)
    :type (constant :type)
    :kind (constant :kind)
    :begin (constant :begin)
    :end (constant :end)
    :value (constant :value)
    # predoc
    :predoc (+ :cmd :args :arg :incl :ev :path :xref)
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
                                        (some (* '(+ "=" :hs) :hs* (+ (* :arg-o :args-opt) :arg :arg-e))))
                                     (* :arg-o :args-opt)))
                    :end ($))
                 ,table)
    # predoc: argument
    :arg (+ :arg-o :arg-p)
    :arg-o (/ (* :type (constant :arg)
                 :kind (constant :opt)
                 :begin ($)
                 "**-"
                 :value (group '(some (if-not "*" 1)))
                 "**"
                 :end ($))
              ,table)
    :arg-p (/ (* :type (constant :arg)
                 :kind (constant :param)
                 :begin ($)
                 "_"
                 :value (group '(some (if-not "_" 1)))
                 "_"
                 :end ($))
              ,table)
    :arg-e (/ (* :type (constant :arg)
                 :kind (constant :etc)
                 :begin ($)
                 :value (group '"...")
                 :end ($))
              ,table)
    # predoc: include directive
    :incl (/ (* :type (constant :incl)
                :begin ($)
                "`#include <"
                :value (group '(* (! ">") (to ">")))
                ">`"
                :end ($)) ,table)
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
                 :value (group '(* (! ">") (to ">")))
                 ">`"
                 :end ($))
    # delims
    :delim (group (+ :st-delim :em-delim :ln-delim))
    # delims: strong
    :st-delim (+ (* (constant :st*)
                    (+ (* :f ($) (> -1 (not :ws)) "**" (> 0 :ws))
                       (* :t ($) (> -1 :ws) "**" (> 0 (not :ws)))
                       (* :nil ($) "__"))
                    ($))
                 (* (constant :st_)
                    (+ (* :f ($) (> -1 (not :ws)) "__" (> 0 :ws))
                       (* :t ($) (> -1 :ws) "__" (> 0 (not :ws)))
                       (* :nil ($) "__"))
                    ($)))
    # delims: emphasis
    :em-delim (+ (* (constant :em*)
                    (+ (* :f ($) (> -1 (not :ws)) "*" (> 0 :ws))
                       (* :t ($) (> -1 :ws) "*" (> 0 (not :ws)))
                       (* :nil ($) "*"))
                    ($))
                 (* (constant :em_)
                    (+ (* :f ($) (> -1 (not :ws)) "_" (> 0 :ws))
                       (* :t ($) (> -1 :ws) "_" (> 0 (not :ws)))
                       (* :nil ($) "_"))
                    ($)))
    # delims: links
    :ln-delim (* (constant :link)
                 (+ (* :t ($) "[")
                    (* :f ($) (* "](" :ln-url ")")))
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

(defn- join [sep qindents indent lines &opt no-trim?]
  (when (and (zero? indent) (empty? qindents) (one? lines))
    (break (first lines)))
  (def res @"")
  (var first? true)
  (each line lines
    (if first?
      (set first? false)
      (buffer/push res sep))
    (var pos 0)
    (each q qindents
      (def qstr (string (string/repeat " " q) "> "))
      (if (string/has-prefix? qstr (string/slice line pos))
        (+= pos (+ q 2)) # 2 is the length of the quote marker
        (break)))
    (def unquoted (if (zero? pos) line (string/slice line pos)))
    (if (not no-trim?)
      (buffer/push res (string/trim unquoted))
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

(defn- make-cmnt [[qindents indent] & lines]
  @{:type :comment
    :indent indent
    :qindents qindents
    :value (join "\n" qindents indent lines true)})

(defn- make-code [[qindents indent] & lines]
  @{:type :code
    :indent indent
    :qindents qindents
    :value (join "\n" qindents indent lines true)})

(defn- make-fm [& keyvals]
  (def value @{})
  (each [k v] (partition 2 keyvals)
    (put value (keyword (string/ascii-lower k)) v))
  @{:type :frontmatter
    :indent 0
    :qindents []
    :value value})

(defn- make-h [[qindents indent] line divider]
  @{:type :heading
    :indent indent
    :level (if (= "=" divider) 1 2)
    :qindents qindents
    :value (-> (join " " qindents indent [line])
               (parse-inlines))})

(defn- make-indent [qstrs istr]
  (def qindents @[])
  (var qsum 0)
  (each qstr qstrs
    (def len (length qstr))
    (+= qsum len 2) # 2 is the length of the quote marker
    (array/push qindents len))
  @[qindents (+ qsum (length istr))])

(defn- make-li [loose? [qindents indent] marker lines]
  (def kind (get {42 :ul 45 :ul} (first marker) :ol))
  (def hang (+ indent (length marker)))
  (def para @{:type :paragraph
              :indent hang
              :value (-> (join " " qindents hang lines)
                         (parse-inlines))})
  @{:type :list-item
    :kind kind
    :hang hang
    :indent indent
    :loose? loose?
    :qindents qindents
    :value @[para]})

(defn- make-list [child]
  (put child :loose? nil)
  @{:type :list
    :kind (get child :kind)
    :indent (get child :indent)
    :loose? false
    :qindents (get child :qindents)
    :value @[child]})

(defn- make-para [[qindents indent] & lines]
  @{:type :paragraph
    :indent indent
    :qindents qindents
    :value (-> (join " " qindents indent lines)
               (parse-inlines))})

(defn- make-qt [indent]
  @{:type :blockquote
    :hang (+ 2 indent) # 2 is the quote marker length
    :indent indent
    :value @[]})

(defn- make-ti [loose? [qindents indent] marker f-line lines]
  (def hang (+ indent (length marker)))
  (def tag @{:type :paragraph
             :indent hang
             :value (parse-inlines f-line)})
  (def body @{:type :paragraph
              :indent hang
              :value (-> (join " " qindents hang lines)
                         (parse-inlines))})
  @{:type :list-item
    :kind :tl
    :hang hang
    :indent indent
    :loose? loose?
    :qindents qindents
    :value @[tag body]})

(defn- make-tblp [[qindents indent] & lines]
  (def rows @[])
  (var cols nil)
  (each line lines
    (if (nil? cols)
      (set cols (length line))
      (unless (= cols (length line))
        (error "columns must be equal across rows")))
    (array/push rows
                (map (fn [x]
                       @{:type :td
                         :indent 0
                         :value (parse-inlines (string/trim x))})
                     line)))
  @{:type :table
    :indent indent
    :cols cols
    :qindents qindents
    :value @[nil rows]})

(defn- nest-blocks [& nodes]
  (def res @[])
  (defn close-list [list]
    (def children (get list :value))
    (var i 0)
    (while (def child (get children i))
      (if (get child :loose?)
        (put list :loose? true))
      (put child :qindents nil)
      (put child :loose? nil)
      (++ i)))
  (defn form-queue [node]
    (when (empty? (get node :qindents)) (break [node]))
    (def res (map (fn [x] (make-qt x)) (get node :qindents)))
    (array/push res node))
  (var siblings res)
  (var indent 0)
  (def trail @[])
  (each n* nodes
    (def queue (form-queue n*))
    (put n* :qindents nil)
    (each n queue
      (def n-type (get n :type))
      (def n-indent (get n :indent))
      # use infinite loop to make break work like continue
      (while true
        # node is indented >= last node
        (when (<= indent n-indent)
          (when (= :blockquote n-type)
            (array/push siblings n)
            (array/push trail n)
            (set siblings (get n :value))
            (set indent (get n :hang)) # 2 is the quote marker length
            (break))
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
            (case (get outer :type)
              # set up for a list
              :list
              (do
                (def prev (last (get outer :value)))
                (set siblings (get prev :value))
                (set indent (get prev :hang)))
              # set up for a blockquote
              :blockquote
              (do
                (set siblings (get outer :value))
                (set indent (get outer :hang))) # 2 is the quote marker length
              # default
              (error "impossible"))
            )
          (do
            (set siblings res)
            (set indent 0))))))
  (if (= :list (get (last res) :type))
    (close-list (last res)))
  res)

(def grammar
  ~{:main (/ (* (? :fm)
                (? :block)
                (any (* (some :nl) (? :block)))
                :eof) ,nest-blocks)
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
    :indent (/ (* (group (any (* ':hs* :qt-m))) ':hs*) ,make-indent)
    :line (* (! :eol) '(to :eol) (? (if-not :break :nl)))
    :cont (if-not :block-m :line)
    :cont+ (some :cont)
    :loose? (+ (if (> -2 :break) :t) :f)
    # front matter
    :fm (/ (* (at-least 3 "-")
              (some (* :nl ':w+ ": " '(to :nl))) :nl
              (at-least 3 "-")) ,make-fm)
    # blocks
    :block (+ :cmnt :tblp :code :ti :li :h :para)
    :block-m (+ :pre-m :code-o :ti-m :li-m)
    # quote marker
    :qt-m (* "> ")
    # predoc marker
    :pre-m (* :hs* (3 "`"))
    # comment
    :cmnt (/ (* :indent :cmnt-o :cmnt-b :cmnt-c) ,make-cmnt)
    :cmnt-o (* :pre-m :nl)
    :cmnt-c (* :pre-m)
    :cmnt-b (some (if-not :cmnt-c :cmnt-l))
    :cmnt-l (* :hs* "// " '(to :nl) :nl)
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
    :code (/ (* :code-o :code-b :code-c) ,make-code)
    :code-o (* :indent (only-tags (<- (at-least 4 "`") :cf)) :nl)
    :code-c (* :hs* (backmatch :cf))
    :code-b (any (if-not :code-c :code-l))
    :code-l (* '(to :nl) :nl)
    # tag list item
    :ti (some (/ (* :loose? :ti-m :ti-t (group :cont+)) ,make-ti))
    :ti-m (* :indent '(* "-" :hs+))
    :ti-t (* (! :ti-d) '(to :ti-d) :ti-d)
    :ti-d (* ":" :nl)
    # list item
    :li (some (/ (* :loose? :li-m (group :cont+)) ,make-li))
    :li-m (* :indent '(+ (* :d+ "." :hs+) (* (set "-*") :hs+)))
    # heading
    :h (/ (* :indent :line :h-l) ,make-h)
    :h-l (+ (* '"=" (at-least 2 "="))
            (* '"-" (at-least 2 "-")))
    # para block
    :para (/ (* :indent (some :line)) ,make-para)
    })
