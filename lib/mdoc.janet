# helpers

(def- nl 10)
(def- sp 32)
(def- po 40)
(def- pc 41)
(def- mi 45)
(def- bo 91)
(def- bs 92)
(def- bc 93)

(def- authors @[])

(var- progname nil)
(var- no-author? true)
(var- needs-pp? false)
(var- section nil)
(var- subsection nil)

(defn- buffer-cont [b & text]
  (if (= nl (last b))
    (buffer/popn b 1))
  (buffer/push b ;text nl))

(defn- buffer-line [b & text]
  (buffer/push b ;text nl))

(defn- calc-width [v]
  (case (type v)
    # string
    :string
    (length v)
    # array
    :array
    (do
      (var w 0)
      (var first? true)
      (each el v
        (if first?
          (set first? false)
          (+= w 1))
        (+= w (calc-width el)))
      w)
    # table
    :table
    (calc-width (get v :value))
    # impossible
    (error "invalid type")))

(defn- ending-nl? [b]
  (= nl (last b)))

(defn- ensure-nl [b]
  (unless (ending-nl? b)
    (buffer/push b nl)))

(defn- trailing-delim [s start]
  (peg/match '(* (some '(set ".,:;?!)")) (+ (set " \t") -1)) s start))

(defn- parse-date [s]
  (def months {"1" "January" "2" "February" "3" "March" "4" "April" "5" "May"
               "6" "June" "7" "July" "8" "August" "9" "September" "10" "October"
               "11" "November" "12" "December"})
  (defn date-ymd [y m d]
    (string (get months m) " " d ", " y))
  (defn date-uk [d m y]
    (string m " " d ", " y))
  (defn date-us [m d y]
    (string m " " d ", " y))
  (def month-peg ['+ ; (values months)])
  (try
    (first
      (peg/match ~{:main (+ :ymd :us :uk)
                   :1-9 (* (? "0") '(range "19"))
                   :year '(4 :d)
                   :mon (+ :1-9 '"11" '"12")
                   :day (+ '(* "3" (set "01")) '(* (set "12") :d) :1-9)
                   :sep (set "-/")
                   :ymd (/ (* :year :sep :mon :sep :day) ,date-ymd)
                   :month (capture ,month-peg)
                   :uk (/ (* :day " " :month (? ", ") :year) ,date-uk)
                   :us (/ (* :month " " :day ", " :year ) ,date-us)} s))
    ([e]
     (error "could not parse date in frontmatter"))))

# dependent helpers

# render function (forward declaration)

(varfn render [b node] nil)

# independent render- functions

(defn- render-arg [b node]
  (def [macro value]
    (case (get node :kind)
      :opt
      ["Fl " (first (get node :value))]
      :param
      ["Ar " (first (get node :value))]
      :etc
      ["No " "..."]))
  (buffer-line b "." macro value))

(defn- render-oneliner [b s]
  (buffer-line b ".Nd" (string/slice s 2)))

(defn- render-string [b s]
  (var i 0)
  (def len (length s))
  (while (< i len)
    (def delims (trailing-delim s i))
    (when delims
      (if (ending-nl? b)
        (do
          (buffer/popn b 1)
          (buffer/push b " ")
          (buffer-line b (string/join delims " ")))
        (buffer-line b ;delims))
      (set i (+ i (length delims))))
    (when (def ch (get s i))
      (cond
        # backslash
        (and (= bs ch) (= bs (get s (inc i))))
        (do
          (++ i)
          (buffer/push b "\\e"))
        # hyphen
        (and (= mi ch) (zero? i) (ending-nl? b))
        (do
          (buffer/popn b 1)
          (buffer/push b " Ns -"))
        # spaces at end of lines
        (and (= sp ch) (ending-nl? b))
        (when (string/has-suffix? "\\c\n" b)
          (buffer/popn b 3)
          (buffer/push b "\n"))
        # default
        (buffer/push b ch)))
    (++ i))
  (def lastch (last b))
  (if (or (= bo lastch) (= po lastch))
    (buffer/push b "\\c"))
  (ensure-nl b))

(defn- render-args [b node]
  (def an-optional? (= :optional (get node :kind)))
  (when an-optional?
    (buffer-line b ".Oo"))
  (var no-space? false)
  (each arg (get node :value)
    (when (= :sequence (get node :kind))
      (if (and (not= " " arg) no-space?)
        (buffer-cont b " Ns"))
      (set no-space? (not= " " arg)))
    (cond
      (= " " arg)
      (buffer-cont b arg)
      (= "=" arg)
      (buffer-cont b " No " arg)
      (= :arg (get arg :type))
      (render-arg b arg)
      (= :args (get arg :type))
      (render-args b arg)))
  (when an-optional?
    (buffer-line b ".Oc")))

(defn- render-authors [b]
  (unless no-author?
    (break))
  (set no-author? false)
  (buffer-line b ".")
  (buffer-line b ".Sh " "AUTHORS")
  (each a authors
    (when (def m (peg/match '(* '(some (if-not " <" 1))
                                (? (* " <" '(to ">") ">"))) a))
      (def [name email] m)
      (buffer/push b ".An " name)
      (if email
        (buffer/push b " Aq Mt " email))
      (buffer/push b nl))))

(defn- render-blockquote [b node]
  (buffer-line b ".Bd -ragged -offset 3n")
  (each v (get node :value)
    (render b v))
  (buffer-line b ".Ed"))

(defn- render-code [b node]
  (buffer-line b ".Bd -literal -offset indent")
  (buffer-line b (get node :value))
  (buffer-line b ".Ed")
  (set needs-pp? true))

(defn- render-command [b node]
  (def name (first (get node :value)))
  (if (= progname name)
    (if (= section "NAME")
      (buffer-line b ".Nm " name)
      (buffer-line b ".Nm"))
    (buffer-line b ".Ic " name)))

(defn- render-comment [b node]
  (each line (string/split "\n" (get node :value))
    (buffer-line b `.\" ` line)))

(defn- render-emphasis [b s]
  (when (not= "Xo" (string/slice b -4 -2))
    (buffer/popn b 1)
    (buffer/push b "\\c\n"))
  (buffer-line b ".Bf Em")
  (render-string b s)
  (buffer-line b ".Ef"))

(defn- render-env-var [b s]
  (buffer-line b ".Ev " s))

(defn- render-h [b node]
  (buffer-line b ".")
  (def v (string/join (get node :value) " "))
  (case (get node :level)
    1
    (do
      (when (or (= "CAVEATS" v)
                (= "BUGS" v)
                (= "SECURITY CONSIDERATIONS" v))
        (render-authors b))
      (set section v)
      (set subsection nil)
      (buffer-line b ".Sh " v))
    2
    (do
      (set subsection v)
      (buffer-line b ".Ss " v))))

(defn- render-incl [b s]
  (buffer-line b ".In " s))

(defn- render-link [b node]
  (def args (get node :value))
  (buffer/push b ".Lk ")
  (buffer/push b (get args 0))
  (if (get args 1)
    (buffer/push b " \"" (get args 1) "\""))
  (buffer/push b nl))

(defn- render-list-tag [b node]
  (def loose? (get node :loose?))
  (buffer-line b ".Pp")
  (buffer-line b ".Bl -tag -width Ds" (if loose? "" " -compact"))
  (each item (get node :value)
    (buffer-line b ".It Xo ")
    (set needs-pp? false)
    (each el (get-in item [:value 0 :value])
      (case (type el)
        :string
        (if (= " | " el)
          (buffer-line b ".No " el)
          (buffer-cont b el))
        :table
        (render b el)))
    (set needs-pp? false)
    (buffer-line b ".Xc")
    (var i 1)
    (while (def block (get-in item [:value i]))
      (render b block)
      (++ i)))
  (buffer-line b ".El")
  (set needs-pp? true))

(defn- render-list-other [b node]
  (def loose? (get node :loose?))
  (def offset "3n")
  (buffer-line b ".Pp")
  (cond
    (= :ol (get node :kind))
    (buffer-line b ".Bl -enum -offset " offset (if loose? "" " -compact"))
    (= :ul (get node :kind))
    (buffer-line b ".Bl -dash -offset " offset (if loose? "" " -compact")))
  (each item (get node :value)
    (buffer-line b ".It")
    (set needs-pp? false)
    (each block (get item :value)
      (render b block)))
  (buffer-line b ".El"))

(defn- render-list [b node]
  (if (= :tl (get node :kind))
    (render-list-tag b node)
    (render-list-other b node)))

(defn- render-para [b node para-break?]
  (when para-break?
    (buffer-line b ".Pp"))
  (each v (get node :value)
    (cond
      # string
      (string? v)
      (cond
        (= section "NAME")
        (render-oneliner b v)
        # default
        (render-string b v))
      # table
      (table? v)
      (render b v)))
  (set needs-pp? true))

(defn- render-path [b s]
  (buffer-line b ".Eo")
  (buffer-line b ".Pa " s)
  (buffer-line b ".Ec"))

(defn- render-strong [b s]
  (when (not= "Xo" (string/slice b -4 -2))
    (buffer/popn b 1)
    (buffer/push b "\\c\n"))
  (buffer-line b ".Bf Sy")
  (render-string b s)
  (buffer-line b ".Ef"))

(defn- render-prologue [b node]
  (def fm (get node :value))
  (def [title sec] (peg/match ~(* '(* :w (any (+ :w "-")))  "(" ':d+ ")") (get fm :title)))
  (def date (parse-date (get fm :date)))
  (if (def as (get fm :authors))
    (array/concat authors (string/split ", " as))
    (set no-author? false))
  (def licence (-?> (get fm :license) (slurp)))
  (when licence
    (each line (string/split "\n" licence)
      (buffer-line b `.\" ` line)))
  (buffer-line b ".Dd " date)
  (buffer-line b ".Dt " title " " sec)
  (buffer-line b ".Os " (or (get fm :os)
                            (string (get fm :project) " " (get fm :version)))))

(defn- render-raw [b s]
  (if (string/has-suffix? " " s)
    (buffer-line b ".Ql \"" s "\"")
    (buffer-line b ".Ql " s "\\&")))

(defn- render-table [b node]
  (buffer/push b ".Bl -column")
  (def [header rows] (get node :value))
  (def widths (array/new-filled (get node :cols) 0))
  (each row rows
    (var i 0)
    (while (def cell (get row i))
      (def width (calc-width cell))
      (if (< (get widths i) width)
        (put widths i width))
      (++ i)))
  (each w widths
    (buffer/push b " \"" (string/repeat " " w) "\""))
  (buffer/push b nl)
  (each row rows
    (buffer-line b ".It Xo")
    (var first? true)
    (each cell row
      (if first?
        (set first? false)
        (buffer-line b ".Ta "))
      (each v (get cell :value)
        (case (type v)
          :string
          (render-string b v)
          :table
          (render b v))))
    (buffer-line b ".Xc"))
  (buffer-line b ".El")
  (set needs-pp? true))

(defn- render-xref [b node]
  (def v (get node :value))
  (case (get node :kind)
    :manual
    (buffer-line b ".Xr " (get v 0) " " (get v 1) "\\&")
    :section
    (buffer-line b ".Sx \"" (get v 0) "\"")))

(varfn render [b node]
  (def para-break? needs-pp?)
  (set needs-pp? false)
  (def v (first (get node :value)))
  (case (get node :type)
    # frontmatter
    :frontmatter
    (render-prologue b node)
    # blocks
    :comment
    (render-comment b node)
    :blockquote
    (render-blockquote b node)
    :table
    (render-table b node)
    :code
    (render-code b node)
    :heading
    (render-h b node)
    :list
    (render-list b node)
    :paragraph
    (render-para b node para-break?)
    # inlines
    :arg
    (render-arg b node)
    :args
    (render-args b node)
    :command
    (render-command b node)
    :emphasis
    (render-emphasis b v)
    :env-var
    (render-env-var b v)
    :incl
    (render-incl b v)
    :link
    (render-link b node)
    :path
    (render-path b v)
    :raw
    (render-raw b v)
    :strong
    (render-strong b v)
    :xref
    (render-xref b node)
    (error (string (get node :type) " not implemented"))
    ))

(defn- add-note [b]
  (def fmt "%Y-%m-%dT%H:%M:%SZ")
  (buffer-line b `.\"`)
  (buffer-line b `.\" Generated by predoc at ` (os/strftime fmt))
  (buffer-line b `.\"`))

(defn render-doc [program root &named no-ad?]
  (set progname program)
  (def b @"")
  (unless no-ad?
    (add-note b))
  (each node root
    (render b node))
  (render-authors b)
  (string b))
