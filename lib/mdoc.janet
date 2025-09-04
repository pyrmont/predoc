# helpers

(def- nl 10)
(def- sp 32)
(def- dq 34)
(def- sq 39)
(def- po 40)
(def- pc 41)
(def- ms 45)
(def- fs 46)
(def- bo 91)
(def- bs 92)
(def- bc 93)

(def- authors @[])

(var- progname nil)
(var- no-author? true)
(var- needs-pp? false)
(var- section nil)
(var- subsection nil)
(var- inline-macros 0)

(defn- buffer-cont [b & text]
  (if (= nl (last b))
    (buffer/popn b 1))
  (buffer/push b ;text nl))

(defn- buffer-esc [b s &opt inline?]
  (def escapes
    {bo "\\(lB" bs "\\e" dq "\\(dq" fs "\\&." sq "\\(aq"})
  (var i 0)
  (while (def ch (get s i))
    (buffer/push b (get escapes ch ch))
    (++ i))
  (unless inline?
    (buffer/push b nl)))

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
      :mod
      ["Cm \\&" (get node :value)]
      :opt
      ["Fl " (get node :value)]
      :param
      ["Ar \\&" (get node :value)]
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
        (and (= ms ch) (zero? i) (ending-nl? b))
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

# dependent render- functions

(defn- render-args [b node]
  (def an-optional? (= :optional (get node :kind)))
  (def an-alternate? (= :alternate (get node :kind)))
  (when an-optional?
    (buffer-line b ".Oo"))
  (var no-space? false)
  (var first? true)
  (each arg (get node :value)
    (when (= :sequence (get node :kind))
      (if (and (not= " " arg) no-space?)
        (buffer-cont b " Ns"))
      (set no-space? (not= " " arg)))
    (when an-alternate?
      (if first?
        (set first? false)
        (buffer-cont b " No | ")))
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

(defn- render-break [b]
  (buffer-line b ".br"))

(defn- render-code [b node]
  (buffer-line b ".Bd -literal -offset indent")
  (buffer-esc b (get node :value))
  (buffer-line b ".Ed")
  (set needs-pp? true))

(defn- render-command [b node]
  (def name (get node :value))
  (if (= progname name)
    (if (= section "NAME")
      (buffer-line b ".Nm " name)
      (buffer-line b ".Nm"))
    (buffer-line b ".Ic \\&" name)))

(defn- render-emphasis [b node]
  (when (not= "Xo" (string/slice b -4 -2))
    (buffer/popn b 1)
    (buffer/push b "\\c\n"))
  (if (zero? inline-macros)
    (buffer/push b ".")
    (buffer/push b " "))
  (buffer/push b "Em ")
  (++ inline-macros)
  (each v (get node :value)
    (if (table? v)
      (render b v)
      (render-string b v)))
  (-- inline-macros))

(defn- render-env-var [b node]
  (buffer-line b ".Ev " (get node :value)))

(defn- render-h [b node]
  (buffer-line b ".")
  (def v (string/join (get node :value) " "))
  (case (get node :level)
    1
    (do
      (when (or (= "CAVEATS" v)
                (= "BUGS" v)
                (= "SECURITY CONSIDERATIONS" v))
        (buffer/popn b 2)
        (render-authors b)
        (buffer-line b "."))
      (set section v)
      (set subsection nil)
      (buffer-line b ".Sh " v))
    2
    (do
      (set subsection v)
      (buffer-line b ".Ss " v))))

(defn- render-link [b node]
  (def args (get node :value))
  (if (zero? inline-macros)
    (buffer/push b ".")
    (buffer/push b " "))
  (buffer/push b "Lk ")
  (buffer/push b (get args 0))
  # :value could have more than 2 values. This needs to be handled
  (if (get args 1)
    (buffer/push b " \"" (get args 1) "\""))
  (buffer/push b nl))

(defn- render-list-with-head [b node]
  (def loose? (get node :loose?))
  (buffer-line b ".Pp")
  (cond
    (= :tl (get node :kind))
    (buffer-line b ".Bl -tag -width Ds" (if loose? "" " -compact"))
    (= :il (get node :kind))
    (buffer-line b ".Bl -ohang -offset Ds" (if loose? "" " -compact")))
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

(defn- render-list-without-head [b node]
  (def loose? (get node :loose?))
  (def offset "3n")
  (buffer-line b ".Pp")
  (cond
    (= :ol (get node :kind))
    (buffer-line b ".Bl -enum -offset " offset (if loose? "" " -compact"))
    (= :ul (get node :kind))
    (buffer-line b ".Bl -bullet -offset " offset (if loose? "" " -compact")))
  (each item (get node :value)
    (buffer-line b ".It")
    (set needs-pp? false)
    (each block (get item :value)
      (render b block)))
  (buffer-line b ".El"))

(defn- render-list [b node]
  (def with-head? {:tl true :il true})
  (if (get with-head? (get node :kind))
    (render-list-with-head b node)
    (render-list-without-head b node)))

(defn- render-mdoc [b node]
  (buffer-line b ".Pp")
  (buffer-line b (get node :value))
  (set needs-pp? true))

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

(defn- render-path [b node]
  (buffer-line b ".Eo")
  (buffer-line b ".Pa " (get node :value))
  (buffer-line b ".Ec"))

(defn- render-strong [b node]
  (when (not= "Xo" (string/slice b -4 -2))
    (buffer/popn b 1)
    (buffer/push b "\\c\n"))
  (if (zero? inline-macros)
    (buffer/push b ".")
    (buffer/push b " "))
  (buffer/push b "Sy ")
  (++ inline-macros)
  (each v (get node :value)
    (if (table? v)
      (render b v)
      (render-string b v)))
  (-- inline-macros))

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

(defn- render-raw [b node]
  (def s (get node :value))
  (defn backticks? [s]
    (and (string/has-prefix? " `" s)
         (string/has-suffix? "` " s)))
  (buffer/push b ".Ql \"")
  (buffer-esc b (if (backticks? s) (string/slice s 1 -2) s) true)
  (buffer/push b "\"" nl))

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
    (buffer-line b ".It Xo ")
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
  (case (get node :type)
    # frontmatter
    :frontmatter
    (render-prologue b node)
    # blocks
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
    :mdoc
    (render-mdoc b node)
    :paragraph
    (render-para b node para-break?)
    # inlines
    :arg
    (render-arg b node)
    :args
    (render-args b node)
    :break
    (render-break b)
    :command
    (render-command b node)
    :emphasis
    (render-emphasis b node)
    :env-var
    (render-env-var b node)
    :link
    (render-link b node)
    :path
    (render-path b node)
    :raw
    (render-raw b node)
    :strong
    (render-strong b node)
    :xref
    (render-xref b node)
    (error (string (get node :type) " not implemented"))))

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
