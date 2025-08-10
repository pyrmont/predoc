# helpers

(def- nl 10)
(def- sp 32)
(def- po 40)
(def- pc 41)
(def- mi 45)
(def- bo 91)
(def- bc 93)

(def- authors @[])

(var- progname nil)
(var- no-author? true)
(var- needs-pp? false)
(var- section nil)
(var- subsection nil)

(defn- buffer-line [b & text]
  (buffer/push b ;text nl))

(defn- ending-nl? [b]
  (= nl (last b)))

(defn- ensure-nl [b]
  (unless (ending-nl? b)
    (buffer/push b nl)))

(defn- ensure-sp [b]
  (unless (= sp (last b))
    (buffer/push b sp)))

(defn- leading-delim [s start]
  (def m (peg/match '(* '(some (set ".,:;?!")) (+ (set " \t") -1)) s start))
  (when m (first m)))

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

# render function (forward declaration)

(var- render nil)

# independent render- functions

(defn- render-arg [b node inline?]
  (def [macro value]
    (case (get node :kind)
      :alt
      [" " "|"]
      :etc
      ["No " "..."]
      :opt
      ["Fl " (get node :value)]
      :param
      ["Ar " (get node :value)]))
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b macro value))
    (do
      (ensure-nl b)
      (buffer-line b "." macro value))))

(defn- render-oneliner [b s]
  (buffer-line b ".Nd" (string/slice s 2)))

(defn- render-string [b s &opt inline?]
  (var i 0)
  (def len (length s))
  (while (< i len)
    (def delim (leading-delim s i))
    (when delim
      (when (ending-nl? b)
        (buffer/popn b 1)
        (buffer/push b " "))
      (buffer/push b delim nl)
      (set i (+ i (length delim))))
    (when (def ch (get s i))
      (def macro (get {po "Po" pc "Pc" bo "Bo" bc "Bc"} ch))
      (if macro
        (if inline?
          (do
            (ensure-sp b)
            (buffer/push b macro sp))
          (do
            (ensure-nl b)
            (buffer/push b "." macro nl)))
        (cond
          (and (= mi ch) (zero? i) (ending-nl? b))
          (do
            (buffer/popn b 1)
            (buffer/push b " Ns -"))
          (and (= sp ch) (ending-nl? b))
          nil # do nothing
          (buffer/push b ch))))
    (++ i)))

# dependent render- functions

(defn- render-args [b node &opt inline?]
  (def an-optional? (= :optional (get node :kind)))
  (if (ending-nl? b)
    (buffer/push b ".")
    (buffer/push b " "))
  (when an-optional?
    (if inline?
      (buffer/push b "Oo")
      (buffer/push b "Op")))
  (each arg (get node :value)
    (when (get arg :ns?)
      (buffer/push b " Ns"))
    (case (get arg :type)
      :arg
      (render-arg b arg true)
      :args
      (render-args b arg true)))
  (when an-optional?
    (if inline?
      (buffer/push b " Oc")
      (buffer/push b nl))))

(defn- render-authors [b]
  (unless no-author?
    (break))
  (set no-author? false)
  (ensure-nl b)
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

(defn- render-code [b node]
  (ensure-nl b)
  (buffer-line b ".Bd -literal -offset indent")
  (buffer-line b (get node :value))
  (buffer-line b ".Ed")
  (set needs-pp? true))

(defn- render-command [b node &opt inline?]
  (def name (get node :value))
  (if (= progname name)
    (do
     (ensure-nl b)
     (if (= section "NAME")
      (buffer-line b ".Nm " name)
      (buffer-line b ".Nm")))
    (if inline?
      (do
        (ensure-sp b)
        (buffer/push b "Ic " name))
      (do
        (ensure-nl b)
        (buffer-line b ".Ic " name)))))

(defn- render-emphasis [b s &opt inline?]
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b "Em " s))
    (do
      (ensure-nl b)
      (buffer-line b ".Em " s))))

(defn- render-env-var [b s &opt inline?]
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b "Ev " s))
    (do
      (ensure-nl b)
      (buffer-line b ".Ev " s))))

(defn- render-h [b node]
  (ensure-nl b)
  (buffer-line b ".")
  (def v (get node :value))
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

(defn- render-prologue [b node]
  (def [title sec] (peg/match ~(* '(* :w (any (+ :w "-")))  "(" ':d+ ")") (get node :title)))
  (def date (parse-date (get node :date)))
  (array/concat authors (string/split ", " (get node :authors)))
  (buffer-line b ".Dd " date)
  (buffer-line b ".Dt " title " " sec)
  (buffer-line b ".Os " (or (get node :os)
                            (string (get node :project) " " (get node :version)))))

(defn- render-list-tag [b node]
  (ensure-nl b)
  (buffer-line b ".Bl -tag -width Ds" (if (get node :loose?) "" " -compact"))
  (each item (get node :value)
    (ensure-nl b)
    (buffer/push b ".It")
    (set needs-pp? false)
    (each el (get-in item [:value 0 :value])
      (case (type el)
        :string
        (buffer/push b el)
        :table
        (render b el true)))
    (set needs-pp? false)
    (buffer/push b nl)
    (var i 1)
    (while (def block (get-in item [:value i]))
      (render b block)
      (++ i)))
  (buffer-line b ".El")
  (set needs-pp? true))

(defn- render-list-other [b node]
  (ensure-nl b)
  (cond
    (= :ol (get node :kind))
    (buffer-line b ".Bl -enum")
    (= :ul (get node :kind))
    (buffer-line b ".Bl -dash"))
  (each item (get node :value)
    (ensure-nl b)
    (buffer-line b ".It")
    (set needs-pp? false)
    (each block (get item :value)
      (render b block)))
  (buffer-line b ".El"))

(defn- render-list [b node]
  (if (= :tag (get node :kind))
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

(defn- render-table-pipe [b node]
  (ensure-nl b)
  (buffer/push b ".Bl -column")
  (each w (get node :widths)
    (buffer/push b " \"" (string/repeat " " w) "\""))
  (each row (get node :value)
    (ensure-nl b)
    (buffer/push b ".It ")
    (var first? true)
    (each cell row
      (ensure-sp b)
      (if first?
        (set first? false)
        (buffer/push b "Ta "))
      (each v (get cell :value)
        (case (type v)
          :string
          (render-string b v true)
          :table
          (render b v true)))))
  (ensure-nl b)
  (buffer-line b ".El")
  (set needs-pp? true))

(defn- render-path [b s &opt inline?]
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b "Pa " s))
    (do
      (ensure-nl b)
      (buffer-line b ".Pa " s))))

(defn- render-strong [b s &opt inline?]
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b "Sy \"" s "\""))
    (do
      (ensure-nl b)
      (buffer-line b ".Sy \"" s "\""))))

(defn- render-verbatim [b s &opt inline?]
  (if inline?
    (do
      (ensure-sp b)
      (buffer/push b "Ql \"" s "\""))
    (do
      (ensure-nl b)
      (buffer-line b ".Ql \"" s "\""))))

(defn- render-xref [b node &opt inline?]
  (def v (get node :value))
  (case (get node :kind)
    :manual
    (if inline?
      (do
        (ensure-sp b)
        (buffer/push b "Xr " (get v 0) " " (get v 1)))
      (do
        (ensure-nl b)
        (buffer-line b ".Xr " (get v 0) " " (get v 1))))
    :section
    (if inline?
      (do
        (ensure-sp b)
        (buffer/push b "Sx \"" v "\""))
      (do
        (ensure-nl b)
        (buffer-line b ".Sx \"" v "\"")))))

(varfn render [b node &opt inline?]
  (def para-break? needs-pp?)
  (set needs-pp? false)
  (def v (get node :value))
  (case (get node :type)
    # frontmatter
    :frontmatter
    (render-prologue b node)
    # blocks
    :table-pipe
    (render-table-pipe b node)
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
    (render-arg b node inline?)
    :args
    (render-args b node inline?)
    :command
    (render-command b node inline?)
    :emphasis
    (render-emphasis b v inline?)
    :env-var
    (render-env-var b v inline?)
    :path
    (render-path b v inline?)
    :strong
    (render-strong b v inline?)
    :xref
    (render-xref b node inline?)
    :verbatim
    (render-verbatim b v inline?)
    #error
    ))

(defn- add-note [b]
  (def fmt "%Y-%m-%dT%H:%M:%SZ")
  (buffer-line b `.\"`)
  (buffer-line b `.\" Generated by predoc at ` (os/strftime fmt))
  (buffer-line b `.\"`))

(defn render-doc [program root &named no-note?]
  (set progname program)
  (def b @"")
  (unless no-note?
    (add-note b))
  (each node root
    (render b node))
  (render-authors b)
  (string b))
