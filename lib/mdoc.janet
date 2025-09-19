(import ./util)

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

(def- trail-delims ".,:;?!)")
(def- macros [
  "Ac" "Ad" "An" "Ao" "Ap" "Aq" "Ar" "At" "Bc" "Bd" "Bf" "Bk" "Bl" "Bo" "Bq"
  "Brc" "Bro" "Brq" "Bsx" "Bt" "Bx" "Cd" "Cm" "D1" "Db" "Dc" "Dd" "Dl" "Do"
  "Dq" "Dt" "Dv" "Dx" "Ec" "Ed" "Ef" "Ek" "El" "Em" "En" "Eo" "Er" "Es" "Ev"
  "Ex" "Fa" "Fc" "Fd" "Fl" "Fn" "Fo" "Fr" "Ft" "Fx" "Hf" "Ic" "In" "It" "Lb"
  "Li" "Lk" "Lp" "Ms" "Mt" "Nd" "Nm" "No" "Ns" "Nx" "Oc" "Oo" "Op" "Os" "Ot"
  "Ox" "Pa" "Pc" "Pf" "Po" "Pp" "Pq" "Qc" "Ql" "Qo" "Qq" "Re" "Rs" "Rv" "Sc"
  "Sh" "Sm" "So" "Sq" "Ss" "St" "Sx" "Sy" "Ta" "Tg" "Tn" "Ud" "Ux" "Va" "Vt"
  "Xc" "Xo" "Xr" "%A" "%B" "%C" "%D" "%I" "%J" "%N" "%O" "%P" "%Q" "%R" "%T"
  "%U" "%V"])

# state

(def- authors @[])
(var- ended-sp? false)
(var- inline-macros 0)
(var- needs-pp? false)
(var- no-author? true)
(var- progname nil)
(var- section nil)
(var- subsection nil)

(defn reset []
  (array/clear authors)
  (set ended-sp? false)
  (set inline-macros 0)
  (set needs-pp? false)
  (set no-author? true)
  (set progname nil)
  (set section nil)
  (set subsection nil))

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

(defn- ending-sp? [b]
  (= sp (last b)))

(defn- ensure-nl [b]
  (unless (ending-nl? b)
    (buffer/push b nl)))

(defn- trailing-delim [s start]
  (peg/match ~(* (some '(set ,trail-delims)) (+ (set " \t") -1)) s start))

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
  (def name (get node :value))
  (def macro? (index-of name macros))
  (def [macro value]
    (case (get node :kind)
      :mod
      ["Cm " name]
      :opt
      ["Fl " name]
      :param
      ["Ar " name]
      :etc
      ["No " "..."]))
  (buffer-line b "." macro (if macro? "\\&" "") value))

(defn- render-oneliner [b s]
  (buffer-line b ".Nd" (string/slice s 2)))

(defn- render-string [b s]
  (var cnt 0) # used to keep line length below 80 'bytes'
  (var i 0)
  (def len (length s))
  (var too-long? false)
  (while (< i len)
    (when (> cnt 60)
      (set too-long? true))
    (def delims (trailing-delim s i))
    (when delims
      (if (ending-nl? b)
        (do
          (buffer/popn b 1)
          (buffer/push b " ")
          (buffer-line b (string/join delims " ")))
        (buffer-line b ;delims))
      (set i (+ i (length delims)))
      (set cnt 0)
      (set too-long? false))
    (when (def ch (get s i))
      (cond
        # backslash
        (= bs ch)
        (buffer/push b "\\e")
        # hyphen
        (and (= ms ch) (zero? i) (ending-nl? b))
        (do
          (buffer/popn b 1)
          (buffer/push b " Ns -"))
        # spaces at end of lines
        (and (= sp ch) (ending-nl? b))
        (when (string/has-suffix? "\\c\n" b)
          (buffer/popn b 3)
          (buffer/push b nl))
        # spaces
        (and (= sp ch) too-long?)
        (do
          (buffer/push b nl)
          (set cnt 0)
          (set too-long? false))
        # default
        (buffer/push b ch)))
    (++ i)
    (++ cnt))
  (set ended-sp? false)
  (def lastch (last b))
  (cond
    (or (= bo lastch) (= po lastch))
    (buffer/push b "\\c")
    (= sp lastch)
    (do
      (set ended-sp? true)
      (buffer/popn b 1)))
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
      nil # skip spaces
      # (buffer-cont b arg)
      (= "=" arg)
      (buffer-cont b " No " arg)
      (= :arg (get arg :type))
      (render-arg b arg)
      (= :args (get arg :type))
      (render-args b arg)))
  (when an-optional?
    (buffer-line b ".Oc")))

(defn- render-authors [b]
  (when no-author?
    (break))
  (set no-author? true)
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
  (def macro? (index-of name macros))
  (if (= progname name)
    (if (= section "NAME")
      (buffer-line b ".Nm " (if macro? "\\&" "") name)
      (buffer-line b ".Nm"))
    (buffer-line b ".Ic " (if macro? "\\&" "") name)))

(defn- render-emphasis [b node cont-output? &opt strong?]
  (when (and cont-output?
             (ending-nl? b)
             (not (string/has-suffix? "Xo\n" b))
             (not (string/check-set trail-delims (string/slice b -3 -2))))
    (buffer/popn b 1)
    (buffer/push b "\\c\n"))
  (if (zero? inline-macros)
    (buffer/push b ".")
    (buffer/push b " "))
  (buffer/push b (if strong? "Sy" "Em") " ")
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
  (unless loose? (buffer-line b ".Pp"))
  (cond
    (= :tl (get node :kind))
    (buffer-line b ".Bl -tag -width Ds" (if loose? "" " -compact"))
    (= :il (get node :kind))
    (buffer-line b ".Bl -ohang -offset Ds" (if loose? "" " -compact")))
  (each item (get node :value)
    (buffer-line b ".It Xo")
    (set needs-pp? false)
    (each el (get-in item [:value 0 :value])
      (case (type el)
        :string
        (if (= " | " el)
          (buffer-line b ".No" el)
          (buffer-cont b (if (ending-sp? b) "" " ") el))
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
  (unless loose? (buffer-line b ".Pp"))
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

(defn- render-prologue [b node]
  (def fm (get node :value))
  (def [title sec] (peg/match ~(* '(* :w (any (+ :w "-")))  "(" ':d+ ")") (get fm :title)))
  (def date (parse-date (get fm :date)))
  (when (def as (get fm :authors))
    (array/concat authors (string/split ", " as))
    (set no-author? false))
  (def licence-path (get fm :license))
  (when licence-path
    (def parent-dir (-?> (dyn :predoc-file) util/abspath util/parent))
    (def licence (slurp
                   (if (string/has-prefix? "./" licence-path)
                     (if (nil? parent-dir)
                       (error "cannot infer path to licence")
                       (string parent-dir util/sep (string/slice licence-path 2)))
                     licence-path)))
    (each line (string/split "\n" licence)
      (buffer-line b `.\"` (if (or (nil? line) (empty? line)) "" " ") line)))
  (buffer-line b ".Dd " date)
  (buffer-line b ".Dt " title " " sec)
  (def os (-> (filter (comp not nil?) [(get fm :os) (get fm :project) (get fm :version)]) ))
  (buffer-line b ".Os" (if (get fm :os)
                         (string " " (get fm :os))
                         "")
                       (if (get fm :project)
                         (string " " (get fm :project))
                         "")
                       (if (get fm :version)
                         (string " " (get fm :version))
                         "")))

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
    (buffer-line b ".It Xo")
    (var first? true)
    (each cell row
      (if first?
        (set first? false)
        (buffer-line b ".Ta"))
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
    (buffer-line b ".Xr " (get v 0) " " (get v 1)) # I used to end \\& but why?
    :section
    (buffer-line b ".Sx \"" (get v 0) "\"")))

(varfn render [b node]
  (def cont-output? (not ended-sp?))
  (set ended-sp? false)
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
    (render-emphasis b node cont-output?)
    :env-var
    (render-env-var b node)
    :link
    (render-link b node)
    :path
    (render-path b node)
    :raw
    (render-raw b node)
    :strong
    (render-emphasis b node cont-output? true)
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
  (reset)
  (string b))
