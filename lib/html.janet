# helpers

(def- nl 10)
(def- sp 32)
(def- am 38)
(def- lt 60)
(def- gt 62)
(def- bs 92)

# state

(def- authors @[])
(var- progname nil)
(var- no-author? true)
(var- section nil)
(var- subsection nil)

(defn reset []
  (array/clear authors)
  (set progname nil)
  (set no-author? true)
  (set section nil)
  (set subsection nil))

(defn- buffer-esc [b s &opt inline?]
  (def escapes
    {38 "&amp;" 60 "&lt;" 62 "&gt;"})
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
  (def [class value]
    (case (get node :kind)
      :mod
      ["arg-mod" (get node :value)]
      :opt
      ["arg-opt" (string "-" (get node :value))]
      :param
      ["arg-param" (get node :value)]
      :etc
      ["" "..."]))
  (buffer/push b `<span class="` class `">` value "</span>"))

(defn- render-string [b s]
  (var i 0)
  (def len (length s))
  (while (< i len)
    (when (def ch (get s i))
      (cond
        # backslash
        (and (= bs ch) (= bs (get s (inc i))))
        (do
          (++ i)
          (buffer/push b "\\"))
        # ampersand
        (= am ch)
        (buffer/push b "&amp;")
        # less than
        (= lt ch)
        (buffer/push b "&lt;")
        # greater than
        (= gt ch)
        (buffer/push b "&gt;")
        # default
        (buffer/push b ch)))
    (++ i)))

# dependent render- functions

(defn- render-args [b node]
  (def an-optional? (= :optional (get node :kind)))
  (def an-alternate? (= :alternate (get node :kind)))
  (when an-optional?
    (buffer/push b `<span class="optional">`))
  (var first? true)
  (each arg (get node :value)
    (when an-alternate?
      (if first?
        (set first? false)
        (buffer/push b "|")))
    (cond
      (= " " arg)
      (buffer/push b arg)
      (= "=" arg)
      (buffer/push b arg)
      (= :arg (get arg :type))
      (render-arg b arg)
      (= :args (get arg :type))
      (render-args b arg)))
  (when an-optional?
    (buffer/push b "</span>")))

(defn- render-authors [b]
  (when no-author?
    (break))
  (set no-author? true)
  (buffer-line b `<h2 class="section">Authors</h2>`)
  (each a authors
    (when (def m (peg/match '(* '(some (if-not " <" 1))
                                (? (* " <" '(to ">") ">"))) a))
      (def [name email] m)
      (buffer/push b `<span class="author-name">` name `</span>`)
      (if email
        (buffer/push b ` &lt;<a href="mailto:` email `">` email "</a>&gt;"))
      (buffer/push b nl))))

(defn- render-blockquote [b node]
  (buffer-line b "<blockquote>")
  (each v (get node :value)
    (render b v))
  (buffer-line b "</blockquote>"))

(defn- render-break [b]
  (buffer-line b "<br>"))

(defn- render-code [b node]
  (ensure-nl b)
  (buffer/push b `<div class="codeblock">`)
  (buffer-esc b (get node :value))
  (buffer/push b `</div>`))

(defn- render-command [b node]
  (def name (get node :value))
  (if (= progname name)
    (buffer/push b `<span class="name">` name `</span>`)
    (buffer/push b `<span class="command">` name `</span>`)))

(defn- render-emphasis [b node]
  (buffer/push b "<em>")
  (each v (get node :value)
    (if (table? v)
      (render b v)
      (render-string b v)))
  (buffer/push b "</em>"))

(defn- render-env-var [b node]
  (buffer-line b `<span class="ev">` (get node :value) "</span>"))

(defn- render-h [b node]
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
      (buffer-line b `<h2 class="section">` v `</h2>`))
    2
    (do
      (set subsection v)
      (buffer-line b `<h3 class="subsection">` v `</h3>`))))

(defn- render-link [b node]
  (def args (get node :value))
  (buffer/push b `<a href="` (get args 0) `">`)
  # :value could have more than 2 values. This needs to be handled
  (if (get args 1)
    (buffer/push b (get args 1))
    (buffer/push b (get args 0)))
  (buffer/push b `</a>`))

(defn- render-list-with-head [b node]
  (def loose? (get node :loose?))
  (cond
    (= :tl (get node :kind))
    (buffer-line b `<ul class="tagged-list` (if loose? "" " compact") `">`)
    (= :il (get node :kind))
    (buffer-line b `<ul class="indented-list` (if loose? "" " compact") `">`))
  (each item (get node :value)
    (buffer-line b "<li>")
    (buffer/push b `<h4>`)
    (each el (get-in item [:value 0 :value])
      (case (type el)
        :string
        (buffer/push b el)
        :table
        (render b el)))
    (buffer-line b "</h4>")
    (var i 1)
    (while (def block (get-in item [:value i]))
      (render b block)
      (++ i))
    (buffer-line b "</li>"))
  (buffer-line b "</ul>"))

(defn- render-list-without-head [b node]
  (def loose? (get node :loose?))
  (def kind (case (get node :kind) :ol "ol" :ul "ul"))
  (buffer-line b `<` kind ` class="` (if loose? "" " compact") `">`)
  (each item (get node :value)
    (buffer/push b "<li>")
    (each block (get item :value)
      (render b block))
    (buffer/push b "</li>"))
  (buffer-line b `</` kind `>`))

(defn- render-list [b node]
  (def with-head? {:tl true :il true})
  (if (get with-head? (get node :kind))
    (render-list-with-head b node)
    (render-list-without-head b node)))

(defn- render-mdoc [b node]
  (buffer-line b ".Pp")
  (buffer-line b (get node :value)))

(defn- render-para-syn [b node]
  (def children (get node :value))
  (unless children
    (break))
  (buffer/push b `<p class="synopsis">`)
  (def name (first children))
  (if (string? name)
    (render-string b name)
    (render b name))
  (buffer/push b `<span class="rest">`)
  (var i 1)
  (while (def v (get children i))
    (if (string? v)
      (render-string b v)
      (render b v))
    (++ i))
  (buffer/push b `</span></p>`))

(defn- render-para [b node]
  (if (= "SYNOPSIS" section)
    (render-para-syn b node)
    (do
      (buffer/push b "<p>")
      (each v (get node :value)
        (if (string? v)
          (render-string b v)
          (render b v)))
      (buffer/push b "</p>"))))

(defn- render-path [b node]
  (buffer-line b `<span class="path">`)
  (buffer-line b (get node :value))
  (buffer-line b `</span>`))

(defn- render-strong [b node]
  (buffer/push b "<strong>")
  (each v (get node :value)
    (if (table? v)
      (render b v)
      (render-string b v)))
  (buffer/push b "</strong>"))

(defn- render-fm [b fm &opt end?]
  (if (not end?)
    (do
      (def [title sec] (peg/match ~(* '(* :w (any (+ :w "-")))  "(" ':d+ ")") (get fm :title)))
      (def sec-name
        (case sec
          "1" "General Commands Manual"
          "2" "System Calls Manual"
          "3" "Library Functions Manual"
          "4" "Device Drivers Manual"
          "5" "File Formats Manual"
          "6" "Games Manual"
          "7" "Miscellaneous Information Manual"
          "8" "System Manager's Manual"
          "9" "Kernel Developer's Manual"
          ""))
      (def licence (-?> (get fm :license) slurp))
      (when licence
        (buffer-line b "<!--")
        (each line (string/split "\n" licence)
          (buffer-line b "  " line))
        (buffer-line b "-->"))
      (buffer-line b `<div class="header">`)
      (buffer-line b `<div class="title">` title "(" sec ")" "</div>")
      (buffer-line b `<div class="man-sec">` sec-name "</div>")
      (buffer-line b `<div class="title">` title "(" sec ")" "</div>")
      (buffer-line b "</div>"))
    (do
      (def proj (or (get fm :os)
                    (string (get fm :project) " " (get fm :version))))
      (def date (parse-date (get fm :date)))
      (buffer-line b `<div class="footer">`)
      (buffer-line b `<div class="project">` proj "</div>")
      (buffer-line b `<div class="date">` date "</div>")
      (buffer-line b `<div class="project">` proj "</div>")
      (buffer-line b "</div>"))))

(defn- render-raw [b node]
  (def s (get node :value))
  (defn backticks? [s]
    (and (string/has-prefix? " `" s)
         (string/has-suffix? "` " s)))
  (buffer/push b `<span class="raw">`)
  (buffer-esc b (if (backticks? s) (string/slice s 1 -2) s) true)
  (buffer/push b `</span>`))

(defn- render-table [b node]
  (buffer-line b "<table>")
  (def [header rows] (get node :value))
  (each row rows
    (buffer-line b "<tr>")
    (each cell row
      (buffer-line b "<td>")
      (each v (get cell :value)
        (case (type v)
          :string
          (render-string b v)
          :table
          (render b v)))
      (buffer-line b "</td>"))
    (buffer-line b "</tr>"))
  (buffer-line b "</table>"))

(defn- render-xref [b node]
  (def v (get node :value))
  (case (get node :kind)
    :manual
    (buffer/push b `<span class="xref">` (get v 0) "(" (get v 1) ")</span>")
    :section
    (buffer/push b `<a href="#` (get v 0) `">` (get v 0) `</a>`)))

(varfn render [b node]
  (case (get node :type)
    # frontmatter
    :frontmatter
    (render-fm b node)
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
    (render-para b node)
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
  (buffer-line b `<!--`)
  (buffer-line b `  Generated by predoc at ` (os/strftime fmt))
  (buffer-line b `-->`))

(defn render-doc [program root &named no-ad?]
  (set progname program)
  (def b @"")
  (unless no-ad?
    (add-note b))
  (buffer-line b `<div class="manpage">`)
  (def fm (do
           (def node (first root))
           (if (= :frontmatter (get node :type))
             (get node :value))))
  (when fm
    (when (def as (get fm :authors))
      (array/concat authors (string/split ", " as))
      (set no-author? false))
    (render-fm b fm))
  (var first? (not (nil? fm)))
  (each node root
    (if first?
      (set first? false)
      (render b node)))
  (ensure-nl b)
  (render-authors b)
  (when fm
    (render-fm b fm true))
  (buffer/push b "</div>")
  (reset)
  (string b))
