(use ../deps/testament)

(import ../lib/parser :as p)
(import ../lib/formats/html :as html)

(defn parse-blocks [s]
  (first (peg/match p/grammar s)))

(defn render-doc [name blocks]
  (html/render-doc name blocks :no-ad? true))

(defn render-page [name blocks &named css-path]
  (html/render-page name blocks :no-ad? true :css-path css-path))

(def- frontmatter
  ```
  ---
  Title: TEST(1)
  Date: March 15, 2013
  Project: Test
  Version: 1.0
  ---

  ```)

(deftest html-page-wraps-fragment
  (def blocks (parse-blocks "foo"))
  (def fragment (render-doc "test" blocks))
  (def actual (render-page "test" blocks))
  (is (string/has-prefix? "<!doctype html>\n" actual))
  (is (string/has-suffix? "</body>\n</html>\n" actual))
  (is (string/find fragment actual)))

(deftest html-page-without-css
  (def blocks (parse-blocks "foo"))
  (def actual (render-page "test" blocks))
  (def expect
    ```
    <!doctype html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>test</title>
    </head>
    <body>
    <div class="manpage">
    <p>foo</p>
    </div>
    </body>
    </html>

    ```)
  (is (== expect actual)))

(deftest html-page-with-css
  (def blocks (parse-blocks "foo"))
  (def actual (render-page "test" blocks :css-path "../css/predoc.css"))
  (is (string/find `<link rel="stylesheet" href="../css/predoc.css">` actual)))

(deftest html-page-title-from-frontmatter
  (def blocks (parse-blocks (string frontmatter "foo")))
  (def actual (render-page "test" blocks))
  (is (string/find "<title>TEST(1)</title>" actual)))

(deftest html-page-title-escaped
  (def blocks (parse-blocks "foo"))
  (def actual (render-page `a&b"c` blocks))
  (is (string/find `<title>a&amp;b&quot;c</title>` actual)))

(deftest html-page-css-path-escaped
  (def blocks (parse-blocks "foo"))
  (def actual (render-page "test" blocks :css-path `a"b.css`))
  (is (string/find `href="a&quot;b.css"` actual)))

(deftest html-heading-has-id
  (def input
    ```
    EXIT STATUS
    ===========

    Sub Head
    --------
    ```)
  (def actual (render-doc "test" (parse-blocks input)))
  (is (string/find `<h2 class="section" id="EXIT-STATUS">EXIT STATUS</h2>` actual))
  (is (string/find `<h3 class="subsection" id="Sub-Head">Sub Head</h3>` actual)))

(deftest html-section-xref-matches-heading-id
  (def input
    ```
    NAME
    ====

    See `<EXIT STATUS>` below.

    EXIT STATUS
    ===========

    foo
    ```)
  (def actual (render-doc "test" (parse-blocks input)))
  (is (string/find `<a href="#EXIT-STATUS">EXIT STATUS</a>` actual))
  (is (string/find `id="EXIT-STATUS"` actual)))

(deftest html-mdoc-block-in-pre
  (def input
    (string "before\n\n```\n.Sh RAW\nroff <content> & more\n```\n\nafter"))
  (def actual (render-doc "test" (parse-blocks input)))
  (is (string/find "<pre>.Sh RAW\nroff &lt;content&gt; &amp; more\n</pre>" actual))
  (is (string/find "<p>before</p>" actual))
  (is (string/find "<p>after</p>" actual)))

(deftest html-tagged-list-head-escaped
  (def input
    ```
    - >0:
      An error occurred.
    ```)
  (def actual (render-doc "test" (parse-blocks input)))
  (is (string/find "<h4>&gt;0</h4>" actual))
  (is (not (string/find "<h4>>0</h4>" actual))))

(deftest html-licence-resolved-against-input-file
  (def dir "tmp-html-test")
  (def licence-path (string dir "/doc.license"))
  (defer (do (os/rm licence-path) (os/rmdir dir))
    (os/mkdir dir)
    (spit licence-path "Licence line\n")
    (def input
      (string "---\n"
              "Title: TEST(1)\n"
              "Date: March 15, 2013\n"
              "Project: Test\n"
              "Version: 1.0\n"
              "License: ./doc.license\n"
              "---\n\n"
              "foo"))
    (def blocks (parse-blocks input))
    (def [fragment page]
      (with-dyns [:predoc-file (string dir "/doc.predoc")]
        [(render-doc "test" blocks) (render-page "test" blocks)]))
    # the licence precedes anything that opens, in both renderings
    (is (< (string/find "Licence line" fragment)
           (string/find `<div class="manpage">` fragment)))
    (is (< (string/find "Licence line" page)
           (string/find "<!doctype html>" page)))))

(deftest html-fragment-unchanged
  (def blocks (parse-blocks "foo"))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    <div class="manpage">
    <p>foo</p>
    </div>
    ```)
  (is (== expect actual)))

(run-tests!)
