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
