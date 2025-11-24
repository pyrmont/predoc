(use ../deps/testament)

(import ../lib/parser :as p)
(import ../lib/formats/mdoc :as mdoc)

(defn parse-blocks [s]
  (first (peg/match p/grammar s)))

(defn render-doc [name blocks]
  (mdoc/render-doc name blocks :no-ad? true))

(deftest mdoc-ul-basic
  (def input
    ```
    - foo
    - bar
    - baz
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    .Bl -bullet -offset 3n -compact
    .It
    foo
    .It
    bar
    .It
    baz
    .El

    ```)
  (is (== expect actual)))

(deftest mdoc-ul-with-period
  (def input
    ```
    - foo
    - .bar
    - baz
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    .Bl -bullet -offset 3n -compact
    .It
    foo
    .It
    \&.bar
    .It
    baz
    .El

    ```)
  (is (== expect actual)))

(deftest mdoc-ul-with-escaped-period
  (def input
    ```
    - foo
    - .bar
    - baz
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    .Bl -bullet -offset 3n -compact
    .It
    foo
    .It
    \&.bar
    .It
    baz
    .El

    ```)
  (is (== expect actual)))

(deftest mdoc-ol-basic
  (def input
    ```
    1. foo
    2. bar
    3. baz
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    .Bl -enum -offset 3n -compact
    .It
    foo
    .It
    bar
    .It
    baz
    .El

    ```)
  (is (== expect actual)))

(deftest mdoc-para-basic
  (def input
    ```
    This is a paragraph.
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    This is a paragraph.

    ```)
  (is (== expect actual)))

(deftest mdoc-para-with-period-start
  (def input
    ```
    .This starts with a period.
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    \&.This starts with a period.

    ```)
  (is (== expect actual)))

(deftest mdoc-emphasis
  (def input
    ```
    This is *emphasized* text.
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    This is
    .Em emphasized
    text.

    ```)
  (is (== expect actual)))

(deftest mdoc-strong
  (def input
    ```
    This is __strong__ text.
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    This is
    .Sy strong
    text.

    ```)
  (is (== expect actual)))

(deftest mdoc-escaped-period
  (def input
    ```
    Write abbreviations (i.e.\ like this). Or avoid them.
    ```)
  (def blocks (parse-blocks input))
  (def actual (render-doc "test" blocks))
  (def expect
    ```
    Write abbreviations (i.e.\u200B like this).
    Or avoid them.

    ```)
  (is (== (string/replace `\u200B` "\u200B" expect) actual)))

(run-tests!)
