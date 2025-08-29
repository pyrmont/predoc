(use ../deps/testament)

(import ../lib/parser :as p)

(defn parse-blocks [s]
  (first (peg/match p/grammar s)))

(deftest block-fm
  (def input1
    ```
    ---
    foo: bar
    baz: qux
    ---
    ```)
  (def expect1 [{:indent 0
                 :type :frontmatter
                 :value {:foo "bar" :baz "qux"}}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```

    ---
    foo: bar
    baz: qux
    ---
    ```)
  (def expect2 [{:indent 0
                 :type :paragraph
                 :value ["--- foo: bar baz: qux ---"]}])
  (is (== expect2 (parse-blocks input2))))

(deftest block-break
  (def input1
    ```
    foo  
    bar
    ```)
  (def expect1 [{:indent 0 :type
                 :paragraph :value ["foo" {:type :break} "bar"]}])
  (is (== expect1 (parse-blocks input1))))

(deftest block-code
  (def input1
    `````
    ````
    foo bar baz
    qux
    quux corge
    ````
    `````)
  (def expect1
    [{:indent 0 :type :code :value "foo bar baz\nqux\nquux corge"}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ``````
    `````
    foo
    ````
    bar
    `````
    ``````)
  (def expect2
    [{:indent 0 :type :code :value "foo\n````\nbar"}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    `````
      ````
      foo
    bar
    ````
    `````)
  (def expect3
    [{:indent 2 :type :code :value "foo\nbar"}])
  (is (== expect3 (parse-blocks input3)))
  (def input4
    `````
    ````
    foo

    bar
    ````
    `````)
  (def expect4 [{:indent 0 :type :code :value "foo\n\nbar"}])
  (is (== expect4 (parse-blocks input4))))

(deftest block-heading
  (def input1
    ```
    Foo
    ===
    ```)
  (def expect1
    [{:indent 0
      :level 1
      :type :heading
      :value ["Foo"]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    Foo
    ---
    ```)
  (def expect2
    [{:indent 0
      :level 2
      :type :heading
      :value ["Foo"]}])
  (is (== expect2 (parse-blocks input2))))

(deftest block-il
  (def input1
    ```
    - foo -
      bar baz
    - qux -
      quux
    ```)
  (def expect1
    [{:indent 0
      :kind :il
      :loose? false
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :il
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}
           {:indent 2
            :type :paragraph
            :value @["bar baz"]}]}
        {:hang 2
         :indent 0
         :kind :il
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["qux"]}
           {:indent 2
            :type :paragraph
            :value ["quux"]}]}]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    - foo -
      bar baz

    - qux -
      quux
    ```)
  (def expect2
    [{:indent 0
      :kind :il
      :loose? true
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :il
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}
           {:indent 2
            :type :paragraph
            :value @["bar baz"]}]}
        {:hang 2
         :indent 0
         :kind :il
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["qux"]}
           {:indent 2
            :type :paragraph
            :value ["quux"]}]}]}])
  (is (== expect2 (parse-blocks input2))))

(deftest block-mdoc
  (def input1
    ````
    ```
    .\" foo
    ```
    ````)
  (def expect1
    [{:indent 0
      :type :mdoc
      :value ".\\\" foo"}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ````
    ```
    .foo bar qux
    This is an input line.
    ```
    ````)
  (def expect2
    [{:indent 0
      :type :mdoc
      :value ".foo bar qux\nThis is an input line."}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    ````
    ```
    foo
    .bar
    ```
    ````)
  (def expect3
    [{:indent 0
      :type :paragraph
      :value [
        {:type :raw
         :value " foo .bar "}]}])
  (is (== expect3 (parse-blocks input3))))

(deftest block-ol
  (def input1
    ```
    1. foo
    ```)
  (def expect1
    [{:indent 0
      :kind :ol
      :loose? false
      :type :list
      :value [
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["foo"]}]}]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    1. foo
    - bar
    3. baz
    ```)
  (def expect2
    [{:indent 0
      :kind :ol
      :loose? false
      :type :list
      :value [
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["foo"]}]}]}
     {:indent 0
      :kind :ul
      :loose? false
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["bar"]}]}]}
     {:indent 0
      :kind :ol
      :loose? false
      :type :list
      :value [
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["baz"]}]}]}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    ```
    1. foo

    2. bar

    3. baz
    ```)
  (def expect3
    [{:indent 0
      :kind :ol
      :loose? true
      :type :list
      :value [
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["foo"]}]}
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["bar"]}]}
        {:hang 3
         :indent 0
         :kind :ol
         :type :list-item
         :value [
           {:indent 3
            :type :paragraph
            :value ["baz"]}]}]}])
  (is (== expect3 (parse-blocks input3))))

(deftest block-tl
  (def input1
    ```
    - foo:
      bar baz
    - qux:
      quux
    ```)
  (def expect1
    [{:indent 0
      :kind :tl
      :loose? false
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :tl
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}
           {:indent 2
            :type :paragraph
            :value @["bar baz"]}]}
        {:hang 2
         :indent 0
         :kind :tl
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["qux"]}
           {:indent 2
            :type :paragraph
            :value ["quux"]}]}]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    - foo:
      bar baz

    - qux:
      quux
    ```)
  (def expect2
    [{:indent 0
      :kind :tl
      :loose? true
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :tl
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}
           {:indent 2
            :type :paragraph
            :value @["bar baz"]}]}
        {:hang 2
         :indent 0
         :kind :tl
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["qux"]}
           {:indent 2
            :type :paragraph
            :value ["quux"]}]}]}])
  (is (== expect2 (parse-blocks input2))))

(deftest block-table
  (def input1
    ````
    ```
    |-----|-----|
    | foo | bar |
    |-----|-----|
    ```
    ````)
  (def expect1
    [{:cols 2
      :indent 0
      :type :table
      :value [
        nil
        [[{:indent 0
           :type :td
           :value ["foo"]}
          {:indent 0
           :type :td
           :value ["bar"]}]]]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ````
    ```
    |-----|------|-----|
    | foo | bar  | baz |
    | qux | quux |     |
    |-----|------|-----|
    ```
    ````)
  (def expect2
    [{:cols 3
      :indent 0
      :type :table
      :value [
        nil
        [[{:indent 0
           :type :td
           :value ["foo"]}
          {:indent 0
           :type :td
           :value ["bar"]}
          {:indent 0
           :type :td
           :value ["baz"]}]
         [{:indent 0
           :type :td
           :value ["qux"]}
          {:indent 0
           :type :td
           :value ["quux"]}
          {:indent 0
           :type :td
           :value []}]]]}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    ````
    ```
    |-----|-----|
    | foo | bar |
    | baz |
    |-----|-----|
    ```
    ````)
  (def expect3 "columns must be equal across rows")
  (assert-thrown-message expect3 (parse-blocks input3)))

(deftest block-ul
  (def input1
    ```
    - foo
    ```)
  (def expect1
    [{:indent 0
      :kind :ul
      :loose? false
      :type :list
      :value [{:hang 2
               :indent 0
               :kind :ul
               :type :list-item
               :value [{:indent 2
                        :type :paragraph
                        :value ["foo"]}]}]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    - foo
    - bar
    - baz
    ```)
  (def expect2
    [{:indent 0
      :kind :ul
      :loose? false
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}]}
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["bar"]}]}
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["baz"]}]}]}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    ```
    - foo

    - bar

    - baz
    ```)
  (def expect3
    [{:indent 0
      :kind :ul
      :loose? true
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}]}
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["bar"]}]}
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["baz"]}]}]}])
  (is (== expect3 (parse-blocks input3)))
  (def input4
    ```
    - foo
      - bar
        - baz
    - qux
    ```)
  (def expect4
    [{:indent 0
      :kind :ul
      :loose? false
      :type :list
      :value [
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["foo"]}
           {:indent 2
            :kind :ul
            :loose? false
            :type :list
            :value [
              {:hang 4
               :indent 2
               :kind :ul
               :type :list-item
               :value [
                 {:indent 4
                  :type :paragraph
                  :value ["bar"]}
                 {:indent 4
                  :kind :ul
                  :loose? false
                  :type :list
                  :value [
                    {:hang 6
                     :indent 4
                     :kind :ul
                     :type :list-item
                     :value [
                       {:indent 6
                        :type :paragraph
                        :value ["baz"]}]}]}]}]}]}
        {:hang 2
         :indent 0
         :kind :ul
         :type :list-item
         :value [
           {:indent 2
            :type :paragraph
            :value ["qux"]}]}]}])
  (is (== expect4 (parse-blocks input4)))
  )

(deftest block-paragraph
  (def input1
    ```
    foo
    ```)
  (def expect1 [{:indent 0 :type :paragraph :value ["foo"]}])
  (is (== expect1 (parse-blocks input1)))
  (def input2
    ```
    foo
    bar
    baz
    ```)
  (def expect2 [{:indent 0 :type :paragraph :value ["foo bar baz"]}])
  (is (== expect2 (parse-blocks input2)))
  (def input3
    ```
       foo
         bar
      baz
    ```)
  (def expect3 [{:indent 3 :type :paragraph :value ["foo bar baz"]}])
  (is (== expect3 (parse-blocks input3))))

(run-tests!)
