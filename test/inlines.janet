(use ../deps/testament)

(import ../lib/parser :as p)

(defn parse-inlines [s]
  (first (peg/match p/i-grammar s)))

(deftest inline-plain
  (def input
    ```
    foo
    ```)
  (def expect ["foo"])
  (is (== expect (parse-inlines input))))

(deftest inline-em
  (def input1
    ```
    *foo*
    ```)
  (def expect1 [{:type :emphasis :value ["foo"]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    *foo
    ```)
  (def expect2 ["*foo"])
  (is (== expect2 (parse-inlines input2)))
  (def input3
    ```
    * foo*
    ```)
  (def expect3 [{:type :emphasis :value [" foo"]}])
  (is (== expect3 (parse-inlines input3)))
  (def input4
    ```
    foo* *bar
    ```)
  (def expect4 ["foo* *bar"])
  (is (== expect4 (parse-inlines input4)))
  (def input5
    ```
    \\*foo*
    ```)
  (def expect5 ["\\\\" {:type :emphasis :value ["foo"]}])
  (is (== expect5 (parse-inlines input5))))

(deftest inline-st
  (def input1
    ```
    __foo__
    ```)
  (def expect1 [{:type :strong :value ["foo"]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    foo__
    ```)
  (def expect2 ["foo__"])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-ln
  (def input1
    ```
    [foo](https://example.org)
    ```)
  (def expect1 [{:type :link :value ["https://example.org" "foo"]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    <https://foo.bar>
    ```)
  (def expect2 [{:type :link :value ["https://foo.bar"]}])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-cmd
  (def input
    ```
    **foo**
    ```)
  (def expect [{:type :command :value ["foo"]}])
  (is (== expect (parse-inlines input))))

(deftest inline-args
  (def input1
    ```
    **--foo** | _bar_
    ```)
  (def expect1
    [{:kind :alternate
      :type :args
      :value [
        {:kind :opt
         :type :arg
         :value ["-foo"]}
        {:kind :param
         :type :arg
         :value ["bar"]}]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    **-f**[_oo_]
    ```)
  (def expect2
    [{:kind :sequence
      :type :args
      :value [
        {:kind :opt
         :type :arg
         :value ["f"]}
        {:kind :optional
         :type :args
         :value [
           {:kind :param
            :type :arg
            :value ["oo"]}]}]}])
  (is (== expect2 (parse-inlines input2)))
  (def input3
    ```
    [**--foo** _bar_ | **--baz** | _qux_]
    ```)
  (def expect3
    [{:kind :optional
      :type :args
      :value [
        {:kind :alternate
         :type :args
         :value [
           {:kind :sequence
            :type :args
            :value [
              {:kind :opt
               :type :arg
               :value ["-foo"]}
              " "
              {:kind :param
               :type :arg
               :value ["bar"]}]}
          {:kind :opt
           :type :arg
           :value ["-baz"]}
          {:kind :param
           :type :arg
           :value ["qux"]}]}]}])
  (is (== expect3 (parse-inlines input3)))
  (def input4
    ```
    [...]
    ```)
  (def expect4
    [{:kind :optional
      :type :args
      :value [{:kind :etc
               :type :arg
               :value ["..."]}]}])
  (is (== expect4 (parse-inlines input4))))

(deftest inline-arg
  (def input1
    ```
    _foo_
    ```)
  (def expect1 [{:kind :param :type :arg :value ["foo"]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    **-f**
    ```)
  (def expect2 [{:kind :opt :type :arg :value ["f"]}])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-incl
  (def input1
    ```
    `#include <foo.h>`
    ```)
	(def expect1 [{:type :incl :value ["foo.h"]}])
	(is (== expect1 (parse-inlines input1))))

(deftest inline-ev
  (def input1
    ```
    `FOO`
    ```)
	(def expect1 [{:type :env-var :value ["FOO"]}])
	(is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    `$FOO_BAR`
    ```)
  (def expect2 [{:type :env-var :value ["$FOO_BAR"]}])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-path
  (def input1
    ```
    `/`
    ```)
	(def expect1 [{:type :path :value ["/"]}])
	(is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    `~/home`
    ```)
  (def expect2 [{:type :path :value ["~/home"]}])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-xref
  (def input1
    ```
    `<FOO>`
    ```)
  (def expect1 [{:kind :section :type :xref :value ["FOO"]}])
  (is (== expect1 (parse-inlines input1)))
  (def input2
    ```
    `foo(1)`
    ```)
  (def expect2 [{:kind :manual :type :xref :value ["foo" "1"]}])
  (is (== expect2 (parse-inlines input2))))

(deftest inline-nested
  (def input
    ```
    [*__foo__* ``bar``](https://example.org/?query=(baz))
    ```)
  (def expect [{:type :link
                :value [
                  "https://example.org/?query=(baz)"
                  {:type :emphasis
                   :value [
                     {:type :strong
                      :value ["foo"]}]}
                  " "
                  {:type :raw
                   :value ["bar"]}]}])
  (is (== expect (parse-inlines input))))

(deftest inline-escaped
  (def input
    ```
    \`foo\`
    ```)
  (def expect ["\\`foo\\`"])
  (is (== expect (parse-inlines input))))

(run-tests!)
