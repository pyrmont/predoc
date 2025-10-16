(use ../deps/testament)

(import ../lib/smarty :as s)

(deftest no-changes
  (def input `foo`)
  (def expect "foo")
  (is (== expect (s/pants input))))

(deftest dquotes
  (def input1 `"foo"`)
  (def expect1 "\u201Cfoo\u201D")
  (is (== expect1 (s/pants input1)))
  (def input2 `foo, "bar".`)
  (def expect2 "foo, \u201Cbar\u201D.")
  (is (== expect2 (s/pants input2)))
  (def input3 `"foo, "bar"."`)
  (def expect3 "\u201Cfoo, \u201Cbar\u201D.\u201D")
  (is (== expect3 (s/pants input3))))

(deftest squotes
  (def input1 `foo't`)
  (def expect1 "foo\u2019t")
  (is (== expect1 (s/pants input1)))
  (def input2 `'foo'`)
  (def expect2 "\u2018foo\u2019")
  (is (== expect2 (s/pants input2)))
  (def input3 `'foo, 'bar'.'`)
  (def expect3 "\u2018foo, \u2018bar\u2019.\u2019")
  (is (== expect3 (s/pants input3)))
  (def input4 `It's '.'.`)
  (def expect4 "It\u2019s \u2018.\u2019.")
  (is (== expect4 (s/pants input4))))

(deftest ellipsis
  (def input1 `foo... bar`)
  (def expect1 "foo\u2026 bar")
  (is (== expect1 (s/pants input1))))

(deftest combo
  (def input1 `foo! "bar't baz... 'qux?'".`)
  (def expect1 "foo! \u201Cbar\u2019t baz\u2026 \u2018qux?\u2019\u201D.")
  (is (== expect1 (s/pants input1))))

(deftest escapes
  (def input1 `\"foo\"`)
  (def expect1 "\"foo\"")
  (is (== expect1 (s/pants input1)))
  (def input2 `\'foo\'`)
  (def expect2 "'foo'")
  (is (== expect2 (s/pants input2)))
  (def input3 `\*foo\*`)
  (def expect3 "\\*foo\\*")
  (is (== expect3 (s/pants input3))))

(deftest raw
  (def input1 ```foo ``bar`` baz't```)
  (def expect1 "foo ``bar`` baz\u2019t")
  (is (== expect1 (s/pants input1))))

(run-tests!)
