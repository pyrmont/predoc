# ascii codes

(def- ht 9) # tab
(def- lf 10) # linefeed
(def- hs 32) # space
(def- em 33) # !
(def- dq 34) # "
(def- sq 39) # '
(def- po 40) # (
(def- pc 41) # )
(def- cm 44) # ,
(def- ms 45) # -
(def- fs 46) # .
(def- sl 47) # /
(def- cl 58) # :
(def- sc 59) # ;
(def- ao 60) # <
(def- ac 62) # >
(def- qm 63) # ?
(def- bo 91) # [
(def- bs 92) # \
(def- bc 93) # ]
(def- bt 96) # `
(def- co 123) # {
(def- cc 125) # }

(defn- ws? [ch]
  (or (= ht ch) (= lf ch) (= hs ch)))

(defn pants [b]
  (def res (buffer/new (length b)))
  (def in-raw? false)
  (def dqo "\u201C")
  (def dqc "\u201D")
  (def sqo "\u2018")
  (def sqc "\u2019")
  (def word (array/concat (range 48 58) (range 65 91) (range 97 123)))
  (defn open-br? [ch]
    (or (= po ch) (= bo ch) (= co ch) (= ao ch)))
  (defn close-br? [ch]
    (or (= pc ch) (= bc ch) (= cc ch) (= ac ch)))
  (defn maybe-open? [ch]
    (or (open-br? ch) (= cm ch) (= ms ch) (= sl ch) (= cl ch) (= sc ch)))
  (defn maybe-close? [ch]
    (or (close-br? ch) (= fs ch) (= em ch) (= qm ch) (= cm ch) (= cl ch) (= sc ch)))
  (defn word? [ch]
    (has-value? word ch))
  (defn guess [c res b i st]
    (def op (if (= dq c) dqo sqo))
    (def cl (if (= dq c) dqc sqc))
    (def p (get b (dec i)))
    (def n (get b (inc i)))
    (def left-open (or (nil? p) (ws? p) (maybe-open? p)))
    (def right-open (or (word? n) (open-br? n)))
    (def left-close (or (word? p) (maybe-close? p)))
    (def right-close (or (nil? n) (ws? n) (maybe-close? n)))
    (def open?
      (cond
        # both heuristics are open
        (and left-open right-open)
        true
        # both heuristics are close
        (and left-close right-close)
        false
        left-open
        true
        right-close
        false
        (= c (array/peek st))
        false
        # default
        true))
    (buffer/push res (if open? op cl))
    (if open?
      (array/push st c)
      (array/pop st)))
  (defn skip-raw [b start]
    (var i start)
    (while (def ch (get b i))
      (if (not= bt ch)
        (break))
      (++ i))
    (def goal (- i start))
    (var cnt 0)
    (while (def ch (get b i))
      (if (= bt ch)
        (++ cnt)
        (set cnt 0))
      (if (= goal cnt)
        (break))
      (++ i))
    (if (nil? (get b i))
      (dec i)
      i))
  (def st @[])
  (var i 0)
  (while (def ch (get b i))
    (cond
      # backtick
      (= bt ch)
      (do
        (def start i)
        (set i (skip-raw b start))
        (buffer/push res (buffer/slice b start (inc i))))
      # double quote
      (= dq ch)
      (guess ch res b i st)
      # single quote
      (= sq ch)
      (cond
        # inside word
        (and (word? (get b (dec i))) (word? (get b (inc i))))
        (buffer/push res sqc)
        # doesn't handle elisions
        # TODO: elision handling
        # default
        (guess ch res b i st))
      # backslash
      (= bs ch)
      (do
        (def n (get b (++ i)))
        (case n
          nil
          (buffer/push res ch)
          dq
          (buffer/push res dq)
          sq
          (buffer/push res sq)
          # default
          (buffer/push res ch n)))
      # ellipsis
      (and (= fs ch) (= fs (get b (+ 1 i))) (= fs (get b (+ 2 i))))
      (do
        (buffer/push res 0xE2 0x80 0xA6)
        (+= i 2))
      # default
      (buffer/push res ch))
    (++ i))
  res)
