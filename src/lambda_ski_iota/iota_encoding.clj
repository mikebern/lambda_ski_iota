(ns lambda-ski-iota.iota-encoding)
(use 'lambda-ski-iota.combinator-definitions)
(use 'lambda-ski-iota.lambda-to-ski-translator)
(use 'lambda-ski-iota.ski-encoding)

;;;;;;;;;;;;;;;     To Iota translation     ;;;;;;;;;;;;;;;


(defn modify-func-name [func-name, from, to]
  (let [from-escaped  (clojure.string/replace (clojure.string/replace  from "*" "\\*") "?" "\\?")]
    (clojure.string/replace func-name (re-pattern (str "^" from-escaped)) to)))

; example
(modify-func-name "SKI-If" "SKI" "X")

(defn translate-definitions [source-definitions translator prefix-from prefix-to]
  (into {} (for [[k v] source-definitions] [(symbol (modify-func-name k prefix-from prefix-to)) (translator v)])))

(def iota-mapping {'S "(X (X (X (X X))))", 'K "(X (X (X X)))", 'I "(X X)"})

(def iota-translations (translate-definitions @ski-translations (partial substitute-translations iota-mapping) "SKI" "X" ))

; instantiate all definitions
(defn instantiate-definitions [definitions] (doall (map (fn [[k v]] (gen-def k v))  definitions)))

(instantiate-definitions iota-translations)

;;;;;;;;;;;;;;;     Examples     ;;;;;;;;;;;;;;;

(println (get iota-translations 'X-True))

(to-bool X-True)

(to-int (X-Fact X-Five))
(to-int (X-Z Fact-Maker-Lambda X-Five))

(get iota-translations 'X-Fact)
(get iota-translations 'X-Five)

(to-int (X-McCarthy X-Eleven))
(to-int (X-McCarthy (X-Succ (X-Succ X-Hundred))))

(get iota-translations 'X-McCarthy)

(to-int-list (X-QuickSort X-L-Not-Sorted))

(get iota-translations 'X-QuickSort)

(to-int-list (X-Eratosthenes-Sieve (X-Tail (X-ConsRangeList X-Ten))))

(get iota-translations 'X-Eratosthenes-Sieve)

(to-int (X-Ackermann (X-Cons X-Three (X-Cons X-Three X-Nil))))

(get iota-translations 'X-Ackermann)

(to-bool (X-Even? X-Five))
(to-bool (X-Odd? X-Five))

(to-bool (X-Even? X-Four))
(to-bool (X-Odd? X-Four))

(to-bool (X-Even? X-FortyTwo))
(to-bool (X-Odd? X-FortyTwo))

(get iota-translations 'X-Y*)
(get iota-translations 'X-FortyTwo)

(to-int-list (X-Cons X-One (X-Cons X-Two (X-Cons X-Three X-Nil)))) ; (1,2,3)

