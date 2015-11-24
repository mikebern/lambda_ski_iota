(ns lambda-ski-iota.ski-encoding)
(use 'lambda-ski-iota.combinator-definitions)
(use 'lambda-ski-iota.lambda-to-ski-translator)
(use 'backtick)
;;for debugging
(use 'clojure.tools.trace)

;(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
;nop
;(defmacro dbg[x] `(let [x# ~x]  x#))


(translate-lambda-to-ski 'SKI-Id (template (fn [x#] x#)))
((SKI-Id 5))


(translate-lambda-to-ski 'SKI-Ex1 (template (fn [x#] (fn [y#] x#))))
(((SKI-Ex1 5) 6))
((SKI-Ex1 5 6))

(translate-lambda-to-ski 'SKI-Ex2 (template (fn [x#] (fn [y#] y#))))
((SKI-Ex2 5 6))

;;;;;;;;;;;;;;;     Boolean Logic     ;;;;;;;;;;;;;;;

;True
(translate-lambda-to-ski 'SKI-True (template (fn  [a# b#] a#)))
;False
(translate-lambda-to-ski 'SKI-False (template (fn  [a# b#] b#)))
;If
(translate-lambda-to-ski 'SKI-If (template (fn [f# x# y#] (f# x# y#))))
;another version of If
(translate-lambda-to-ski 'SKI-If (template (fn [f#] (fn [x#] (fn [y#] (f# x# y#))))))


;And
(translate-lambda-to-ski 'SKI-And (template (fn  [p# q#] (p# q# p#))))
;Or
(translate-lambda-to-ski 'SKI-Or (template (fn [p# q#] (p# p# q#))))


((SKI-True "true"  "false"))
(((SKI-True "true") "false"))
((SKI-True "true" "false"))
(((SKI-True) "true" "false"))

; alternative definitions
;(def False (S K))
;(def False (K I))


((SKI-False "true"  "false"))


((SKI-If SKI-True "true" "false"))
(((SKI-If SKI-True) "true" "false"))

((SKI-If SKI-False "true"  "false"))

((SKI-If (SKI-And SKI-True SKI-True) "first"  "second"))
((SKI-If (SKI-And SKI-True SKI-False) "first"  "second"))
((SKI-If (SKI-And SKI-False SKI-True) "first"   "second"))

((SKI-If (SKI-And SKI-False SKI-False)  "first"   "second"))
((SKI-If (SKI-And SKI-False SKI-False)  "first"   "second"))

((SKI-If (SKI-Or SKI-True SKI-True)  "first"   "second"))
((SKI-If (SKI-Or SKI-True SKI-False) "first"   "second"))
((SKI-If (SKI-Or SKI-False SKI-True) "first"   "second"))
((SKI-If (SKI-Or SKI-False SKI-False) "first"   "second"))


(translate-lambda-to-ski 'SKI-Not (template (fn [p#] (p# SKI-False SKI-True))))
((SKI-If (SKI-Not SKI-True)  "first"   "second"))
((SKI-If (SKI-Not SKI-False)  "first"   "second"))

;;;;;;;;;;;;;;;     Arithmetics     ;;;;;;;;;;;;;;;

; helper functions to convert Church numerals to numbers
(defn ski-plus-one [x] (+ 1 (get-val x)))
(defn ski-concat [x] (str (get-val x) "|"))


(translate-lambda-to-ski 'SKI-Null (template (fn [f# x#] x#)))
(translate-lambda-to-ski 'SKI-Succ (template (fn [n# f# x#] (f# (n# f# x#)))))


(translate-lambda-to-ski 'SKI-One '(SKI-Succ SKI-Null))
(translate-lambda-to-ski 'SKI-Two '(SKI-Succ SKI-One))
(translate-lambda-to-ski 'SKI-Three '(SKI-Succ SKI-Two))
(translate-lambda-to-ski 'SKI-Four '(SKI-Succ SKI-Three))
(translate-lambda-to-ski 'SKI-Five '(SKI-Succ SKI-Four))
(translate-lambda-to-ski 'SKI-Six '(SKI-Succ SKI-Five))
(translate-lambda-to-ski 'SKI-Seven '(SKI-Succ SKI-Six))


(((SKI-One ski-plus-one) 0))
((SKI-Five ski-plus-one 0))
(((SKI-Five ski-plus-one) 0))


(translate-lambda-to-ski 'SKI-Pred  (template (fn [n# f# x#] ((n# (fn [g#] (fn [h#] (h# (g# f#)))) (fn [u#] x#)) (fn [w#] w#)))))


(translate-lambda-to-ski 'SKI-AnotherFour '(SKI-Pred SKI-Five))

((SKI-AnotherFour ski-plus-one 0))

(translate-lambda-to-ski 'SKI-Plus (template (fn [m# n#] (fn [f# x#] (m# f# (n# f# x#))))))

(((SKI-Plus SKI-Five SKI-Three) ski-plus-one 0))

(translate-lambda-to-ski 'SKI-Minus (template (fn [m# n#] (fn [f# x#] ((n# (fn [t#] (fn [r# s#] (SKI-Pred t# r# s#))) m#) f#  x#)))))

(translate-lambda-to-ski 'SKI-Minus2 (template (fn [m# n#] ((n# SKI-Pred) m#))))

(((SKI-Minus SKI-Five SKI-Three) ski-plus-one 0))
(((SKI-Minus SKI-Three SKI-Two) ski-plus-one 0))

(((SKI-Minus2 SKI-Five SKI-Three) ski-plus-one 0))
(((SKI-Minus2 SKI-Three SKI-Two) ski-plus-one 0))

(translate-lambda-to-ski 'SKI-Mul (template (fn [m# n#] (fn [f# x#] (m# (n# f#) x#)))))
(translate-lambda-to-ski 'SKI-IsZero (template (fn [n#] (n# (fn [x#] SKI-False) SKI-True))))
(translate-lambda-to-ski 'SKI-Eq (template (fn [m# n#] (SKI-And (SKI-IsZero (SKI-Minus m# n#)) (SKI-IsZero (SKI-Minus n# m#))))))


(((SKI-Mul SKI-Five SKI-Two) ski-plus-one 0))
((SKI-IsZero SKI-One "true" "false"))
((SKI-IsZero SKI-Null "true" "false"))
((SKI-IsZero (SKI-Minus SKI-Three SKI-Two) "true" "false"))
((SKI-IsZero (SKI-Minus SKI-Two SKI-Two) "true" "false"))


(translate-lambda-to-ski 'SKI-Ten '(SKI-Mul SKI-Five SKI-Two))
(translate-lambda-to-ski 'SKI-Hundred '(SKI-Mul SKI-Ten SKI-Ten))

((SKI-Hundred ski-concat ""))

(((SKI-Plus SKI-Two (SKI-Mul SKI-Four SKI-Ten)) ski-plus-one 0))


(translate-lambda-to-ski 'SKI-FortyTwo '(SKI-Minus (SKI-Mul SKI-Seven SKI-Seven) SKI-Seven))


(((SKI-FortyTwo) ski-plus-one 0))
(((SKI-Eq SKI-Two SKI-Two) "true" "false"))
(((SKI-Eq SKI-Two SKI-One) "true" "false"))
(((SKI-Eq SKI-One SKI-Two) "true" "false"))


;;;;;;;;;;;;;;;     Y-combinator     ;;;;;;;;;;;;;;;

(translate-lambda-to-ski 'SKI-Y (template (fn [f#] ((fn [g#] (g# g#)) (fn [h#] (fn [n#] ((f# (h# h#)) n#)))))))

(def Fact-Maker-Lambda (fn [g]
           (fn [n] (SKI-If (SKI-Eq SKI-Null n) SKI-One (SKI-Mul n (g (SKI-Minus n SKI-One)))))))
((((SKI-Y Fact-Maker-Lambda) SKI-Five) ski-plus-one 0))


(translate-lambda-to-ski 'SKI-Fact-Maker (template (fn [g#]
           (fn [n#] (SKI-If (SKI-Eq SKI-Null n#) SKI-One (SKI-Mul n# (g# (SKI-Minus n# SKI-One))))))))

((((SKI-Y SKI-Fact-Maker) SKI-Five) ski-plus-one 0))

(translate-lambda-to-ski 'SKI-Fact '(SKI-Y SKI-Fact-Maker))
(((SKI-Fact SKI-Five) ski-plus-one 0))


(def SKI-Y-Wiki (S (K (S I I)) (S (S (K S) K) (K (S I I)))))
(((SKI-Y-Wiki SKI-Fact-Maker SKI-Five) ski-plus-one 0))

(defn Fact-Maker-Lambda-Variadized-def [g n] (SKI-If (SKI-Eq SKI-Null n) SKI-One (SKI-Mul n (g (SKI-Minus n SKI-One)))))
(def Fact-Maker-Lambda-Variadized (variadize Fact-Maker-Lambda-Variadized-def ))

(((SKI-Y-Wiki Fact-Maker-Lambda-Variadized SKI-Five) ski-plus-one 0))


; Z-combinator
;Z = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))

(translate-lambda-to-ski 'SKI-Z (template (fn [f#]
                                            ( (fn [x#] (f# (fn [v#] ((x# x#) v#))))
                                              (fn[y#] (f# (fn [w#] ((y# y#) w#))))))))



(((SKI-Z SKI-Fact-Maker SKI-Five) ski-plus-one 0))
(((SKI-Z Fact-Maker-Lambda SKI-Five) ski-plus-one 0))



;;;;;;;;;;;;;;;     Lists     ;;;;;;;;;;;;;;;

(translate-lambda-to-ski  'SKI-Pair (template (fn [a# b#] (fn [f#] (f# a# b#)))))
(translate-lambda-to-ski  'SKI-First (template (fn [p#] (p# (fn [a# b#] a#)))))
(translate-lambda-to-ski  'SKI-Second (template (fn [p#] (p# (fn [a# b#] b#)))))

((SKI-First (SKI-Pair "first" "second")))
((SKI-Second (SKI-Pair "first" "second")))


(translate-lambda-to-ski 'SKI-Nil '(SKI-Pair SKI-True SKI-True))
(translate-lambda-to-ski 'SKI-IsNil (template (fn [l#] (SKI-First l#))))
(translate-lambda-to-ski 'SKI-Cons (template (fn [h# t#] (SKI-Pair SKI-False (SKI-Pair h# t#)))))

(translate-lambda-to-ski 'SKI-Head (template (fn [l#] (SKI-First (SKI-Second l#)))))
(translate-lambda-to-ski 'SKI-Tail (template (fn [l#] (SKI-Second (SKI-Second l#)))))


(translate-lambda-to-ski 'SKI-ConsZeroList (template (fn [n#] (n# (fn [l#] (SKI-Cons SKI-Null l#)) SKI-Nil))))

(translate-lambda-to-ski  'SKI-ConsRangeList (template (fn [n#]
  (SKI-Second (n# ( fn [p#] (SKI-Pair (SKI-Minus (SKI-First p#) SKI-One) (SKI-Cons (SKI-First p#) (SKI-Second p#)))) (SKI-Pair n# SKI-Nil))))))


(((SKI-First SKI-Nil) "true" "false"))
(((SKI-Second SKI-Nil) "true" "false"))
(((SKI-IsNil SKI-Nil) "true" "false"))



; the following functions convert SKI lists of Church numerals to lists of numbers

(defn ski-list-to-clj-list [f l] (( SKI-Y
     (fn [r]
       (fn [m]
         (SKI-If (SKI-IsNil m ) nil
              (conj [(f (SKI-Head m))] ( r (SKI-Tail m ))))))) l ))

(defn to-num-list-impl [expr, cur-lst]
  (let [lst (get-val expr)
        elem (first lst)]
     (if elem (to-num-list-impl (second lst) (cons elem cur-lst)) cur-lst)
  )
)

(defn to-num-list [ski-lst] (let [expr (ski-list-to-clj-list  (fn [x] (x ski-plus-one 0)) ski-lst)]
                           (map get-val (reverse (to-num-list-impl expr '())))))

(to-num-list SKI-Nil)

(translate-lambda-to-ski 'SKI-L1 '(SKI-Cons SKI-One (SKI-Cons SKI-Two (SKI-Cons SKI-Three SKI-Nil))))

(to-num-list SKI-L1)

(((SKI-IsNil SKI-L1) "true" "false"))
(((SKI-Head SKI-L1) ski-plus-one 0))


(translate-lambda-to-ski  'SKI-L2 '(SKI-ConsZeroList SKI-Three))

(to-num-list SKI-L2)
(to-num-list (SKI-ConsRangeList SKI-Ten))


(translate-lambda-to-ski 'SKI-Map (template (fn [f# l#]
  (( SKI-Y
     (fn [r#]
       (fn [m#]
         (SKI-If (SKI-IsNil m# ) SKI-Nil
              (SKI-Cons (f# (SKI-Head m#)) ( r# (SKI-Tail m# ))))))) l# ))))

(translate-lambda-to-ski 'SKI-Filter (template (fn [f# l#]
  (( SKI-Y
     (fn [r#]
       (fn [m#]
         (SKI-If (SKI-IsNil m#) SKI-Nil
                (SKI-If (f# (SKI-Head m#))
                     (SKI-Cons (SKI-Head m#) ( r# ( SKI-Tail m#) ))
                     ( r# (SKI-Tail m#) )))))) l#))))


(translate-lambda-to-ski 'SKI-Reduce (template (fn [f# z# l#]
  ((SKI-Y
    (fn [r#]
      (fn [m#]
        (SKI-If (SKI-IsNil m#) z#
             (f# (SKI-Head m#) (r# (SKI-Tail m#))))))) l#))))



(to-num-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) (SKI-ConsRangeList SKI-Ten)))
(to-num-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) (SKI-ConsRangeList SKI-Null)))
(to-num-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) SKI-Nil))
(to-num-list (SKI-Map (fn [x] (SKI-Plus x SKI-Hundred)) (SKI-ConsZeroList SKI-Ten)))


(to-num-list (SKI-Filter (fn [x] (SKI-Not (SKI-Eq x SKI-Five))) (SKI-ConsRangeList SKI-Ten)))
(to-num-list (SKI-Filter (fn [x] (SKI-Not (SKI-Eq x SKI-Five))) (SKI-ConsRangeList SKI-One)))

(translate-lambda-to-ski 'SKI-Reducer (template (fn [x# y#] (SKI-Plus x# y#))))


(((SKI-Reduce SKI-Reducer SKI-Null (SKI-ConsRangeList SKI-Ten)) ski-plus-one 0))
(((SKI-Reduce SKI-Reducer SKI-Null (SKI-ConsZeroList SKI-Ten)) ski-plus-one 0))



;;;;;;;;;;;;;;;     Y*-combinator     ;;;;;;;;;;;;;;;


(translate-lambda-to-ski 'SKI-Apply (template (fn [f# l#] (SKI-Reduce (fn [x# y#] (y# x#)) f# l#))))
(translate-lambda-to-ski 'SKI-Test-Sum (template (fn (f# s# t#) (SKI-Plus f#  (SKI-Plus s# t#)))))

(((((SKI-Test-Sum SKI-One) SKI-Two) SKI-Three) ski-plus-one 0))
(((SKI-Apply SKI-Test-Sum SKI-L1)  ski-plus-one 0))


(translate-lambda-to-ski 'SKI-Second-Elem  (template (fn [l#] (SKI-Head (SKI-Tail l#)))))
(translate-lambda-to-ski 'SKI-Make-Even?  (template (fn [e-and-o#] (fn [n#] (SKI-Or (SKI-Eq SKI-Null n#) ((SKI-Second-Elem e-and-o#) (SKI-Minus n# SKI-One)))))))
(translate-lambda-to-ski 'SKI-Make-Odd?  (template (fn [e-and-o#] (fn [n#] (SKI-And (SKI-Not (SKI-Eq SKI-Null n#)) ((SKI-Head e-and-o#) (SKI-Minus n# SKI-One)))))))
(translate-lambda-to-ski 'SKI-Even-Odd '(SKI-Cons SKI-Make-Even? (SKI-Cons SKI-Make-Odd? SKI-Nil)))


(translate-lambda-to-ski 'SKI-Y* (template (fn [fs#]
    ((fn [x#] (x# x#))
      (fn [p#]
        (SKI-Map
          (fn [f#]
              (f#
                (SKI-Map
                  (fn [ff#]
                    (fn [ y#] ( ff# y#)))
                  (p# p#)
                 )))
           fs#
          ))))))


((((SKI-Head (SKI-Y* (SKI-Cons SKI-Make-Even? (SKI-Cons SKI-Make-Odd? SKI-Nil)))) SKI-Four) "true" "false"))
((((SKI-Second-Elem (SKI-Y* (SKI-Cons SKI-Make-Even? (SKI-Cons SKI-Make-Odd? SKI-Nil)))) SKI-Four) "true" "false"))

((((SKI-Head (SKI-Y* SKI-Even-Odd)) SKI-Four) "true" "false"))
((((SKI-Second-Elem (SKI-Y* SKI-Even-Odd)) SKI-Four) "true" "false"))


(translate-lambda-to-ski 'SKI-Even? '(SKI-Head (SKI-Y* SKI-Even-Odd)))
(translate-lambda-to-ski 'SKI-Odd? '(SKI-Second-Elem (SKI-Y* SKI-Even-Odd)))

(((SKI-Even? SKI-Five) "true" "false"))
(((SKI-Odd? SKI-Five) "true" "false"))

;;;;;;;;;;;;;;;     print all sources and all translations     ;;;;;;;;;;;;;;;

(clojure.pprint/pprint sources)
(clojure.pprint/pprint ski-translations)


;;;BLOG example - remove me
(defn sum-def [x y] (+ (get-val x) (get-val y)))
(def sum (variadize sum-def))
(translate-lambda-to-ski 'SKI-Sum-translated (template (fn [x#] ((fn [y#] (sum x# y#)) x#) )))
(def SKI-Sum (S sum I))
((SKI-Sum 10))
((SKI-Sum-translated 10))

(defn add5 [x] (sum 5 (get-val x)))
(def SKI-Add5 (S (K add5) I))
((SKI-Add5 9))

