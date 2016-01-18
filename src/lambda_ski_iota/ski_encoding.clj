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


(defn to-bool [ski-bool-expr] ((ski-bool-expr true false)))

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

(to-bool SKI-True)


; alternative definitions
;(def False (S K))
;(def False (K I))


(to-bool SKI-False)

((SKI-If SKI-True "first" "second"))
(((SKI-If SKI-True) "first" "second"))

((SKI-If SKI-False "first"  "second"))

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


(defn to-int [ski-int-expr] ((ski-int-expr ski-plus-one 0)))

(translate-lambda-to-ski 'SKI-Zero (template (fn [f# x#] x#)))
(translate-lambda-to-ski 'SKI-Succ (template (fn [n# f# x#] (f# (n# f# x#)))))


(translate-lambda-to-ski 'SKI-One '(SKI-Succ SKI-Zero))
(translate-lambda-to-ski 'SKI-Two '(SKI-Succ SKI-One))
(translate-lambda-to-ski 'SKI-Three '(SKI-Succ SKI-Two))
(translate-lambda-to-ski 'SKI-Four '(SKI-Succ SKI-Three))
(translate-lambda-to-ski 'SKI-Five '(SKI-Succ SKI-Four))
(translate-lambda-to-ski 'SKI-Six '(SKI-Succ SKI-Five))
(translate-lambda-to-ski 'SKI-Seven '(SKI-Succ SKI-Six))


(((SKI-One ski-plus-one) 0))
((SKI-Five ski-plus-one 0))
(((SKI-Five ski-plus-one) 0))

(to-int SKI-Five)

(translate-lambda-to-ski 'SKI-Pred  (template (fn [n# f# x#] ((n# (fn [g#] (fn [h#] (h# (g# f#)))) (fn [u#] x#)) (fn [w#] w#)))))


(translate-lambda-to-ski 'SKI-AnotherFour '(SKI-Pred SKI-Five))

(to-int SKI-AnotherFour)
(translate-lambda-to-ski 'SKI-Plus (template (fn [m# n#] (fn [f# x#] (m# f# (n# f# x#))))))

(to-int (SKI-Plus SKI-Five SKI-Three))

(translate-lambda-to-ski 'SKI-Minus (template (fn [m# n#] (fn [f# x#] ((n# (fn [t#] (fn [r# s#] (SKI-Pred t# r# s#))) m#) f#  x#)))))

; An alternative definition of 'Minus'
(translate-lambda-to-ski 'SKI-Minus2 (template (fn [m# n#] ((n# SKI-Pred) m#))))


(to-int (SKI-Minus SKI-Five SKI-Three))
(to-int (SKI-Minus SKI-Three SKI-Two))

(to-int (SKI-Minus2 SKI-Five SKI-Three))
(to-int (SKI-Minus2 SKI-Three SKI-Two))

(translate-lambda-to-ski 'SKI-Mul (template (fn [m# n#] (fn [f# x#] (m# (n# f#) x#)))))
(translate-lambda-to-ski 'SKI-Zero? (template (fn [n#] (n# (fn [x#] SKI-False) SKI-True))))


; m <= n
(translate-lambda-to-ski 'SKI-Leq? (template (fn [m# n#] (SKI-Zero? (SKI-Minus m# n#)))))

; m = n
(translate-lambda-to-ski 'SKI-Eq?  (template (fn [m# n#] (SKI-And (SKI-Leq? m# n#) (SKI-Leq? n# m#)))))

; m < n
(translate-lambda-to-ski 'SKI-Lt? (template (fn [m# n#] (SKI-And (SKI-Leq? m# n#) (SKI-Not (SKI-Eq?  n# m#))))))

(to-bool (SKI-Leq? SKI-Five SKI-Two))
(to-bool (SKI-Leq? SKI-Two SKI-Five))
(to-bool (SKI-Leq? SKI-Five SKI-Five))

(to-bool (SKI-Eq?  SKI-Two SKI-Two))
(to-bool (SKI-Eq?  SKI-Two SKI-One))
(to-bool (SKI-Eq?  SKI-One SKI-Two))


(to-bool (SKI-Lt? SKI-Five SKI-Two))
(to-bool (SKI-Lt? SKI-Two SKI-Five))
(to-bool (SKI-Lt? SKI-Five SKI-Five))

(to-int (SKI-Mul SKI-Five SKI-Two))

(to-bool (SKI-Zero? SKI-One))
(to-bool (SKI-Zero? SKI-Zero))
(to-bool (SKI-Zero? (SKI-Minus SKI-Three SKI-Two)))
(to-bool (SKI-Zero? (SKI-Minus SKI-Two SKI-Two)))

(translate-lambda-to-ski 'SKI-Ten '(SKI-Mul SKI-Five SKI-Two))
(translate-lambda-to-ski 'SKI-Eleven '(SKI-Succ SKI-Ten))
(translate-lambda-to-ski 'SKI-Hundred '(SKI-Mul SKI-Ten SKI-Ten))

((SKI-Hundred ski-concat ""))

(to-int (SKI-Plus SKI-Two (SKI-Mul SKI-Four SKI-Ten)))

(translate-lambda-to-ski 'SKI-FortyTwo '(SKI-Minus (SKI-Mul SKI-Seven SKI-Seven) SKI-Seven))


(to-int SKI-FortyTwo)


;;;;;;;;;;;;;;;     Y-combinator     ;;;;;;;;;;;;;;;

(translate-lambda-to-ski 'SKI-Y (template (fn [f#] ((fn [g#] (g# g#)) (fn [h#] (fn [n#] ((f# (h# h#)) n#)))))))

(def Fact-Maker-Lambda (fn [g]
           (fn [n] (SKI-If (SKI-Eq?  SKI-Zero n) SKI-One (SKI-Mul n (g (SKI-Minus n SKI-One)))))))

(to-int ((SKI-Y Fact-Maker-Lambda) SKI-Five))
(to-int (SKI-Y Fact-Maker-Lambda SKI-Five))


(translate-lambda-to-ski 'SKI-Fact-Maker (template (fn [g#]
           (fn [n#] (SKI-If (SKI-Eq?  SKI-Zero n#) SKI-One (SKI-Mul n# (g# (SKI-Minus n# SKI-One))))))))


(translate-lambda-to-ski 'SKI-Fact '(SKI-Y SKI-Fact-Maker))
(to-int (SKI-Fact SKI-Five))


(def SKI-Y-Wiki (S (K (S I I)) (S (S (K S) K) (K (S I I)))))
(to-int (SKI-Y-Wiki SKI-Fact-Maker SKI-Five))


(defn Fact-Maker-Lambda-Variadized-def [g n] (SKI-If (SKI-Eq?  SKI-Zero n) SKI-One (SKI-Mul n (g (SKI-Minus n SKI-One)))))
(def Fact-Maker-Lambda-Variadized (variadize Fact-Maker-Lambda-Variadized-def ))

(to-int (SKI-Y-Wiki Fact-Maker-Lambda-Variadized SKI-Five))


; Z-combinator
; Z = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))

(translate-lambda-to-ski 'SKI-Z (template (fn [f#]
                                            ( (fn [x#] (f# (fn [v#] ((x# x#) v#))))
                                              (fn[y#] (f# (fn [w#] ((y# y#) w#))))))))



(to-int (SKI-Z SKI-Fact-Maker SKI-Five))
(to-int (SKI-Z Fact-Maker-Lambda SKI-Five))


; McCarthy 91 function
(translate-lambda-to-ski 'SKI-McCarthy-Maker (template (fn [f#]
           (fn [n#] (SKI-If (SKI-Leq? n# SKI-Hundred) (f# (f# (SKI-Plus n# SKI-Eleven))) (SKI-Minus n# SKI-Ten) )))))

(translate-lambda-to-ski 'SKI-McCarthy '(SKI-Y SKI-McCarthy-Maker))


(to-int (SKI-McCarthy SKI-One))
;(to-int (SKI-McCarthy SKI-Eleven))
;(to-int (SKI-McCarthy SKI-Hundred))
;(to-int (SKI-McCarthy (SKI-Succ SKI-Hundred)))
;(to-int (SKI-McCarthy (SKI-Succ (SKI-Succ SKI-Hundred))))

;;;;;;;;;;;;;;;     Lists     ;;;;;;;;;;;;;;;

(translate-lambda-to-ski  'SKI-Pair (template (fn [a# b#] (fn [f#] (f# a# b#)))))
(translate-lambda-to-ski  'SKI-First (template (fn [p#] (p# (fn [a# b#] a#)))))
(translate-lambda-to-ski  'SKI-Second (template (fn [p#] (p# (fn [a# b#] b#)))))

((SKI-First (SKI-Pair "first" "second")))
((SKI-Second (SKI-Pair "first" "second")))


(translate-lambda-to-ski 'SKI-Nil '(SKI-Pair SKI-True SKI-True))
(translate-lambda-to-ski 'SKI-Nil? (template (fn [l#] (SKI-First l#))))
(translate-lambda-to-ski 'SKI-Cons (template (fn [h# t#] (SKI-Pair SKI-False (SKI-Pair h# t#)))))

(translate-lambda-to-ski 'SKI-Head (template (fn [l#] (SKI-First (SKI-Second l#)))))
(translate-lambda-to-ski 'SKI-Tail (template (fn [l#] (SKI-Second (SKI-Second l#)))))
(translate-lambda-to-ski 'SKI-Second-Elem  (template (fn [l#] (SKI-Head (SKI-Tail l#)))))

(translate-lambda-to-ski 'SKI-ConsZeroList (template (fn [n#] (n# (fn [l#] (SKI-Cons SKI-Zero l#)) SKI-Nil))))

(translate-lambda-to-ski  'SKI-ConsRangeList (template (fn [n#]
  (SKI-Second (n# ( fn [p#] (SKI-Pair (SKI-Minus (SKI-First p#) SKI-One) (SKI-Cons (SKI-First p#) (SKI-Second p#)))) (SKI-Pair n# SKI-Nil))))))


(to-bool (SKI-First SKI-Nil))
(to-bool (SKI-Second SKI-Nil))
(to-bool (SKI-Nil? SKI-Nil))

; the following functions convert SKI lists of Church numerals to lists of numbers

(defn ski-list-to-clj-list [f l] (( SKI-Y
     (fn [r]
       (fn [m]
         (SKI-If (SKI-Nil? m ) nil
              (conj [(f (SKI-Head m))] ( r (SKI-Tail m ))))))) l ))

(defn to-int-list-impl [expr, cur-lst]
  (let [lst (get-val expr)
        elem (first lst)]
     (if elem (to-int-list-impl (second lst) (cons elem cur-lst)) cur-lst)
  )
)

(defn to-int-list [ski-lst] (let [expr (ski-list-to-clj-list  (fn [x] (x ski-plus-one 0)) ski-lst)]
                           (map get-val (reverse (to-int-list-impl expr '())))))

(to-int-list SKI-Nil)

(translate-lambda-to-ski 'SKI-L1 '(SKI-Cons SKI-One (SKI-Cons SKI-Two (SKI-Cons SKI-Three SKI-Nil))))

(to-int-list SKI-L1)

(to-bool (SKI-Nil? SKI-L1))

(to-int (SKI-Head SKI-L1))


(translate-lambda-to-ski  'SKI-L2 '(SKI-ConsZeroList SKI-Three))

(to-int-list SKI-L2)
(to-int-list (SKI-ConsRangeList SKI-Ten))


(translate-lambda-to-ski 'SKI-Map (template (fn [f# l#]
  (( SKI-Y
     (fn [r#]
       (fn [m#]
         (SKI-If (SKI-Nil? m# ) SKI-Nil
              (SKI-Cons (f# (SKI-Head m#)) ( r# (SKI-Tail m# ))))))) l# ))))

(translate-lambda-to-ski 'SKI-Filter (template (fn [f# l#]
  (( SKI-Y
     (fn [r#]
       (fn [m#]
         (SKI-If (SKI-Nil? m#) SKI-Nil
                (SKI-If (f# (SKI-Head m#))
                     (SKI-Cons (SKI-Head m#) ( r# ( SKI-Tail m#) ))
                     ( r# (SKI-Tail m#) )))))) l#))))


;fold right
(translate-lambda-to-ski 'SKI-Reduce (template (fn [f# z# l#]
  ((SKI-Y
    (fn [r#]
      (fn [m#]
        (SKI-If (SKI-Nil? m#) z#
             (f# (SKI-Head m#) (r# (SKI-Tail m#))))))) l#))))

;fold left
(translate-lambda-to-ski 'SKI-ReduceL (template (fn [f# z# l#]
  ((SKI-Y
    (fn [r#]
      (fn [m# a#]
        (SKI-If (SKI-Nil? m#) a#
             (r#  (SKI-Tail m#) (f# (SKI-Head m#) a#) ))))) l# z#))))



; fold left by using fold right
; see  http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
(translate-lambda-to-ski 'SKI-ReduceL2 (template (fn [f# z# l#] (SKI-Reduce (fn [b# g# a#] (g# (f# a# b#))) I l# z#) )))

;;;;;;;;;;;;;;;    Examples of using SKI-Map and SKI-Filter    ;;;;;;;;;;;;;;;

(to-int-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) (SKI-ConsRangeList SKI-Ten)))
(to-int-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) (SKI-ConsRangeList SKI-Zero)))
(to-int-list (SKI-Map (fn [x] (SKI-Plus x SKI-One)) SKI-Nil))
(to-int-list (SKI-Map (fn [x] (SKI-Plus x SKI-Hundred)) (SKI-ConsZeroList SKI-Ten)))


(to-int-list (SKI-Filter (fn [x] (SKI-Not (SKI-Eq?  x SKI-Five))) (SKI-ConsRangeList SKI-Ten)))
(to-int-list (SKI-Filter (fn [x] (SKI-Not (SKI-Eq?  x SKI-Five))) (SKI-ConsRangeList SKI-One)))


;;;;;;;;;;;;;;;    Examples of using SKI-Reduce    ;;;;;;;;;;;;;;;

(translate-lambda-to-ski 'SKI-Reducer-Ex1 (template (fn [x# y#] (SKI-Plus x# y#))))


(to-int (SKI-ReduceL SKI-Reducer-Ex1 SKI-Zero (SKI-ConsRangeList SKI-Ten)))
(to-int (SKI-Reduce SKI-Reducer-Ex1 SKI-Zero (SKI-ConsZeroList SKI-Ten)))
(to-int (SKI-ReduceL2 SKI-Reducer-Ex1 SKI-Zero (SKI-ConsRangeList SKI-Ten)))

;;;;;;;;;;;;;;; Some interesting lambda functions ;;;;;;;;;;;;;;;

; http://jwodder.freeshell.org/lambda.html

; Implementation of Concat function for two lists using SKI-Reduce
(translate-lambda-to-ski 'SKI-L3 '(SKI-Cons SKI-Four (SKI-Cons SKI-Five (SKI-Cons SKI-Six SKI-Nil))))
(translate-lambda-to-ski 'SKI-Reducer-Concat (template (fn [x# y#] (SKI-Cons x# y#))))
;appends two lists - notice that the lists should be passed to SKI-Reduce in the reversed order - SKI-Reduce is foldr
(translate-lambda-to-ski 'SKI-Concat (template (fn [l1# l2#] (SKI-Reduce SKI-Reducer-Concat l2# l1#))))
(to-int-list (SKI-Concat SKI-L1 SKI-L3))
(to-int-list (SKI-Concat SKI-L1 (SKI-Cons (SKI-Plus SKI-One SKI-One) SKI-Nil)))

; Implementation of Length function for lists using SKI-Reduce
(translate-lambda-to-ski 'SKI-Reducer-Length (template (fn [x# y#] (SKI-Plus SKI-One y#))))
(translate-lambda-to-ski 'SKI-Length (template (fn [l#] (SKI-Reduce SKI-Reducer-Length SKI-Zero l#))))


(to-int (SKI-Length SKI-L1))

; Implementation of Map using SKI-Reduce
(translate-lambda-to-ski 'SKI-Reducer-Map (template (fn [f# x# y#] (SKI-Cons (f# x#) y#))))
(translate-lambda-to-ski 'SKI-Map2 (template (fn [f# l#] (SKI-Reduce (SKI-Reducer-Map f#) SKI-Nil l#))))
(translate-lambda-to-ski 'SKI-Mapper-Ex1 (template (fn [x#] (SKI-Plus x# SKI-One))))
(to-int-list (SKI-Map2 SKI-Mapper-Ex1 (SKI-ConsRangeList SKI-Ten)))

; Implementation of Map using SKI-ReduceL
(translate-lambda-to-ski 'SKI-Reducer-MapL (template (fn [f# x# y#] (SKI-Concat y# (SKI-Cons (f# x#) SKI-Nil)))))
(translate-lambda-to-ski 'SKI-Map2L (template (fn [f# l#] (SKI-ReduceL (SKI-Reducer-MapL f#) SKI-Nil l#))))
(to-int-list (SKI-Map2L SKI-Mapper-Ex1 (SKI-ConsRangeList SKI-Ten)))


; Implementation of Filter using SKI-Reduce
(translate-lambda-to-ski 'SKI-Reducer-Filter (template (fn [f# x# y#] (SKI-If (f# x#) (SKI-Cons x# y#) y#))))
(translate-lambda-to-ski 'SKI-Filter2 (template (fn [f# l#] (SKI-Reduce (SKI-Reducer-Filter f#) SKI-Nil l#))))
(translate-lambda-to-ski 'SKI-Filter-Ex1 (template (fn [x#] (SKI-Not (SKI-Eq?  x# SKI-Five)))))
(to-int-list (SKI-Filter2 SKI-Filter-Ex1 (SKI-ConsRangeList SKI-Ten)))

; QuickSort
;(defn quicksort-maker [f]  (fn [x]  (if (empty? x) '() (concat (f (filter (fn [y1] (< y1 (first x))) x))
;                                                                  (concat (filter (fn [y2] (= y2 (first x))) x)
;                                                                           (f (filter (fn [y3] (> y3 (first x))) x))) ))))


(translate-lambda-to-ski 'SKI-QuickSort-Maker (template  (fn [f#] (fn [x#]  (SKI-If (SKI-Nil? x#) SKI-Nil (SKI-Concat (f# (SKI-Filter (fn [y1#] (SKI-Lt? y1# (SKI-Head x#))) x#))
                                                                  (SKI-Concat (SKI-Filter (fn [y2#] (SKI-Eq?  y2# (SKI-Head x#))) x#)
                                                                           (f# (SKI-Filter (fn [y3#] (SKI-Lt?(SKI-Head x#) y3#)) x#))) ))))))

(translate-lambda-to-ski 'SKI-L-Not-Sorted '(SKI-Cons SKI-Three (SKI-Cons SKI-One (SKI-Cons SKI-Two SKI-Nil))))
(translate-lambda-to-ski 'SKI-QuickSort '(SKI-Y SKI-QuickSort-Maker))
(to-int-list (SKI-QuickSort SKI-L-Not-Sorted))



; Ackermann

;(defn ackermann-maker [a] (fn [x] (if (= (first x) 0) (inc (second x))
;                                       (if (= (second x) 0)
;                                             (a (cons (dec (first x)) (cons 1 nil)))
;                                             (a (cons (dec (first x)) (cons (a (cons (first x) (cons (dec (second x)) nil))) nil)))
;                                         ))))


(translate-lambda-to-ski 'SKI-Ackermann-Maker  (template (fn [a#] ( fn[x#] (SKI-If (SKI-Zero? (SKI-Head x#))  (SKI-Succ (SKI-Second-Elem x#))
                                                                                                              (SKI-If (SKI-Zero? (SKI-Second-Elem x#))
                                                                                                                    (a# (SKI-Cons (SKI-Pred (SKI-Head x#)) (SKI-Cons SKI-One SKI-Nil)))
                                                                                                                    (a# (SKI-Cons (SKI-Pred (SKI-Head x#)) (SKI-Cons (a# (SKI-Cons (SKI-Head x#) (SKI-Cons (SKI-Pred (SKI-Second-Elem x#)) SKI-Nil))) SKI-Nil )))
                                                                                                                      ))))))
(translate-lambda-to-ski 'SKI-Ackermann '(SKI-Y SKI-Ackermann-Maker))

(to-int (SKI-Ackermann (SKI-Cons SKI-Three (SKI-Cons SKI-Four SKI-Nil))))


;;;;;;;;;;;;;;;     Y*-combinator     ;;;;;;;;;;;;;;;

; http://stackoverflow.com/questions/4899113/fixed-point-combinator-for-mutually-recursive-functions
; an implementation of Apply using reduce - we apply one argument at a time
; Ski-Test-Sum sums three elements. We use Apply to apply this function to a list containing 3 elements
(translate-lambda-to-ski 'SKI-Apply (template (fn [f# l#] (SKI-Reduce (fn [x# y#] (y# x#)) f# l#))))
(translate-lambda-to-ski 'SKI-Test-Sum (template (fn (f# s# t#) (SKI-Plus f#  (SKI-Plus s# t#)))))


(to-int (((SKI-Test-Sum SKI-One) SKI-Two) SKI-Three))
(to-int (SKI-Apply SKI-Test-Sum SKI-L1))


(translate-lambda-to-ski 'SKI-Make-Even?  (template (fn [e-and-o#] (fn [n#] (SKI-Or (SKI-Eq?  SKI-Zero n#) ((SKI-Second-Elem e-and-o#) (SKI-Minus n# SKI-One)))))))
(translate-lambda-to-ski 'SKI-Make-Odd?  (template (fn [e-and-o#] (fn [n#] (SKI-And (SKI-Not (SKI-Eq?  SKI-Zero n#)) ((SKI-Head e-and-o#) (SKI-Minus n# SKI-One)))))))
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

(to-bool (SKI-Even? SKI-Five))
(to-bool (SKI-Odd? SKI-Five))


; Sieve of Eratosthenes
(translate-lambda-to-ski 'SKI-Mod-Impl-Maker (template  (fn [f#] (fn [p#] (SKI-If (SKI-Lt? (SKI-Head p#) (SKI-Second-Elem p#)) (SKI-Head p#) (f# (SKI-Cons (SKI-Minus (SKI-Head p#) (SKI-Second-Elem p#)) (SKI-Cons (SKI-Second-Elem p#) SKI-Nil ))))))))
(translate-lambda-to-ski 'SKI-Mod-Impl '(SKI-Y SKI-Mod-Impl-Maker))
(translate-lambda-to-ski 'SKI-Mod (template (fn [x# y#] (SKI-Mod-Impl (SKI-Cons x# (SKI-Cons y# SKI-Nil))  ) )))

(to-int (SKI-Mod SKI-Seven SKI-Five))
(to-int (SKI-Mod SKI-Five SKI-Seven))
(to-int (SKI-Mod SKI-Five SKI-Five))


;(defn eratosthenes-sieve-maker [f] (fn [l] (if (empty? l) '()  (cons (first l) (f (filter (fn [x] (not (= 0 (mod x (first l))))) l))))))

(translate-lambda-to-ski 'SKI-Eratosthenes-Sieve-Maker (template (fn [f#] (fn [l#] (SKI-If (SKI-Nil? l#) SKI-Nil (SKI-Cons (SKI-Head l#) (f# (SKI-Filter (fn [x#] (SKI-Not (SKI-Eq?  SKI-Zero (SKI-Mod x# (SKI-Head l#)))))l#))))))))
(translate-lambda-to-ski 'SKI-Eratosthenes-Sieve '(SKI-Y SKI-Eratosthenes-Sieve-Maker))
(to-int-list (SKI-Eratosthenes-Sieve (SKI-Tail (SKI-ConsRangeList SKI-Ten))))


;;;;;;;;;;;;;;;     print all sources and all translations     ;;;;;;;;;;;;;;;

;(clojure.pprint/pprint sources)
;(clojure.pprint/pprint ski-translations)


;;;;;;;;;;;;;;;     Blog post examples     ;;;;;;;;;;;;;;;
(defn sum-def [x y] (+ (get-val x) (get-val y)))
(def sum (variadize sum-def))

(def SKI-Double (S sum I))
((SKI-Double 10))

;(translate-lambda-to-ski 'SKI-Double-Translated (template (fn [z#] ((fn [v#] (sum z# v#)) z#) )))

(translate-lambda-to-ski 'SKI-Double-Translated (template (fn [z#] ((fn [v#] (sum z# v#)) ((fn [y#] y#) z#)))))

((SKI-Double-Translated 10))
(get @ski-translations 'SKI-Double-Translated)
;(((S (S (S (K S) (S (K K) (S (K sum) I))) (K I)) I) 10))

(((S (S (S (K S) (S (K K) (S (K sum) I))) (K I)) (S (K I) I)) 10))

(defn add5-def [x] (sum 5 (get-val x)))
(def add5 (variadize add5-def))
(def SKI-Add5 (S (K add5) I))
((SKI-Add5 10))

;(translate-lambda-to-ski 'SKI-Add5-Translated (template (fn [x#] ((fn [y#] (add5 y#)) x#) )))
(translate-lambda-to-ski 'SKI-Add5-Translated (template (fn [z#] ((fn [v#] (add5 v#)) ((fn [y#] y# ) z#)))))

((SKI-Add5-Translated 10))
(get @ski-translations 'SKI-Add5-Translated)
(((S (K (S (K add5) I)) I) 10))
(((S (K (S (K add5) I)) (S (K I) I)) 10))

(get @sources 'SKI-Minus2)
(get @expanded-sources 'SKI-Minus2)

(to-int (SKI-Minus2 SKI-Five SKI-Three))
(to-bool (SKI-Or SKI-True SKI-False))
(to-int (SKI-Minus SKI-Two SKI-Two))
(to-int-list (SKI-Cons SKI-One (SKI-Cons SKI-Two (SKI-Cons SKI-Three SKI-Nil))))

