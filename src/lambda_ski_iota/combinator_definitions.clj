(ns lambda-ski-iota.combinator-definitions)
(use 'clojure.test)
(use 'clojure.tools.trace)



(defn gen-invokes-with-args [f]
  (for [arity (range 1 21),
        :let [args (repeatedly arity gensym)]]
    `(~'invoke [~'this ~@args] ((~'force ~f) ~@args))))


(defn gen-invoke-no-args [f]
  `(~'invoke [~'this] (~'let [exp# (~'force ~f)] (~'if (~'instance? ~'DelayedEval exp#) (exp#) exp#))))


(defn gen-apply-to [f]
  `(~'applyTo [~'this args#] (apply (~'force ~f) args#)))

(defn extend-IFn [f]
  `(clojure.lang.IFn
     ~(gen-invoke-no-args f)
     ~@(gen-invokes-with-args f)
     ~(gen-apply-to f)
     ))

(defmacro defDelayedEval []
  (let [delayed-exp (gensym)]
    `(~'defrecord ~'DelayedEval [~delayed-exp]
       ~@(extend-IFn delayed-exp))
    ))


(defDelayedEval)

; a less fancy way to generate DelayedEval
;(defrecord DelayedEval [delayed-exp]
;  clojure.lang.IFn
;  (invoke [this] (let [exp (force delayed-exp)] (if ( instance? DelayedEval exp) (exp) exp)))

;  (invoke [this arg] ((force delayed-exp) arg))
;  (invoke [this arg arg2] ((force delayed-exp) arg arg2))
;  (invoke [this arg arg2 arg3] ((force delayed-exp) arg arg2 arg3))
;  (invoke [this arg arg2 arg3 arg4] ((force delayed-exp) arg arg2 arg3 arg4))
;  (applyTo [this args] (apply (force delayed-exp) args))
;)


; extracts value from a DelayedEval expression
(defn get-val [delayed-val] (if (instance? DelayedEval delayed-val) (delayed-val) delayed-val))


; finds out how many arguments the function 'f' has
;http://stackoverflow.com/questions/1696693/clojure-how-to-find-out-the-arity-of-function-at-runtime
(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))



(defn variadize-strict
      ([fnct] (variadize-strict fnct (arg-count fnct)))
  ([fnct num-args]
   (if (= num-args 0) (fnct)
     (fn [& args]
       (cond
         (> (count args) num-args) (apply (apply fnct (take num-args args)) (drop num-args args))
         (= (count args) num-args) (apply fnct args)
         :else (variadize-strict (reduce (fn [f x] (partial f x)) fnct args) (- num-args (count args)))
         )
       )
     ))
  )



(defn variadize
      ([fnct] (variadize fnct (arg-count fnct)))
  ([fnct num-args]
   (if (= num-args 0) (DelayedEval. (delay (fnct)))
     (fn [& args]
       (cond
         (> (count args) num-args)
             (DelayedEval. (delay (apply (apply fnct (take num-args args)) (drop num-args args))))
         (= (count args) num-args) (DelayedEval. (delay (apply fnct args)))
         :else (variadize (reduce (fn [f x] (partial f x)) fnct args) (- num-args (count args)))
         )
       )
     ))
  )



(defn S-def [f g x] (f x (g x)))
(def S (variadize S-def))
;(def S (variadize-strict S-def))


(defn K-def [x y] x)
(def K (variadize K-def))
;(def K (variadize-strict K-def))


(defn I-def [x] x)
(def I (variadize I-def))
;(def I (variadize-strict I-def))
; an alternative definition
;(def I (S K K))


(defn X-def [f] (f S K))
(def X (variadize X-def))
;(def X (variadize-strict X-def))


