(ns lambda-ski-iota.lambda-to-ski-translator)
(use 'lambda-ski-iota.combinator-definitions)
(use 'clojure.set)
;for debugging
(use 'clojure.tools.trace)

; forward declarations
(declare translate-function)
(declare translate-application)
(declare translate-term)

; This is the entry point function. It finds out what it was given as its argument
; and calls an appropriate function to translate.
(defn translate-dispatch [exp]
  (cond (list? exp)
        (let [[f & rst] exp]
          (if (= 'fn f)
            (translate-function exp)
            (translate-application exp)
            ))
    :else (translate-term exp)
    )
  )

; Not much to do to translate a term
(defn translate-term [t] t)


; This function translates the function denfinition.
; It get a function as an input and returns a transaltion
; of it into  SKI-calculus.
; The function to be transalted can depend on several parameters,
; in which case the call will take care of the first argument
; and recursively call translation for the rest of the arguments

(declare abstract-dispatch)
(defn translate-function [f]
  (let [[fn-sym [x & rst-args] body] f]
    (cond
      (= (count rst-args) 0) (abstract-dispatch x (translate-dispatch body))
      :else (abstract-dispatch x (translate-dispatch (list fn-sym rst-args body)))
      )
    )
  )

; This functions deals with expressions of type
; (E1 E2 E3 ...). Several cases are possible, '->' means "is translated to":
; 1. (E1) -> (E1)
; 2. (E1 E2) -> (E1 E2) (this is our base case; expressions with more elements are reduced to this case)
; 3. (E1 E2 E3) -> ((E1 E2) E3) (and apply point 2. twice)
; 4. (E1 E2 E3 E4) -> ((E1 E2 E3) E4) -> (((E1 E2) E3) E4) (and apply point 2. thrice)
; 5. (E1 E2 E3 E4 ...) (similar to the above)
(defn translate-application [exp]
  (if (coll? exp)
    (cond
      (= (count exp) 0) (throw (Exception. "should not be an empty list"))
      (= (count exp) 1) (list (translate-dispatch (first exp)))
      (= (count exp) 2) (list (translate-dispatch (first exp)) (translate-dispatch (last exp))) ; we don't want to convert (E1 E2) to ((E1) E2) ...
      :else (translate-application (list (apply list (butlast exp)) (last exp)))) ; (apply list ...) converts seq back to list
    (throw (Exception. "should be an application"))
    )
  )


; This is function determines if var 'x' is present in 'expr'. 'expr' can have a nsted structure
(defn contains-var? [x exp]
  (if (list? exp)
    (true? (some #(= x %) (flatten exp))) ; we want the function to return 'true' or 'false'. 'some' returns 'nil' if no matching element is found.
    (= x exp)
    )
  )

; This function takes a var 'x' and "pushes it out" of the expression 'expr', i.e. it removes all instances of 'x' and replaces them with
; the appropriate combinators.
(defn abstract-dispatch [x exp]
  (if (contains-var? x exp)
    (if (coll? exp)
      (cond
        (= (count exp) 0) (throw (Exception. "should not be an empty list"))
        (= (count exp) 1) (throw (Exception. "not supported")) ;(abstract-dispatch x (first exp))
        (= (count exp) 2) (list 'S (abstract-dispatch x (first exp)) (abstract-dispatch x (last exp)))
        :else (abstract-dispatch x (list (apply list (butlast exp)) (last exp)))) ; just convert to (E1 E2) case and call the function again
      ;else
      (if (= x exp)
        'I
        (list 'K exp) ; this is redundant as it should be taken care of by the external if
        )
      )
    ;else
    (list 'K exp)
    )
  )


;;;;;;;;;;;;;;;     Support for substitutions     ;;;;;;;;;;;;;;;

; This atom stores all definitions of functions that were translated.
(def ^{:doc "Atom that contains all sources."}
sources
  (atom {}))

; This atom stores all expanded definitions of functions that were translated.
(def ^{:doc "Atom that contains all expanded sources."}
expanded-sources
  (atom {}))

; This atom stores all SKI translations.
(def ^{:doc "Atom that contains all translations."}
ski-translations
  (atom {}))

(defn add-translation [dest func-name func-def]
  (swap! dest assoc func-name func-def)
  )

; This function finds all function names in the 'func-def' that were previously translated.
(defn get-symbols-to-substitute [translation-map func-def]
  (clojure.set/intersection (set (flatten func-def)) (set (keys translation-map))))

; Substitues the function names in the body of the  'func-def' with their definitions from 'translation map'. We use recursion
; to make sure that all definitions are recursively expanded.
(defn substitute-translations [translation-map func-def]
  (if-let [symbols-to-substitute (seq (get-symbols-to-substitute translation-map func-def))]
    (recur translation-map (read-string (reduce (fn [accum next-val]
                                                  (clojure.string/replace accum
                                                                          ; We need to make sure that the identifier (first next-val) is not a part of another identifier
                                                                          ; i.e. we should not replace 'abcd if our identifier is 'bc.
                                                                          ; We are doing that ensuring that the character before and after the identifier cannot belong
                                                                          ; to an identifier.
                                                                          (re-pattern (str "(?<![\\w|\\-|\\?|\\*])"
                                                                                           ; we need to escape the allowed '*' and '?' characters so that they don't interfere
                                                                                           ; with the regular impression
                                                                                           (clojure.string/replace (clojure.string/replace (first next-val) "*" "\\*") "?" "\\?")
                                                                                           "(?![\\w|\\-|\\?|\\*])"))
                                                                          (str (last next-val)))) (str func-def) (select-keys translation-map symbols-to-substitute))))
    func-def
    ))



; This function instantiates expr 'd' under name 'n'.
(defn gen-def
  [n d]
  (let [n (symbol n)]
    (eval `(def ~n ~d))))


; This is the entry point function for the translation. It expands all previously defined names in the 'func-def', translates it to SKI
; calculus, adds 'func-def' to the 'sources', adds the resulting translation to 'ski-translations', and prints the translation to the console.
(defn translate-lambda-to-ski [func-name func-def]
  (let [expanded-func-def (substitute-translations @sources func-def)
        translated-def (translate-dispatch expanded-func-def)
        _ (add-translation expanded-sources func-name expanded-func-def)
        _ (add-translation sources func-name func-def)
        _ (add-translation ski-translations func-name translated-def)
        _ (println "function " func-name " translated to " translated-def)
        ]
    (gen-def func-name translated-def))
  )








