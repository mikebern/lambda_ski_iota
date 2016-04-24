
; Implementation of AP combinators from "Theoretical Pearls: Flattening Combinatoes: Surviving Without Parentheses" by Chris Okasaki
; http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp03flat.pdf


(ns lambda-ski-iota.ap-encoding)
(use 'lambda-ski-iota.combinator-definitions)
(use 'lambda-ski-iota.ski-encoding)
(use 'lambda-ski-iota.iota-encoding)

; The version one set of AP combinators.
(def ap-combinators-v1 {:begin 'Bv1 :push 'Pv1 :apply 'Av1 :end 'Ev1})


; This function recursively translates an X combinator expression to its AP combinator translation.
; It is called from translate-to-ap and does the actual translation.
;
(defn translate-to-ap-impl [combinators l] (if (list? l)
                                             (if (empty? l)
                                               (throw (Exception. "should not be an empty list"))
                                               (concat (translate-to-ap-impl combinators (first l))  (flatten (map (fn [e] (list (translate-to-ap-impl combinators e) (:apply combinators))) (rest l)))))
                                             (list (:push combinators))))

; Examples
; Push two X's two stack and then call Apply
(translate-to-ap-impl ap-combinators-v1 '(X X ))
; We use curring, i.e. a function is applied to exactly one argument (all functions should be taking at least one argument). If there are several arguments then we call Apply successively for each of them.
(translate-to-ap-impl ap-combinators-v1 '(X X X))
; A more complex example:
(translate-to-ap-impl ap-combinators-v1 '(X X (X X X)))


; The entry point function for the translation.
; It takes a list of AP combinators and an X combinator expression and translates it to the correposnding AP expression.
; This function adds the 'Begin' and 'End' AP combinators and passes the real work to translate-to-ap-impl
(defn translate-to-ap [combinators l] (concat (list (:begin combinators)) (translate-to-ap-impl combinators l) (list (:end combinators))))

(defn Bv1-def [k] (k X-Nil))
(def Bv1 (variadize Bv1-def))

; Push 'X' to the stack
(defn Pv1-def [s k] (k (X-Cons X s)))
(def Pv1 (variadize Pv1-def))

; Apply the second element to the first and push the result to the stack
(defn Av1-def [s k] (k (X-Cons ((X-Second-Elem s)  (X-Head s)) (X-Tail (X-Tail s)))))
(def  Av1 (variadize  Av1-def))

; Return the first element as the result of the entire computation
(defn Ev1-def [s] (X-Head s))
(def Ev1 (variadize Ev1-def))

; Translate all functions in iota-translations
(def ap-translations-v1 (translate-definitions iota-translations (partial translate-to-ap ap-combinators-v1) "X" "APv1"))

(instantiate-definitions ap-translations-v1)

; Examples

(get ap-translations-v1 'APv1-QuickSort)

(to-int-list (APv1-QuickSort APv1-L-Not-Sorted))

(def AP-Seven (translate-to-ap ap-combinators-v1 (get iota-translations 'X-Seven)))

(to-int (eval AP-Seven))

;;;; Version two - getting rid of the 'End' combinator.

(def ap-combinators-v2 {:begin 'Bv2 :push 'Pv2 :apply 'Av2 :end 'Av2})

(defn Bv2-def [k] (k X-Nil))
(def Bv2 (variadize Bv2-def))

(defn Pv2-def [s k] (k (X-Cons X s)))
(def Pv2 (variadize Pv2-def))

(defn Av2-Aux1-def [s] (X-Head s))
(def Av2-Aux1 (variadize Av2-Aux1-def))

(defn Av2-Aux2-def [s k] (k (X-Cons ((X-Second-Elem s)  (X-Head s)) (X-Tail (X-Tail s)))))
(def  Av2-Aux2 (variadize  Av2-Aux2-def))

; Apply acts as End combinator if the size of the list is 1
(defn Av2-def [l] (X-If (X-Eq? (X-Length l) X-One) (Av2-Aux1 l) (Av2-Aux2 l)))
(def  Av2 (variadize  Av2-def))

(def ap-translations-v2 (translate-definitions iota-translations (partial translate-to-ap ap-combinators-v2) "X" "APv2"))

(instantiate-definitions ap-translations-v2)

; Examples

(get ap-translations-v2 'APv2-QuickSort)

(to-int-list (APv2-QuickSort APv2-L-Not-Sorted))

;;; v3

(def ap-combinators-v3 {:begin 'B :push 'P :apply 'A :end 'A})

; This function replaves the first combinators (which are always B and P) with P P A (see the paper for details)
(defn inline-b-p-combinators [combinators l] (if (> (count l) 1)
                                               (concat (list (:push combinators) (:push combinators) (:apply combinators))  (drop 2 l))
                                               l))

(def ap-translations-v3 (translate-definitions iota-translations
                                               (comp (partial inline-b-p-combinators ap-combinators-v3)  (partial translate-to-ap ap-combinators-v3)) "X" "AP"))


; Augmented lists - begin
; Augmented lists have an extra parameter to decide how the combinators should use the list

; We don't care about the  extra variable here, so the parameter is ignored
(defn X-Nil-Mod-def [b] X-Nil)
(def X-Nil-Mod (variadize X-Nil-Mod-def))

(defn X-Nil-Mod?-def [l] (X-Nil? (l X-True)))
(def X-Nil-Mod? (variadize X-Nil-Mod?-def))

(defn X-Cons-Mod-def [h t b] (X-Cons h t))
(def X-Cons-Mod (variadize X-Cons-Mod-def))

(defn X-Head-Mod-def [l] (X-Head (l X-True)))
(def X-Head-Mod (variadize X-Head-Mod-def))

(defn X-Tail-Mod-def [l] (X-Tail (l X-True)))
(def X-Tail-Mod (variadize X-Tail-Mod-def))

; Augmented lists - end

(defn P-def [b s k] (k X-False (X-Cons-Mod X s)))
(def P (variadize P-def))

(defn A-Aux1-def [s k] (k X-False (X-Cons-Mod ((X-Head-Mod (X-Tail-Mod s))  (X-Head-Mod s)) (X-Tail-Mod (X-Tail-Mod s)))))
(def  A-Aux1 (variadize  A-Aux1-def))

(defn A-Aux2-def [l] (X-If (X-Nil-Mod?  (X-Tail-Mod l)) (X-Head-Mod l) (A-Aux1 l)))
(def  A-Aux2 (variadize  A-Aux2-def))

(defn A-def [b] (X-If b X-Nil A-Aux2))

(def  A (variadize  A-def))

(instantiate-definitions ap-translations-v3)

(to-int AP-Seven)

(print (get ap-translations-v3 'AP-QuickSort))

(to-int-list (AP-QuickSort AP-L-Not-Sorted))

(to-int (AP-Ackermann (AP-Cons AP-Three (AP-Cons AP-Three AP-Nil))))
