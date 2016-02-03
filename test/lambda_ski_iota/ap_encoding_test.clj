(ns lambda-ski-iota.ap-encoding-test
  (:require [clojure.test :refer :all]
            [lambda-ski-iota.combinator-definitions :refer :all]
            [lambda-ski-iota.ski-encoding :refer :all]
            [lambda-ski-iota.iota-encoding :refer :all]
            [lambda-ski-iota.ap-encoding :refer :all]))


(deftest translate-to-ap-test-1
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 'X) '(Bv1 Pv1 Ev1) ))
))

(deftest translate-to-ap-test-2
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '(X X)) '(Bv1 Pv1 Pv1 Av1 Ev1) ))
))

(deftest translate-to-ap-test-3
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '(X (X X))) '(Bv1 Pv1 Pv1 Pv1 Av1 Av1 Ev1) ))
))

(deftest translate-to-ap-test-4
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '(X (X X) X)) '(Bv1 Pv1 Pv1 Pv1 Av1 Av1 Pv1 Av1 Ev1) ))
))

(deftest translate-to-ap-test-5
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '((X X) X)) '(Bv1 Pv1 Pv1 Av1 Pv1 Av1 Ev1) ))
))

(deftest translate-to-ap-test-6
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '((X (X X)) X)) '(Bv1 Pv1 Pv1 Pv1 Av1 Av1 Pv1 Av1 Ev1) ))
))

(deftest translate-to-ap-test-7
(testing "translate-to-ap"
(is (= (translate-to-ap ap-combinators-v1 '((X X X) X)) '(Bv1 Pv1 Pv1 Av1 Pv1 Av1 Pv1 Av1 Ev1) ))
))

(deftest inline-b-p-combinators-test-1
(testing "inline-b-p-combinators"
(is (= (inline-b-p-combinators ap-combinators-v3 (list 'B 'P 'P 'A 'A 'A)) '(P P A P A A A) ))
))


(deftest inline-b-p-combinators-test-2
(testing "inline-b-p-combinators"
(is (= (inline-b-p-combinators ap-combinators-v3  (list 'B 'P 'P 'P 'A 'A 'A)) '(P P A P P A A A) ))
))


(deftest augmented-list-test-1
(testing "augmented lists"
(is (to-bool (X-Nil-Mod? X-Nil-Mod)))
))


(deftest augmented-list-test-2
(testing "augmented lists"
(is (false? (to-bool (X-Nil-Mod? (X-Cons-Mod X X-Nil-Mod)))))
))

(deftest augmented-list-test-3
(testing "augmented lists"
(is (false? (to-bool (X-Nil-Mod? (X-Cons-Mod X (X-Cons-Mod X X-Nil-Mod))))  ))
))

(deftest augmented-list-test-4
(testing "augmented lists"
(is (false? (to-bool (X-Nil-Mod? (X-Tail-Mod (X-Cons-Mod X (X-Cons-Mod X X-Nil-Mod)))))  ))
))

(deftest augmented-list-test-5
(testing "augmented lists"
(is  (to-bool (X-Nil-Mod? (X-Tail-Mod (X-Tail-Mod (X-Cons-Mod X (X-Cons-Mod X X-Nil-Mod)))))))
))


; (X X) should be I

(def l (X-Cons-Mod X (X-Cons-Mod X X-Nil-Mod)))

(deftest augmented-list-test-6
(testing "augmented lists"
(is  (= ((((X-Head-Mod (X-Tail-Mod l))  (X-Head-Mod l)) 5)) 5))
))


(deftest augmented-list-test-7
(testing "augmented lists"
(is  (= (((X-Head-Mod (X-Cons-Mod ((X-Head-Mod (X-Tail-Mod l))  (X-Head-Mod l)) (X-Tail-Mod (X-Tail-Mod l)))) 5)) 5))
))


(deftest a-v3-combinator-test-1
(testing "A combinator (v3)"
(is (to-bool (X-Nil? (A X-True))))
))

; (X X) == I

(deftest a-v3-combinator-test-2
(testing "A combinator (v3)"
(is  (= (((A X-False l A) 5)) 5))
))

;(Bv1 Pv1 Pv1 Av1 Ev1)
(deftest a-v3-combinator-test-3
(testing "A combinator (v3)"
(is  (= (((P P A P A A) 5)) 5))
))



(run-tests)
