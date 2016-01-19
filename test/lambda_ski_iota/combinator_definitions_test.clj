(ns lambda-ski-iota.combinator-definitions-test
  (:require [clojure.test :refer :all]
            [lambda-ski-iota.combinator-definitions :refer :all]))


(defn test-fnct-def [x y] (fn [z] (* z (+ x y))))

(def test-fnct (variadize test-fnct-def))


(deftest variadize-test-1
(testing "variadize"
(is (= ((test-fnct 1 2 3)) 9))
))

(deftest variadize-test-2
(is (= ((test-fnct 1 2) 3) 9))
)

(deftest variadize-test-3
(is (= (((test-fnct 1) 2) 3) 9))
)


(deftest variadize-test-4
(is (= ((((test-fnct) 1) 2) 3) 9))
)

(def delayed-eval-expr (->DelayedEval (delay (+ 1 2))))

(deftest delayed-eval-test-1
(is (= (delayed-eval-expr) 3))
)

(deftest S-test-1
(is (= ((S (fn [x y] x) (fn [x] x) (fn [x] x)) 5) 5))
)

(deftest K-test-1
(is (= (((K (fn [x] x) 2)) 3) 3))
)

(deftest K-test-2
(is (= (K (fn [x] x) 2 3) 3))
)


(deftest I-test-1
(is (= ((I 5)) 5))
)


(run-tests)
