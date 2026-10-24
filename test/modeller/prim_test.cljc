(ns modeller.prim-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.prim :as prim]))

(def tree
  [{:prim/id :cut :prim/type :cut}
   {:prim/id :plate :prim/type :plate
    :prim/children [{:prim/id :holes :prim/type :hole-group}
                    {:prim/id :g1 :prim/type :gusset
                     :prim/children [{:prim/id :snipe :prim/type :cut}]}]}])

(deftest walking
  (is (= [:cut :plate :holes :g1 :snipe] (map :prim/id (prim/walk tree))))
  (is (= [[:cut 0] [:plate 0] [:holes 1] [:g1 1] [:snipe 2]]
         (map (fn [[p d]] [(:prim/id p) d]) (prim/depth-list tree))))
  (is (= [1 :prim/children 1 :prim/children 0] (prim/path-of tree :snipe)))
  (is (= :gusset (:prim/type (prim/find-prim tree :g1))))
  (is (= :g1 (prim/parent-of tree :snipe)))
  (is (nil? (prim/parent-of tree :plate)))
  (is (nil? (prim/path-of tree :nope)))
  (is (= [:cut :plate :hole-group :gusset :cut] (prim/summary tree))))

(deftest editing
  (testing "fresh ids per type"
    (is (= :cut-1 (prim/fresh-id tree :cut)))
    (is (= :plate-2 (prim/fresh-id (conj tree {:prim/id :plate-1}) :plate))))
  (testing "add at the top level or as a child"
    (is (= :x (:prim/id (peek (prim/add-prim tree nil {:prim/id :x})))))
    (is (= [:holes :g1 :x] (map :prim/id (:prim/children (prim/find-prim (prim/add-prim tree :plate {:prim/id :x}) :plate)))))
    (is (= [:x] (map :prim/id (:prim/children (prim/find-prim (prim/add-prim tree :holes {:prim/id :x}) :holes))))
        "a prim with no children yet"))
  (testing "remove takes the children with it"
    (is (= [:cut :plate :holes] (map :prim/id (prim/walk (prim/remove-prim tree :g1))))))
  (testing "move among siblings, stopping at the ends"
    (is (= [:plate :cut] (map :prim/id (prim/move-prim tree :cut 1))))
    (is (= [:g1 :holes] (map :prim/id (:prim/children (prim/find-prim (prim/move-prim tree :g1 -1) :plate)))))
    (is (= tree (prim/move-prim tree :cut -1))))
  (testing "update one prim anywhere in the tree"
    (is (= "Snipe" (:prim/name (prim/find-prim (prim/update-prim tree :snipe assoc :prim/name "Snipe") :snipe))))))

(deftest checking
  (let [known #{:cut :plate :hole-group :gusset}]
    (is (empty? (prim/tree-errors tree known)))
    (is (= [:no-parts] (map :reason (prim/tree-errors [] known))))
    (is (= [:unknown-type] (map :reason (prim/tree-errors [{:prim/id :w :prim/type :weld}] known))))
    (is (= [:no-id] (map :reason (prim/tree-errors [{:prim/type :cut}] known))))
    (is (= [:duplicate-id] (map :reason (prim/tree-errors (prim/add-prim tree :g1 {:prim/id :cut :prim/type :cut}) known))))))
