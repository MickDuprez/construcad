(ns modeller.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.grid :as grid]
            [modeller.instr :as instr]
            [modeller.model :as model]
            [modeller.vec :as v]))

(deftest grid-references
  (is (= [6000 8000] (grid/intersection fx/grid "B" "2")))
  (is (= [6000 8000] (grid/intersection fx/grid "2" "B")))
  (is (v/nearly= [6000 8000 4200] (grid/grid-point fx/grid ["2" "B"] 4200 nil)))
  (testing "rotated grid"
    (let [g (assoc fx/grid :grid/angle 90 :grid/origin [1000 0 0])]
      (is (v/nearly= [1000 6000 0] (grid/grid-point g ["A" "2"] 0 nil) 1e-9))))
  (testing "a point reads back as nearest axes plus offsets"
    (is (= {:x-axis {:id :x2 :label "2" :offset 150} :y-axis {:id :y1 :label "A" :offset -300} :z 4200}
           (update-vals (grid/nearest-axes fx/grid [6150 -300 4200])
                        #(if (map? %) (update % :offset long) (long %))))))
  (is (= {:level "L1" :offset 100} (grid/nearest-level fx/levels 4300))))

(deftest grid-labels
  (is (= ["A" "B" "H" "J" "N" "P" "Z" "AA"]
         (map (grid/labels :letters 26) [0 1 7 8 12 13 23 24])))
  (testing "switching styles keeps ids, so references still resolve"
    (let [g (grid/swap-styles fx/grid)]
      (is (= ["A" "B"] (map :label (:grid/x-axes g))))
      (is (= ["1" "2"] (map :label (:grid/y-axes g))))
      (is (= [:x1 :x2] (map :id (:grid/x-axes g))))
      (is (v/nearly= [0 8000 4200]
                     (get-in (model/resolve-xlines (assoc fx/model :grid g)) [:resolved :b-1 :end])))))
  (testing "names from a template"
    (let [{:keys [x-axis y-axis]} (grid/nearest-axes fx/grid [0 0 0])]
      (is (= "A1" (grid/point-name "{y}{x}" x-axis y-axis nil)))
      (is (= "A0" (grid/point-name "{y}{level}" x-axis y-axis {:level/name "0"}))))))

(deftest resolving-xlines
  (let [{:keys [resolved errors]} (model/resolve-xlines fx/model)]
    (is (empty? errors))
    (is (v/nearly= [6000 8000 4200] (get-in resolved [:c-b2 :end])))
    (testing "beams hang off column top nodes"
      (is (v/nearly= [0 0 4200] (get-in resolved [:b-1 :start])))
      (is (v/nearly= [0 8000 4200] (get-in resolved [:b-1 :end])))
      (is (v/nearly= 8000.0 (get-in resolved [:b-1 :length]))))
    (testing "panel offset from grid A"
      (is (v/nearly= [0 -300 0] (get-in resolved [:p-a :start]))))))

(deftest node-kinds
  (let [m (-> fx/model
              (assoc-in [:xlines :brace]
                        {:xline/id :brace
                         :xline/start {:node/on :b-1 :node/station 2000 :node/from :end}
                         :xline/end {:node/rel [0 0 -1000]}}))
        r (:resolved (model/resolve-xlines m))]
    (is (v/nearly= [0 6000 4200] (get-in r [:brace :start])))
    (is (v/nearly= [0 6000 3200] (get-in r [:brace :end])))))

(deftest moving-the-grid-moves-the-model
  (let [m (assoc-in fx/model [:grid :grid/y-axes 1 :offset] 9000)
        r (:resolved (model/resolve-xlines m))]
    (is (v/nearly= [0 9000 4200] (get-in r [:b-1 :end])))
    (testing "raising a level moves column tops and the beams on them"
      (let [m (assoc-in fx/model [:levels 1 :level/elevation] 5000)
            r (:resolved (model/resolve-xlines m))]
        (is (v/nearly= [0 0 5000] (get-in r [:b-1 :start])))))))

(deftest dependencies
  (testing "C-B1 follows because B-1, which moves, is connected to it (its holes)"
    (is (= #{:c-a1 :b-1 :c-b1} (model/affected fx/model [:c-a1]))))
  (is (= #{:b-2 :c-b2} (model/affected fx/model [:c-b2])))
  (is (= #{:p-a} (model/affected fx/model [:p-a])))
  (testing "edit regenerates only what changed"
    (let [{:keys [affected instructions errors]}
          (core/edit fx/model [:c-a1]
                     #(assoc-in % [:xlines :c-a1 :xline/end :node/offset] [0 0 -200]))]
      (is (empty? errors))
      (is (= #{:c-a1 :b-1 :c-b1} affected (set (keys instructions)))))))

(deftest errors
  (testing "cycles"
    (let [m (-> fx/model
                (assoc-in [:xlines :c-a1 :xline/start] {:node/of :b-1 :node/end :start}))]
      (is (= :model/cyclic-dependency (-> (model/resolve-xlines m) :errors first :error/type)))))
  (testing "unknown grid axis names the xline"
    (let [m (assoc-in fx/model [:xlines :c-a1 :xline/start :node/grid] ["Z" "1"])
          {:keys [resolved errors]} (model/resolve-xlines m)]
      (is (= :c-a1 (-> errors first :xline/id)))
      (is (= :unresolved-xline (-> errors second :reason)))
      (is (not (contains? resolved :b-1)))))
  (testing "unknown profile"
    (let [m (assoc-in fx/model [:xlines :c-a1 :assembly/parts 0 :part/profile] "999UB1")]
      (is (= :part/unknown-profile
             (->> (core/regenerate m) :errors (map :error/type) first))))))

(deftest whole-model-instructions-are-valid
  (let [{:keys [instructions errors]} (core/regenerate fx/model)]
    (is (empty? errors))
    (is (= 7 (count instructions)))
    (is (every? empty? (map instr/validate (vals instructions))))))
