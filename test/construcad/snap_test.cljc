(ns construcad.snap-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.edits :as edits]
            [construcad.edits-test :refer [seeded by-mark]]
            [construcad.snap :as snap]
            [modeller.vec :as v]))

(defn plan-view
  "Top view, 1 px per 10 mm, origin at screen (100, 900)."
  [[x y _]]
  [(+ 100 (/ x 10.0)) (- 900 (/ y 10.0))])

(defn ctx []
  (let [{m :model} (seeded)
        view (edits/view m (keys (:xlines m)))]
    {:m m
     :snap {:grid (:grid m) :level (second (:levels m))
            :resolved (:resolved view) :xlines (:xlines m)}}))

(deftest snaps-decide-the-reference
  (let [{:keys [m snap]} (ctx)]
    (testing "a column top wins over the grid intersection under it"
      (let [s (snap/pick snap plan-view [700 900] 12)]
        (is (= :end (:kind s)))
        (is (= :column (get-in m [:xlines (get-in s [:ref :node/of]) :xline/role])))
        (is (= :end (get-in s [:ref :node/end])))))
    (testing "excluding the XLines there leaves the grid intersection at the level"
      (let [ex (set (filter #(#{:column :beam} (get-in m [:xlines % :xline/role])) (keys (:xlines m))))
            s (snap/pick (assoc snap :exclude ex) plan-view [702 899] 12)]
        (is (= {:node/grid [:y1 :x2] :node/level "L1"} (:ref s)))
        (is (= "A2" (:label s)))))
    (testing "along a beam, to the millimetre"
      (let [b1 (by-mark m "B1")
            s (snap/pick snap plan-view [101 500] 12)]
        (is (= {:node/on b1 :node/station 4000 :node/from :start} (:ref s)))
        (is (v/nearly= [0 4000 4200] (:point s)))))
    (is (nil? (snap/pick snap plan-view [400 600] 12)))
    (is (= {:node/at [12 35 4200]} (:ref (snap/free [12.4 34.6 4200]))))))

(deftest a-node-may-not-reference-its-dependents
  (let [{:keys [m]} (ctx)
        c1 (by-mark m "C1") b1 (by-mark m "B1")]
    (is (= #{c1 b1} (snap/excluded (:xlines m) c1)))))

(deftest typed-entry
  (is (= {:relative? true :values [0.0 0.0 600.0]} (snap/parse-entry "@0, 0, 600")))
  (is (= {:relative? false :values [1.0 2.0 3.0]} (snap/parse-entry "1,2,3")))
  (is (:error (snap/parse-entry "@1,2")))
  (let [{:keys [m]} (ctx)]
    (testing "moving a grid node keeps the grid reference"
      (is (= {:node/grid [:y1 :x1] :node/level "L1" :node/offset [0 0 600]}
             (snap/moved-ref (:grid m) {:node/grid [:y1 :x1] :node/level "L1"} [0 0 4200] [0 0 600]))))
    (testing "on a rotated grid the offset is along the grid's own axes"
      (is (= [0 -100 0]
             (:node/offset (snap/moved-ref (assoc (:grid m) :grid/angle 90) {:node/grid [:y1 :x1]} [0 0 0] [100 0 0])))))
    (is (= {:node/at [100 0 4200]}
           (snap/moved-ref (:grid m) {:node/of :x :node/end :end} [0 0 4200] [100 0 0])))))
