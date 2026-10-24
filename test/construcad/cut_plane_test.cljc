(ns construcad.cut-plane-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.cut-plane :as cp]
            [modeller.vec :as v]))

(defn- box-soup
  "A closed box [0 sx] x [0 sy] x [0 sz] as flat coordinates."
  [sx sy sz]
  (let [p (fn [x y z] [(* x sx) (* y sy) (* z sz)])
        quad (fn [a b c d] (concat a b c a c d))]
    (vec (concat (quad (p 0 0 0) (p 0 1 0) (p 1 1 0) (p 1 0 0))
                 (quad (p 0 0 1) (p 1 0 1) (p 1 1 1) (p 0 1 1))
                 (quad (p 0 0 0) (p 1 0 0) (p 1 0 1) (p 0 0 1))
                 (quad (p 0 1 0) (p 0 1 1) (p 1 1 1) (p 1 1 0))
                 (quad (p 0 0 0) (p 0 0 1) (p 0 1 1) (p 0 1 0))
                 (quad (p 1 0 0) (p 1 1 0) (p 1 1 1) (p 1 0 1))))))

(defn- sorted-size [q] (vec (sort (:size q))))

(deftest plane-axes
  (doseq [n [[0 0 1] [1 0 0] [0 1 0] (v/normalize [1 2 3])]]
    (let [[u w] (cp/plane-axes n)]
      (is (v/nearly= 1 (v/magnitude u)))
      (is (v/nearly= 0 (v/dot u n)))
      (is (v/nearly= n (v/cross u w))))))

(deftest square-cut-through-a-member
  ;; a 200 x 310 member along Z, cut square at z = 1000
  (let [soup (box-soup 200 310 3000)
        plane {:origin [0 0 1000] :normal [0 0 1]}
        q (cp/quad plane (cp/section-points soup plane))]
    (testing "the section's extent plus 15% each side"
      (is (v/nearly= [(* 1.3 200) (* 1.3 310)] (sorted-size q) 1e-6)))
    (testing "centred on the section, in the plane"
      (is (v/nearly= [100 155 1000] (:centre q) 1e-6))
      (is (every? #(v/nearly= 1000 (% 2)) (:corners q))))))

(deftest margin-floor
  ;; a 10 x 100 plate edge: 15% is 1.5 and 15 mm, the floor of 25 mm wins
  (let [soup (box-soup 10 100 200)
        plane {:origin [0 0 50] :normal [0 0 1]}]
    (is (v/nearly= [60 150] (sorted-size (cp/quad plane (cp/section-points soup plane))) 1e-6))
    (is (v/nearly= [10 100] (sorted-size (cp/quad plane (cp/section-points soup plane) {:fraction 0 :min 0})) 1e-6))))

(deftest a-plane-that-misses
  (let [plane {:origin [0 0 5000] :normal [0 0 1]}]
    (is (empty? (cp/section-points (box-soup 10 10 10) plane)))
    (is (nil? (cp/quad plane [])))))

(deftest oblique-cut
  ;; a 45 degree cut across a 100 x 100 bar: 100 one way, 100 root 2 the other
  (let [soup (box-soup 100 100 1000)
        plane {:origin [0 0 500] :normal (v/normalize [1 0 1])}
        q (cp/quad plane (cp/section-points soup plane) {:fraction 0 :min 0})]
    (is (v/nearly= [100 (* 100 (Math/sqrt 2))] (sorted-size q) 1e-6))))
