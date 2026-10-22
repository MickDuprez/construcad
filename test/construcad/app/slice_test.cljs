(ns construcad.app.slice-test
  (:require ["three" :as THREE]
            [clojure.test :refer [deftest is testing]]
            [construcad.app.reader :as reader]
            [construcad.app.slice :as slice]
            [modeller.library :as library]
            [modeller.sections-au :as au]))

(def lib (library/library {} au/rows))

(defn- triangles [^js g]
  (let [a (.. g -attributes -position -array)]
    (for [i (range 0 (.-length a) 9)]
      (vec (for [k (range 3)]
             (mapv #(js/Math.round (* 1000 (aget a (+ i (* 3 k) %)))) (range 3)))))))

(defn- open-edges
  "Edges used by an odd number of triangles: zero for a closed mesh."
  [g]
  (->> (triangles g)
       (mapcat (fn [[a b c]] [#{a b} #{b c} #{c a}]))
       frequencies
       (filter (comp odd? val))
       count))

(defn- cap-area
  "Area of the triangles lying in the plane z = at (coordinates in microns)."
  [g at]
  (let [at (* 1000 at)]
    (/ (reduce + (for [[[ax ay az] [bx by bz] [cx cy cz]] (triangles g)
                       :when (= at az bz cz)]
                   (/ (js/Math.abs (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))) 2)))
       1e6)))

(defn- z-range [^js g]
  (.computeBoundingBox g)
  [(.. g -boundingBox -min -z) (.. g -boundingBox -max -z)])

(deftest slicing-keeps-a-closed-solid
  (let [outline (get-in lib [:sections "310UB40.4" :section/outline])
        beam (reader/extrude-geometry outline 6000)]
    (is (zero? (open-edges beam)) "the extrusion itself is closed")
    (testing "a square cut"
      (let [cut (slice/slice beam {:origin [0 0 5000] :normal [0 0 1]} :negative)]
        (is (zero? (open-edges cut)))
        (is (< (js/Math.abs (- 5000 (second (z-range cut)))) 1e-3))))
    (testing "a skew cut through the flanges and web"
      (let [cut (slice/slice beam {:origin [0 0 3000] :normal [0.3 0.2 1]} :positive)]
        (is (zero? (open-edges cut)))
        (is (< (js/Math.abs (- 6000 (second (z-range cut)))) 1e-3))))
    (testing "a hollow section keeps its hole in the cap"
      (let [rhs (reader/extrude-geometry {:outer [[-50 -50] [50 -50] [50 50] [-50 50]]
                                          :holes [[[-40 -40] [-40 40] [40 40] [40 -40]]]}
                                         1000)
            cut (slice/slice rhs {:origin [0 0 500] :normal [0 0 1]} :negative)]
        (is (zero? (open-edges cut)))
        (is (= 3600 (js/Math.round (cap-area cut 500))) "100 x 100 less the 80 x 80 hole")))))
