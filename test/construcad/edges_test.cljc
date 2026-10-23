(ns construcad.edges-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.edges :as edges]))

(defn- quad
  "Two triangles for the quad a b c d (counter-clockwise)."
  [a b c d]
  (concat a b c a c d))

(defn- box-soup
  "A unit box; the top face is split with a T-junction at its front edge
  midpoint, the way a boolean leaves it."
  []
  (let [p (fn [x y z] [x y z])
        m (p 0.5 0 1)]
    (concat
     (quad (p 0 0 0) (p 0 1 0) (p 1 1 0) (p 1 0 0))            ; bottom
     ;; top: three triangles, vertex m on the edge (0 0 1)-(1 0 1)
     (p 0 0 1) m (p 0 1 1)
     m (p 1 1 1) (p 0 1 1)
     m (p 1 0 1) (p 1 1 1)
     ;; front face y=0 uses the whole edge (0 0 1)-(1 0 1): a T-junction at m
     (quad (p 0 0 0) (p 1 0 0) (p 1 0 1) (p 0 0 1))
     (quad (p 1 0 0) (p 1 1 0) (p 1 1 1) (p 1 0 1))
     (quad (p 1 1 0) (p 0 1 0) (p 0 1 1) (p 1 1 1))
     (quad (p 0 1 0) (p 0 0 0) (p 0 0 1) (p 0 1 1)))))

(defn- segments [flat] (partition 6 flat))

(deftest box-with-t-junction
  (let [segs (segments (edges/feature-edges (box-soup) 20))]
    (testing "no diagonals or seams on any face, only the 12 box edges (front top split at the junction)"
      (is (= 13 (count segs)))
      (is (every? (fn [[x1 y1 z1 x2 y2 z2]]
                    (= 1 (count (filter #(> (abs %) 1e-9) [(- x2 x1) (- y2 y1) (- z2 z1)]))))
                  segs)))))

(defn- cylinder-soup [n r h]
  (let [pt (fn [i z] (let [a (* 2 Math/PI (/ i n))] [(* r (Math/cos a)) (* r (Math/sin a)) z]))]
    (mapcat (fn [i]
              (let [j (inc i)]
                (concat (quad (pt i 0) (pt j 0) (pt j h) (pt i h))
                        [0 0 0] (pt j 0) (pt i 0)
                        [0 0 h] (pt i h) (pt j h))))
            (range n))))

(deftest cylinder-is-smooth
  (let [segs (segments (edges/feature-edges (cylinder-soup 32 10 50) 20))]
    (is (= 64 (count segs)) "just the two rims")
    (is (every? (fn [[_ _ z1 _ _ z2]] (= z1 z2)) segs))))
