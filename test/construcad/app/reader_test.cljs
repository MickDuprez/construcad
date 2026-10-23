(ns construcad.app.reader-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.app.reader :as reader]
            [modeller.frame :as frame]
            [modeller.instr :as instr]))

(defn- volume
  "Enclosed volume of a closed triangle soup (sum of signed tetrahedra)."
  [^js g]
  (let [a (.. g -attributes -position -array)]
    (/ (reduce + (for [i (range 0 (.-length a) 9)]
                   (let [[ax ay az bx by bz cx cy cz] (map #(aget a (+ i %)) (range 9))]
                     (+ (* ax (- (* by cz) (* bz cy)))
                        (* ay (- (* bz cx) (* bx cz)))
                        (* az (- (* bx cy) (* by cx)))))))
       6)))

(defn- near? [a b tol] (< (js/Math.abs (- a b)) tol))

;; a 32-sided cylinder of radius r has area 16 r^2 sin(pi/16)
(defn- hole-area [r] (* 16 r r (js/Math.sin (/ js/Math.PI 16))))

(deftest cylinders
  (is (near? (* (hole-area 10) 50) (volume (reader/solid-geometry (instr/cylinder 10 50))) 0.5)))

(deftest subtract-cuts-holes
  (let [plate (instr/box [100 100 10])
        tool (fn [x] (instr/transform (frame/translate frame/world [x 0 -1]) (instr/cylinder 10 12)))]
    (testing "one hole through a plate"
      (is (near? (- 100000 (* 10 (hole-area 10)))
                 (volume (reader/solid-geometry (instr/subtract plate [(tool 0)])))
                 1)))
    (testing "several tools"
      (is (near? (- 100000 (* 2 10 (hole-area 10)))
                 (volume (reader/solid-geometry (instr/subtract plate [(tool -25) (tool 25)])))
                 1)))
    (testing "union and intersect"
      (is (near? (* (hole-area 10) 10)
                 (volume (reader/solid-geometry (instr/intersect [plate (tool 0)])))
                 1)))))
