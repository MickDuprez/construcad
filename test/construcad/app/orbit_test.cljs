(ns construcad.app.orbit-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.app.orbit :as orbit]))

(deftest view-shift
  (testing "a point drawn where it was wanted needs no shift"
    (is (= [0 0] (orbit/view-shift [-2 2 1 -1 1] [0.5 0.5] [0.5 0.5]))))
  (testing "half a view width to the right, at zoom 1 and 2"
    (is (= [2 0] (orbit/view-shift [-2 2 1 -1 1] [0 0] [1 0])))
    (is (= [1 0] (orbit/view-shift [-2 2 1 -1 2] [0 0] [1 0]))))
  (testing "below where it was wanted: slide the camera down"
    (is (= [0 -0.5] (orbit/view-shift [-2 2 1 -1 1] [0 0.5] [0 0])))))
