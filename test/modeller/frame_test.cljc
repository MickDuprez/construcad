(ns modeller.frame-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.frame :as frame]
            [modeller.vec :as v]))

(defn- frame= [a b]
  (every? #(v/nearly= (% a) (% b) 1e-9) [:o :x :y :z]))

(deftest vector-basics
  (is (= [0 0 1] (v/cross [1 0 0] [0 1 0])))
  (is (nil? (v/normalize [0 0 0])))
  (is (v/parallel? [1 0 0] [-1 0 0]))
  (is (not (v/parallel? [1 0 0] [0.9 0.1 0]))))

(deftest xline-frames
  (testing "horizontal beam along world X: local Y is world up"
    (let [{f :frame l :length} (frame/xline-frame [0 0 0] [5000 0 0] 0)]
      (is (v/nearly= 5000.0 l))
      (is (v/nearly= [1 0 0] (:z f)))
      (is (v/nearly= [0 0 1] (:y f)))
      (is (v/nearly= [0 1 0] (:x f)))
      (is (frame/valid? f))))
  (testing "vertical column: reference up falls back to world Y"
    (let [{f :frame} (frame/xline-frame [0 0 0] [0 0 3000] 0)]
      (is (v/nearly= [0 0 1] (:z f)))
      (is (v/nearly= [0 1 0] (:y f)))
      (is (v/nearly= [1 0 0] (:x f)))))
  (testing "rotation rolls the section about the line"
    (let [{f :frame} (frame/xline-frame [0 0 0] [5000 0 0] 90)]
      (is (v/nearly= [0 0 1] (:x f)))
      (is (v/nearly= [0 -1 0] (:y f)))))
  (testing "oblique brace stays orthonormal"
    (let [{f :frame} (frame/xline-frame [100 200 0] [3100 -800 2500] 17)]
      (is (frame/valid? f))
      (is (v/nearly= [100 200 0] (:o f)))))
  (testing "invalid frames are reported, not guessed"
    (is (= :zero-length (get-in (frame/xline-frame [1 1 1] [1 1 1] 0) [:error :reason])))
    (is (= :parallel-ref-up
           (get-in (frame/xline-frame [0 0 0] [0 0 10] 0 [0 0 1]) [:error :reason])))))

(deftest round-trips
  (let [{f :frame} (frame/xline-frame [100 200 300] [-900 1200 2300] 33)
        p [12 -34 56]]
    (is (v/nearly= p (frame/point->local f (frame/point->parent f p)) 1e-9))
    (is (frame= frame/world (frame/compose f (frame/invert f))))
    (testing "points translate, directions do not"
      (is (v/nearly= (:z f) (frame/dir->parent f [0 0 1]) 1e-9))
      (is (v/nearly= (v/v+ (:o f) (:z f)) (frame/point->parent f [0 0 1]) 1e-9)))
    (testing "nested composition equals stepwise transformation"
      (let [child (frame/rotate-z (assoc frame/world :o [10 20 30]) 45)]
        (is (v/nearly= (frame/point->parent f (frame/point->parent child p))
                       (frame/point->parent (frame/compose f child) p) 1e-9))))
    (testing "relative frame of b in a"
      (let [{g :frame} (frame/xline-frame [0 0 0] [0 0 1000] 0)
            rel (frame/relative f g)]
        (is (frame= g (frame/compose f rel)))))))
