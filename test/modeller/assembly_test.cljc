(ns modeller.assembly-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.profile :as profile]
            [modeller.profile-builder :as builder]
            [modeller.sections-au :as au]
            [modeller.vec :as v]))

(defn- part [instr id]
  (some #(when (= id (:id %)) %) (get-in instr [:child :children])))

(deftest profiles
  (let [o (get-in fx/model [:sections "310UB40.4" :section/outline])]
    (testing "root radii are kept as arcs: 12 corners, 4 of them rounded"
      (is (= 16 (count (:outer o))))
      (is (= 4 (count (filter #(= 3 (count %)) (:outer o))))))
    (is (v/nearly= 304.0 (:d o)))
    (testing "area matches the table's gross area within 0.5% (about 5210 mm2)"
      (is (< (Math/abs (- (profile/area o) 5210)) 26))))
  (is (= 1 (count (get-in fx/model [:sections "100x100x6.0SHS" :section/outline :holes]))))
  (testing "every hot rolled section builds to within 0.5% of its table gross area"
    (let [off (for [{:section/keys [designation] :keys [Ag]} au/hot-rolled
                    :let [a (profile/area (get-in fx/model [:sections designation :section/outline]))]
                    :when (> (Math/abs (- 1 (/ a Ag))) 0.005)]
                [designation (Math/round a) Ag])]
      (is (empty? off))
      (is (= 120 (count au/hot-rolled)))))
  (testing "CHS area"
    (let [o (get-in fx/model [:sections "101.6x4.0CHS" :section/outline])]
      (is (v/nearly= (* Math/PI (- (* 50.8 50.8) (* 46.8 46.8))) (profile/area o) 1.0))))
  (is (empty? (:errors fx/lib)))
  (testing "insertion point :top-centre puts top of steel on the line"
    (is (= [0 -152.0] (profile/insertion-shift {:b 165 :d 304} :top-centre))))
  (is (= [82.5 152.0] (profile/insertion-shift {:b 165 :d 304} :bottom-left))))

(deftest builders
  (testing "the example custom builder compiles and builds"
    (is (:outline (builder/build (builder/compile-builder builder/example-builder)
                                 {:b 50 :d 25 :t 2 :r 3}))))
  (testing "a user builder from source"
    (let [c (builder/compile-builder "(fn [{:keys [b d]}] {:outer (round-corners (rect-loop b d) [5 5 5 5])})")
          {:keys [outline]} (builder/build c {:b 100 :d 50})]
      (is (v/nearly= 100.0 (:b outline)))
      (is (v/nearly= (- 5000 (* 4 (- 25 (* Math/PI 25 0.25)))) (profile/area outline) 0.5))))
  (testing "the sandbox has no file access"
    (is (:error (builder/compile-builder "(slurp \"/etc/passwd\")"))))
  (testing "bad output is an error, not a crash"
    (is (= :builder/invalid-outline
           (get-in (builder/build (builder/compile-builder "(fn [_] {:outer 3})") {}) [:error :error/type])))))

(deftest assembly-structure
  (let [{:keys [instructions]} (core/regenerate fx/model)
        col (:c-a1 instructions)]
    (testing "one outer transform carrying the XLine frame"
      (is (= :transform (:op col)))
      (is (v/nearly= [0 0 1] (get-in col [:frame :z]))))
    (testing "the xline itself is in the group for display and grips"
      (is (= {:op :xline :id :c-a1 :length 4200.0} (first (get-in col [:child :children])))))
    (testing "rotation turns the section about its insertion point"
      (let [m (update-in fx/model [:xlines :b-1 :assembly/parts 0] assoc :part/rotation 90)
            p (part (get-in (core/regenerate m) [:instructions :b-1]) :main)
            f (get-in p [:target :target :frame])]
        ;; top-centre stays on the line; the section now hangs off to local +X
        (is (v/nearly= [152.0 0 0] (:o f) 1e-9))
        (is (v/nearly= [0 1 0] (:x f) 1e-9))))
    (testing "part extrudes full length in construction space"
      (is (v/nearly= 4200.0 (get-in (part col :main) [:child :length]))))
    (testing "tilt panel is a rectangle sitting on its line"
      (let [p (part (:p-a instructions) :panel)]
        (is (v/nearly= [0 2400.0 0] (get-in p [:frame :o])))))))

(deftest cut-to-face
  (let [{:keys [instructions errors]} (core/regenerate fx/model)
        beam (part (:b-1 instructions) :main)]
    (is (empty? errors))
    (testing "start and end both cut, start applied first"
      (is (= :slice (:op beam)))
      (is (= :slice (get-in beam [:target :op]))))
    (testing "end cut: column B1 face at y = 8000 - 101.7, less 10 gap"
      (is (v/nearly= 7888.3 (get-in beam [:plane :origin 2])))
      (is (v/nearly= [0 0 -1] (get-in beam [:plane :normal])))
      (is (= :positive (:keep beam))))
    (testing "start cut: column A1 face at 101.7 plus 10 gap"
      (let [inner (:target beam)]
        (is (v/nearly= 111.7 (get-in inner [:plane :origin 2])))
        (is (v/nearly= [0 0 1] (get-in inner [:plane :normal])))
        (is (= :positive (:keep inner)))))
    (testing "the gap comes from connection params over template defaults"
      (let [m (assoc-in fx/model [:connections :b-1-end :connection/params] {:gap 20})
            beam (part (get-in (core/regenerate m) [:instructions :b-1]) :main)]
        (is (v/nearly= 7878.3 (get-in beam [:plane :origin 2])))))
    (testing "a UB column: web or flange face depending on its roll"
      (let [m (assoc-in fx/model [:xlines :c-b1 :assembly/parts 0 :part/profile] "310UB40.4")
            cut #(get-in (part (get-in (core/regenerate %) [:instructions :b-1]) :main)
                         [:plane :origin 2])]
        (is (v/nearly= 7838.0 (cut m)))
        (is (v/nearly= 7907.5 (cut (assoc-in m [:xlines :c-b1 :xline/rotation] 90))))))))

(deftest connection-node-rule
  (testing "a connection whose owner node is not on the referenced XLine is refused"
    (let [m (assoc-in fx/model [:connections :b-1-end :connection/refs] [:c-b2])
          {:keys [errors instructions]} (core/regenerate m)
          beam (part (:b-1 instructions) :main)]
      (is (= [:connection/node-not-on-ref] (map :error/type errors)))
      (testing "and the beam keeps its full length at that end, cut only at the valid start"
        (is (= :slice (:op beam)))
        (is (not= :slice (get-in beam [:target :op])))))))

