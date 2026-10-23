(ns modeller.template-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.params :as params]
            [modeller.template :as template]
            [modeller.vec :as v]))

(defn- part [instr id]
  (some #(when (= id (:id %)) %) (get-in instr [:child :children])))

(defn- counter []
  (let [n (atom 0)] #(keyword (str "conn-" (swap! n inc)))))

(defn- applied []
  (template/apply-template fx/model :b-2 (get-in fx/model [:templates :tpl/beam-end-plates]) "310UB40.4"
                           (counter)))

(deftest formulas
  (let [{:keys [params errors]} (params/resolve-params
                                 {:t 12 :h "(+ d 20)" :p "(- d (* 2 tf) t)" :s "(:tf support)"}
                                 {:d 304 :tf 10.2} {:tf 11.0})]
    (is (empty? errors))
    (is (= {:t 12 :h 324 :s 11.0} (dissoc params :p)))
    (is (v/nearly= 271.6 (:p params))))
  (testing "a bad formula is an error naming the parameter"
    (is (= :h (-> (params/resolve-params {:h "(+ d"} {} nil) :errors first :param)))))

(deftest applying-a-template
  (let [{:keys [model changed connections warnings]} (applied)
        b2 (get-in model [:xlines :b-2])]
    (is (= #{:b-2} changed))
    (is (empty? warnings))
    (is (= :tpl/beam-end-plates (:assembly/template b2)))
    (is (= "310UB40.4" (:assembly/size b2)))
    (testing "one connection per end, support read from the end node"
      (is (= 2 (count (:upsert connections))))
      (is (= [:c-a2] (get-in model [:connections :conn-1 :connection/refs])))
      (is (= [:c-b2] (get-in model [:connections :conn-2 :connection/refs]))))
    (testing "applying again replaces rather than adds"
      (let [again (template/apply-template model :b-2 (get-in model [:templates :tpl/beam-end-plates]) "250UB31.4")]
        (is (= 2 (count (filter #(= :b-2 (:connection/owner %)) (vals (get-in again [:model :connections]))))))
        (is (empty? (get-in again [:connections :remove])))))
    (testing "an end not on another XLine gets no connection and a warning"
      (let [m (assoc-in fx/model [:xlines :b-2 :xline/end] {:node/at [6000 8000 4200]})
            r (template/apply-template m :b-2 (get-in m [:templates :tpl/beam-end-plates]) "310UB40.4")]
        (is (= 1 (count (get-in r [:connections :upsert]))))
        (is (= :end (-> r :warnings first :end)))))))

(deftest end-plates-and-bolts
  (let [{:keys [model]} (applied)
        {:keys [instructions errors]} (core/regenerate model [:b-2])
        b2 (:b-2 instructions)
        beam (part b2 :main)
        plate (part b2 :plate-end)
        bolts (part b2 :bolts-end)]
    (is (empty? errors))
    (testing "beam cut back by the plate thickness behind column C-B2's face (8000 - 101.7)"
      (is (v/nearly= 7886.3 (get-in beam [:plane :origin 2])))
      (is (v/nearly= [0 0 -1] (get-in beam [:plane :normal]))))
    (testing "plate sits on the face, centred on the section, sized from formulas"
      (is (= {:t 12 :w 165.0 :h 324.0} (:part/size plate)))
      (is (= :subtract (:op plate)))
      (is (v/nearly= [0 -152.0 7898.3] (get-in plate [:target :frame :o])))
      (is (v/nearly= [0 0 -1] (get-in plate [:target :frame :z])))
      (is (= 4 (count (:tools plate)))))
    (testing "four bolts at gauge 90 and pitch d - 2tf - 80"
      (is (= 4 (get-in bolts [:part/size :count])))
      (let [ys (sort (distinct (map #(Math/round (* 10 (get-in % [:frame :o 1]))) (:children bolts))))]
        (is (= [(Math/round (* 10 (- -152 101.8))) (Math/round (* 10 (+ -152 101.8)))] ys))))
    (testing "both ends get a plate"
      (is (part b2 :plate-start))
      (is (part b2 :bolts-start)))))

(deftest reapplying-after-a-template-edit
  (let [{:keys [model]} (applied)
        tpl (assoc (get-in model [:templates :tpl/beam-end-plates]) :template/end nil)
        r (template/reapply model tpl)]
    (is (= #{:b-2} (:changed r)))
    (is (= [:conn-2] (get-in r [:connections :remove])))
    (is (= "310UB40.4" (get-in r [:model :xlines :b-2 :assembly/size])))))
