(ns construcad.template-edit-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.seed :as seed]
            [construcad.template-edit :as te]
            [modeller.library :as library]
            [modeller.sections-au :as au]
            [modeller.vec :as v]))

(def lib (library/library {} au/rows))

(def templates
  (seed/starter-templates {:square-cut :square-cut :end-plate :end-plate :beam-cut :beam-cut
                           :beam-plates :beam-plates :column :column}))

(defn- part [instr id]
  (some #(when (= id (:id %)) %) (get-in instr [:child :children])))

(defn- near2? [a b] (and (v/nearly= (a 0) (b 0) 1e-6) (v/nearly= (a 1) (b 1) 1e-6)))

(deftest typed-parameters
  (is (= 12 (te/parse-param " 12 ")))
  (is (= -2.5 (te/parse-param "-2.5")))
  (is (= "(+ d 20)" (te/parse-param "(+ d 20)")))
  (is (= "bf" (te/parse-param "bf")))
  (is (nil? (te/parse-param "  ")))
  (is (= :plate-t (te/param-key "plate-t")))
  (is (nil? (te/param-key "2x")))
  (testing "rows keep the order they were added in, past eight of them"
    (let [ks (map #(keyword (str "p" %)) (range 12))
          params (reduce #(te/set-param %1 %2 1) {} ks)]
      (is (= ks (keys params)))
      (is (= ks (keys (te/set-param params :p3 "(+ d 1)"))))
      (is (= (assoc (vec ks) 3 :q) (keys (te/rename-param params :p3 :q))))
      (is (= (remove #{:p5} ks) (keys (te/remove-param params :p5)))))))

(deftest drafts
  (is (= [:b :a :c] (te/move [:a :b :c] 1 -1)))
  (is (= [:a :b :c] (te/move [:a :b :c] 0 -1)))
  (is (= [:a :c] (te/remove-at [:a :b :c] 1)))
  (is (nil? (:template/id (te/duplicate (:end-plate templates)))))
  (is (= "End plate, 4 bolts (copy)" (:template/name (te/duplicate (:end-plate templates)))))
  (is (empty? (te/draft-errors (te/blank :connection))))
  (is (= 2 (count (te/draft-errors {:template/kind :connection :template/name " "})))))

(deftest end-on-view
  (testing "the chosen insertion point sits on the XLine, plus the offset"
    (let [{:keys [points]} (te/section-view lib "310UB40.4" {:part/insert :top-centre})]
      (is (near2? [0 0] (:top-centre points)))
      (is (near2? [0 -152] (:centre points))))
    (let [{:keys [points]} (te/section-view lib "310UB40.4" {:part/insert :bottom-left :part/offset [10 20]})]
      (is (near2? [10 20] (:bottom-left points)))
      (is (near2? [175 324] (:top-right points)))))
  (testing "rotation rolls the section about the insertion point"
    (let [{:keys [points]} (te/section-view lib "310UB40.4" {:part/insert :top-centre :part/rotation 90})]
      (is (near2? [152 0] (:centre points)))))
  (is (nil? (te/section-view lib "nope" {}))))

(deftest connection-previews
  (let [{:keys [instructions errors]} (te/connection-preview lib templates (:end-plate templates)
                                                             {:size "310UB40.4" :support-size "200UC46.2"})
        beam (:beam instructions)]
    (is (empty? errors))
    (is (= :subtract (:op (part beam :plate-end))))
    (is (= 4 (get-in (part beam :bolts-end) [:part/size :count]))))
  (testing "a new draft (no id yet) previews too, and formula errors name the parameter"
    (let [draft (assoc-in (dissoc (:end-plate templates) :template/id) [:template/params :plate-h] "(+ d")
          {:keys [errors]} (te/connection-preview lib templates draft {:size "310UB40.4" :support-size "200UC46.2"})]
      (is (some #(= :plate-h (:param %)) errors))
      (is (= [:plate-h] (map :param (:errors (te/resolve-for lib draft {} "310UB40.4" "200UC46.2"))))))))

(deftest assembly-previews
  (let [{:keys [instructions errors warnings]}
        (te/assembly-preview lib templates (:beam-plates templates) {:size "250UB31.4" :support-size "200UC46.2"})
        member (:member instructions)]
    (is (empty? errors))
    (is (empty? warnings))
    (is (part member :plate-start))
    (is (part member :plate-end)))
  (testing "a column template stands alone"
    (let [{:keys [instructions errors]} (te/assembly-preview lib templates (:column templates) {:size "200UC46.2"})]
      (is (empty? errors))
      (is (= #{:member} (set (keys instructions)))))))

(deftest end-conditions
  (let [condition {:connection/template :end-plate :connection/params {:plate-t 16}}
        {:keys [template rows errors]} (te/end-summary lib templates condition "310UB40.4" "200UC46.2")
        row (fn [k] (some #(when (= k (:param %)) %) rows))]
    (is (= "End plate, 4 bolts" (:template/name template)))
    (is (empty? errors))
    (is (= 324.0 (:value (row :plate-h))))
    (is (= {:value 16 :template-value 12 :override 16 :overridden? true} (dissoc (row :plate-t) :param)))
    (is (false? (:overridden? (row :gap)))))
  (is (nil? (te/end-summary lib templates nil "310UB40.4" "200UC46.2"))))

(deftest size-pickers
  (let [sections (map #(hash-map :section/designation %) ["200x200x26EA" "460UB82.1" "310UC158" "380PFC" "310UB40.4"])]
    (is (= ["460UB82.1" "310UB40.4" "380PFC" "310UC158" "200x200x26EA"] (te/sizes-for-role sections :beam)))
    (is (= "310UC158" (first (te/sizes-for-role sections :column))))))
