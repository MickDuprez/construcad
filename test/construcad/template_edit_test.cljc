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
        beam (:owner instructions)]
    (is (empty? errors))
    (is (= :subtract (:op (part beam :plate-end))))
    (is (= 4 (get-in (part beam :bolts-end) [:part/size :count]))))
  (testing "a new draft (no id yet) previews too, and formula errors name the parameter"
    (let [draft (assoc-in (dissoc (:end-plate templates) :template/id) [:template/params :plate-h] "(+ d")
          {:keys [errors]} (te/connection-preview lib templates draft {:size "310UB40.4" :support-size "200UC46.2"})]
      (is (some #(= :plate-h (:param %)) errors))
      (is (= [:plate-h] (map :param (:errors (te/resolve-for lib draft {} "310UB40.4" "200UC46.2"))))))))

(deftest assembly-previews
  (testing "the member alone: nothing at its ends, so no connection parts"
    (let [{:keys [instructions errors]}
          (te/assembly-preview lib templates (:beam-plates templates) {:size "250UB31.4"})
          member (:member instructions)]
      (is (empty? errors))
      (is (part member :main))
      (is (nil? (part member :plate-start)))
      (is (nil? (part member :plate-end)))))
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
    (is (= {:value 16 :template-value 12 :override 16 :overridden? true :needs-support? false} (dissoc (row :plate-t) :param)))
    (is (false? (:overridden? (row :gap)))))
  (is (nil? (te/end-summary lib templates nil "310UB40.4" "200UC46.2"))))

(deftest size-pickers
  (let [sections (map #(hash-map :section/designation %) ["200x200x26EA" "460UB82.1" "310UC158" "380PFC" "310UB40.4"])]
    (is (= ["460UB82.1" "310UB40.4" "380PFC" "310UC158" "200x200x26EA"] (te/sizes-for-role sections :beam)))
    (is (= "310UC158" (first (te/sizes-for-role sections :column))))
    (is (= "200x200x26EA" (first (te/sizes-for-role sections :brace))))
    (is (= "460UB82.1" (first (te/sizes-for-role sections :joist))))))

(deftest support-free-end-conditions
  (testing "an assembly template's card resolves without a support: support formulas wait for one"
    (let [t (assoc-in (:end-plate templates) [:template/params :plate-dy] "(:tf support)")
          {:keys [rows errors]} (te/end-summary lib (assoc templates :end-plate t)
                                                {:connection/template :end-plate} "310UB40.4" nil)
          row (fn [k] (some #(when (= k (:param %)) %) rows))]
      (is (empty? errors))
      (is (:needs-support? (row :plate-dy)))
      (is (not (:needs-support? (row :plate-h))))
      (is (= 324.0 (:value (row :plate-h))))
      (testing "with a support it resolves"
        (let [{:keys [rows errors]} (te/end-summary lib (assoc templates :end-plate t)
                                                    {:connection/template :end-plate} "310UB40.4" "200UC46.2")]
          (is (empty? errors))
          (is (not-any? :needs-support? rows)))))))

(deftest side-elevations
  (let [{:keys [xline member y edges]} (te/side-elevation lib "310UB40.4" {:part/insert :top-centre
                                                                         :part/start-ext 50 :part/end-ext -20}
                                                          3000)]
    (is (= [0 3000] xline))
    (is (= [-50 2980] member))
    (is (= [-304.0 0.0] y))
    (testing "the flange faces show as lines along the member"
      (is (= [-304.0 -293.8 -10.2 0.0] edges))))
  (testing "rolled 90 degrees the member is as deep as its flange is wide"
    (is (= [-82.5 82.5] (:y (te/side-elevation lib "310UB40.4" {:part/insert :top-centre :part/rotation 90} 3000)))))
  (is (nil? (te/side-elevation lib "nope" {} 3000))))

(deftest framing-into-a-beam
  (let [{:keys [instructions errors]} (te/connection-preview lib templates (:end-plate templates)
                                                             {:size "250UB31.4" :support-size "460UB82.1"
                                                              :arrangement :beam})
        owner (:owner instructions)]
    (is (empty? errors))
    (is (= #{:owner :support} (set (keys instructions))))
    (is (part owner :plate-end))
    (is (= 4 (get-in (part owner :bolts-end) [:part/size :count])))))

(deftest saving-the-assembly-page
  (let [state {:xline/id :b1 :assembly/size "310UB40.4" :main {:part/insert :centre}}]
    (is (= [{:edit/type :assembly/update
             :state (assoc state :main (assoc te/default-main :part/insert :centre))}]
           (te/save-edits state {:rotation 0 :rotation-was 0})))
    (is (= [{:edit/type :xline/set-rotation :xline/id :b1 :rotation 90} :assembly/update]
           (update (te/save-edits state {:rotation 90 :rotation-was 0}) 1 :edit/type)))))
