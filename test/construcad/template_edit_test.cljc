(ns construcad.template-edit-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.seed :as seed]
            [construcad.template-edit :as te]
            [modeller.assembly :as assembly]
            [modeller.frame :as frame]
            [modeller.instr :as instr]
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
  (testing "ends with a connection sit on sample columns, which are never returned"
    (let [{:keys [instructions nodes errors]}
          (te/assembly-preview lib templates (:beam-plates templates) {:size "250UB31.4"})
          member (:member instructions)]
      (is (empty? errors))
      (is (= #{:member} (set (keys instructions))))
      (is (part member :main))
      (is (part member :plate-start))
      (is (part member :plate-end))
      (testing "the XLine's nodes, for the preview to mark"
        (is (= [[0.0 0.0 3000.0] [1200.0 0.0 3000.0]] (mapv #(mapv double %) nodes))))))
  (testing "a plain end is left as it is"
    (let [{:keys [instructions]} (te/assembly-preview lib templates (dissoc (:beam-plates templates) :template/end)
                                                      {:size "250UB31.4"})]
      (is (part (:member instructions) :plate-start))
      (is (nil? (part (:member instructions) :plate-end)))))
  (testing "a column template stands alone"
    (let [{:keys [instructions errors]} (te/assembly-preview lib templates (:column templates) {:size "200UC46.2"})]
      (is (empty? errors))
      (is (= #{:member} (set (keys instructions)))))))

;; A stand-in for a long end detail (a deep haunch, say): a box reaching
;; :reach in from the member's node along its XLine.
(defmethod assembly/apply-action ::long-detail [{:keys [params conn owner-length]} _ parts]
  (let [reach (get params :reach 1500)
        id (keyword (str "detail-" (name (:connection/end conn))))
        z0 (if (= :start (:connection/end conn)) 0 (- owner-length reach))]
    (assoc parts id (assoc (instr/transform {:o [0 0 z0] :x [1 0 0] :y [0 1 0] :z [0 0 1]}
                                            (instr/box [100 100 reach]))
                           :id id :part/kind :plate))))

(deftest sample-lengths
  (let [plain {:template/kind :assembly :template/name "Plain" :template/main te/default-main}
        long-end {:template/id :long-end :template/kind :connection :template/name "Long"
                  :template/params {:reach 1500} :template/actions [{:action/type ::long-detail}]}
        templates (assoc templates :long-end long-end)]
    (testing "plain ends: the shortest sample"
      (is (= 1200 (te/preview-length lib templates plain "310UB40.4"))))
    (testing "end plates on a 310UB reach a little way in from each node"
      (let [{:keys [instructions]} (te/assembly-preview lib templates (:beam-plates templates)
                                                        {:size "310UB40.4" :length 6000})
            {:keys [start end]} (te/end-extents (:member instructions) 6000 :main)]
        (is (< 50 start 300))
        (is (< 50 end 300)))
      (is (<= 1200 (te/preview-length lib templates (:beam-plates templates) "310UB40.4") 1300)))
    (testing "a long end detail pushes the sample past it, keeping a clear gap"
      (let [one (assoc plain :template/end {:connection/template :long-end})
            both (assoc one :template/start {:connection/template :long-end})]
        (is (= {:start 0.0 :end 1500.0}
               (update-vals (te/end-extents (get-in (te/assembly-preview lib templates one {:size "310UB40.4" :length 8000})
                                                    [:instructions :member])
                                            8000 :main)
                            double)))
        (is (= 1900 (te/preview-length lib templates one "310UB40.4")))    ; 1500 + 304 deep
        (is (= 3400 (te/preview-length lib templates both "310UB40.4")))
        (testing "an override reaches further"
          (is (= 2900 (te/preview-length lib templates
                                         (assoc one :template/end {:connection/template :long-end
                                                                   :connection/params {:reach 2500}})
                                         "310UB40.4"))))
        (testing "the preview is built on it"
          (is (= 1900.0 (double (get-in (te/assembly-preview lib templates one {:size "310UB40.4"}) [:nodes 1 0])))))))))

(deftest preview-view-directions
  (let [near3? (fn [a b] (every? true? (map #(v/nearly= %1 %2 1e-9) a b)))
        dir (fn [nodes label] (some #(when (= label (first %)) (second %)) (te/preview-views nodes)))]
    (is (= ["Iso" "Plan" "Side" "End"] (map first (te/preview-views [[0 0 3000] [3000 0 3000]]))))
    (is (near3? [1 -1.4 0.9] (dir [[0 0 3000] [3000 0 3000]] "Iso")))
    (is (near3? [0 -1 0] (dir [[0 0 3000] [3000 0 3000]] "Side")))
    (is (near3? [-1 0 0] (dir [[0 0 3000] [3000 0 3000]] "End")))
    (testing "square to an XLine running along -Y"
      (is (near3? [0 1 0] (dir [[0 0 0] [0 -8000 0]] "End")))
      (is (near3? [-1 0 0] (dir [[0 0 0] [0 -8000 0]] "Side"))))
    (testing "a vertical XLine is seen as if it ran along X"
      (is (near3? [0 -1 0] (dir [[0 0 0] [0 0 4200]] "Side"))))))

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

(deftest member-space-previews
  (is (frame/valid? te/member-space))
  (testing "the member lies along X, its local Y up, so Side looks at the web from its local -X"
    (is (= [0 0 1] (frame/dir->parent te/member-space [0 1 0])))
    (is (= [0 -1 0] (frame/dir->parent te/member-space [-1 0 0])))
    (is (v/nearly= [0 -1 0] (some #(when (= "Side" (first %)) (second %))
                                  (te/preview-views (:nodes (te/in-member-space {:instructions {} :nodes [[0 0 0] [10 0 0]]}))))
                   1e-9)))
  (let [preview (te/assembly-preview lib templates (:beam-plates templates) {:size "250UB31.4"})
        moved (te/in-member-space preview)
        member (get-in moved [:instructions :member])]
    (testing "only the outer frame changes: the parts stay in the XLine's own space"
      (is (= te/member-space (:frame member)))
      (is (= (get-in preview [:instructions :member :child]) (:child member))))
    (testing "the nodes go to the origin and along X"
      (is (= [[0.0 0.0 0.0] [(double (get-in preview [:nodes 1 0])) 0.0 0.0]] (mapv #(mapv double %) (:nodes moved))))))
  (testing "a vertical XLine lies down too"
    (is (= [[0.0 0.0 0.0] [4200.0 0.0 0.0]] (mapv #(mapv double %) (:nodes (te/in-member-space {:instructions {} :nodes [[5 5 0] [5 5 4200]]}))))))
  (is (= [] (:nodes (te/in-member-space {:instructions {} :nodes []})))))

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
