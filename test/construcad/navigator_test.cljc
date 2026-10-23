(ns construcad.navigator-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.edits :as edits]
            [construcad.edits-test :refer [seeded by-mark]]
            [construcad.navigator :as nav]
            [modeller.grid :as grid]))

(def levels [{:xt/id :gl :level/name "GL" :level/elevation 0}
             {:xt/id :l1 :level/name "L1" :level/elevation 4200}
             {:xt/id :roof :level/name "Roof" :level/elevation 8000}])

(deftest level-at-or-below
  (is (= "GL" (:level/name (grid/level-at-or-below levels 0))))
  (is (= "GL" (:level/name (grid/level-at-or-below levels 4150))))
  (testing "within a millimetre counts as on the level"
    (is (= "L1" (:level/name (grid/level-at-or-below levels 4199.5)))))
  (is (= "Roof" (:level/name (grid/level-at-or-below levels 9000))))
  (testing "under every level: the lowest"
    (is (= "GL" (:level/name (grid/level-at-or-below levels -300)))))
  (is (nil? (grid/level-at-or-below [] 100))))

(deftest assembly-level
  (is (= :l1 (nav/assembly-level levels {:start [0 0 4200] :end [0 0 0]})))
  (is (= :gl (nav/assembly-level levels {:start [0 0 3000]})))
  (is (nil? (nav/assembly-level levels nil))))

(defn- view []
  (let [{m :model} (seeded)]
    {:m m :view (edits/view m (keys (:xlines m)))}))

(defn- shape [tree]
  (for [{:level/keys [name] :keys [groups]} tree]
    [name (for [{:keys [label items]} groups] [label (mapv :mark items)])]))

(deftest tree-groups-by-level-and-role
  (let [{:keys [view]} (view)]
    (testing "columns start at GL, beams at L1; marks sort as people read them"
      (is (= [["GL" [["Columns" ["C1" "C2" "C3" "C4"]] ["Panels" ["P1"]]]]
              ["L1" [["Beams" ["B1" "B2"]]]]]
             (shape (nav/tree view)))))
    (testing "sizes come along, a custom section has none"
      (let [items (mapcat :items (mapcat :groups (nav/tree view)))]
        (is (= "200UC46.2" (:size (first items))))
        (is (= "310UB40.4" (some #(when (= "B2" (:mark %)) (:size %)) items)))
        (is (nil? (some #(when (= "P1" (:mark %)) (:size %)) items)))))
    (testing "level filter"
      (let [l1 (some #(when (= "L1" (:level/name %)) (:xt/id %)) (:levels view))]
        (is (= [["L1" [["Beams" ["B1" "B2"]]]]] (shape (nav/tree view :level-id l1))))))
    (testing "search on mark or size"
      (is (= [["L1" [["Beams" ["B2"]]]]] (shape (nav/tree view :query " b2 "))))
      (is (= [["GL" [["Columns" ["C1" "C2" "C3" "C4"]]]]] (shape (nav/tree view :query "uc46"))))
      (is (= [] (nav/tree view :query "nothing like it"))))))

(deftest tree-edge-cases
  (let [view {:levels levels
              :xlines {:a {:assembly/mark "C10" :xline/role :column}
                       :b {:assembly/mark "C2" :xline/role :column}
                       :c {:assembly/mark "X1" :xline/role :mystery}
                       :d {:assembly/mark "B1" :xline/role :beam}}
              :resolved {:a {:start [0 0 0]} :b {:start [0 0 0]} :c {:start [0 0 8000]}}}]
    (is (= [["GL" [["Columns" ["C2" "C10"]]]]
            ["Roof" [["Other" ["X1"]]]]
            ["No level" [["Beams" ["B1"]]]]]
           (shape (nav/tree view))))
    (is (= [[:roof] [:roof :other]] (nav/tree-path view :c)))
    (is (= [[:gl] [:gl :column]] (nav/tree-path view :a)))))

(deftest properties-summary
  (let [{:keys [m view]} (view)
        b2 (by-mark m "B2")
        b1 (by-mark m "B1")
        names #(get-in m [:templates % :template/name])
        page (edits/assembly-page m b2)
        p (nav/properties view b2 page names)]
    (is (= {:mark "B2" :role :beam :role-label "Beam" :size "310UB40.4" :level "L1"}
           (select-keys p [:mark :role :role-label :size :level])))
    (is (= {:kind :from :name "Beam, end plates both ends"} (dissoc (:template p) :id)))
    (is (== 8000 (Math/round (double (:length p)))))
    (testing "each end: its own connection, where it came from and what it sits on"
      (is (= "End plate, 4 bolts" (get-in p [:ends :start :name])))
      (is (= "End plate, 4 bolts" (get-in p [:ends :start :from :name])))
      (is (some? (get-in p [:ends :start :connection-id])))
      (is (= {:assembly/mark "C2" :size "200UC46.2" :xline/role :column} (get-in p [:ends :start :support])))
      (is (= "C4" (get-in p [:ends :end :support :assembly/mark]))))
    (testing "parts: the main member, then each end's plate and bolts"
      (is (= [[:main "310UB40.4" 1 nil]
              [:plate "PL 12 × 165 × 324" 1 :start] [:bolts "M20 × 53" 4 :start]
              [:plate "PL 12 × 165 × 324" 1 :end] [:bolts "M20 × 53" 4 :end]]
             (map (juxt :kind :size :count :end) (:parts p)))))
    (testing "an assembly not from a template, before its page has arrived"
      (let [p (nav/properties view b1 nil names)]
        (is (= {:kind :none} (:template p)))
        (is (nil? (:ends p)))
        (is (= [[:main "310UB40.4"]] (map (juxt :kind :size) (:parts p))))))
    (testing "an older record's bare template id still names where it came from"
      (let [tid (get-in m [:xlines b2 :assembly/from-template :template/id])
            v (assoc-in view [:xlines b1 :assembly/template] tid)]
        (is (= {:kind :from :id tid :name "Beam, end plates both ends"} (:template (nav/properties v b1 nil names))))))
    (is (nil? (nav/properties view :nope nil names)))))
