(ns construcad.edits-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.edits :as edits]
            [construcad.records :as records]
            [construcad.seed :as seed]
            [modeller.library :as library]
            [modeller.sections-au :as au]
            [modeller.vec :as v]))

(def lib (library/library {} au/rows))

(defn seeded []
  (let [{:keys [docs building-id]} (seed/demo)
        of (fn [t] (filter #(= t (:db/doc-type %)) docs))]
    {:building building-id
     :model (records/docs->model {:grid (first (of :grid))
                                  :levels (of :level)
                                  :assemblies (of :assembly)
                                  :connections (of :connection)
                                  :templates (of :template)}
                                 lib)}))

(defn by-mark [m mark]
  (some (fn [[id x]] (when (= mark (:assembly/mark x)) id)) (:xlines m)))

(deftest seed-regenerates-cleanly
  (let [{m :model} (seeded)
        view (edits/view m (keys (:xlines m)))]
    (is (= 7 (count (:instructions view))))
    (testing "B2 comes from the end plate template, with plates and bolts"
      (is (= "Beam, end plates both ends"
             (get-in m [:templates (get-in m [:xlines (by-mark m "B2") :assembly/template]) :template/name])))
      (is (= [:b-2-xline :main :bolts-end :bolts-start :plate-end :plate-start]
             (map #(if (= :xline (:op %)) :b-2-xline (:id %))
                  (get-in view [:instructions (by-mark m "B2") :child :children])))))
    (is (empty? (:errors view)))
    (is (= #{"GL" "L1"} (set (map :level/name (:levels view)))))))

(deftest moving-a-column-top-moves-the-beams-on-it
  (let [{m :model} (seeded)
        c1 (by-mark m "C1") b1 (by-mark m "B1") b2 (by-mark m "B2")
        p (edits/plan m {:edit/type :xline/set-node :xline/id c1 :xline/end :end
                         :node {:node/grid [:y1 :x1] :node/level "L1" :node/offset [0 0 600]}})
        patch (edits/patch m p)]
    (is (= #{c1} (:changed p)))
    (is (= [[:assembly c1]] (:writes p)))
    (testing "the beam hanging off C1 regenerates; the other bay does not"
      (is (contains? (:upsert patch) c1))
      (is (contains? (:upsert patch) b1))
      (is (not (contains? (:upsert patch) b2))))
    (is (v/nearly= [0 0 4800] (get-in patch [:resolved b1 :start])))))

(deftest moving-a-beam-end-off-its-column-drops-the-connection
  (let [{m :model} (seeded)
        b1 (by-mark m "B1")
        p (edits/plan m {:edit/type :xline/set-node :xline/id b1 :xline/end :end
                         :node {:node/at [0 4000 4200]}})]
    (is (= 1 (count (filter #(= :connection/delete (first %)) (:writes p)))))
    (is (= 3 (count (:connections (:model p)))) "B1 keeps one, B2's two are untouched")
    (is (empty? (:errors (edits/patch m p))))))

(deftest refused-edits
  (let [{m :model} (seeded)
        c1 (by-mark m "C1") b1 (by-mark m "B1")]
    (is (= :edit/bad-node
           (get-in (edits/plan m {:edit/type :xline/set-node :xline/id c1 :xline/end :end
                                  :node {:node/grid [:y1 :x9] :node/level "L1"}})
                   [:error :error/type])))
    (is (= :edit/self-reference
           (get-in (edits/plan m {:edit/type :xline/set-node :xline/id c1 :xline/end :end
                                  :node {:node/of c1 :node/end :start}})
                   [:error :error/type])))
    (testing "a cycle through another XLine"
      (is (= :edit/unresolved
             (get-in (edits/plan m {:edit/type :xline/set-node :xline/id c1 :xline/end :end
                                    :node {:node/on b1 :node/station 100}})
                     [:error :error/type]))))
    (is (= :edit/unknown-type (get-in (edits/plan m {:edit/type :xline/teleport}) [:error :error/type])))))

(deftest grid-and-level-edits
  (let [{m :model} (seeded)
        l1 (some #(when (= "L1" (:level/name %)) (:xt/id %)) (:levels m))]
    (testing "moving grid 2 moves the columns on it, the beam between them, and the panel"
      (let [p (edits/plan m {:edit/type :grid/set-axis-offset :axis/id :x2 :offset 7000})
            patch (edits/patch m p)]
        (is (= 5 (count (:changed p))))
        (is (contains? (:upsert patch) (by-mark m "B2")))
        (is (= 7000 (-> patch :grid :grid/x-axes second :offset)))))
    (testing "raising L1 regenerates every column and both beams"
      (let [p (edits/plan m {:edit/type :level/set-elevation :level/id l1 :elevation 4500})
            patch (edits/patch m p)]
        (is (= 6 (count (:upsert patch))))
        (is (= 4500 (some #(when (= l1 (:xt/id %)) (:level/elevation %)) (:levels patch))))))
    (testing "relabelling keeps ids and regenerates nothing"
      (let [p (edits/plan m {:edit/type :grid/relabel :x-style :letters :y-style :numbers})]
        (is (empty? (:upsert (edits/patch m p))))
        (is (= ["A" "B"] (map :label (get-in p [:model :grid :grid/x-axes]))))))))

(deftest creating-an-xline
  (let [{m :model} (seeded)
        c4 (by-mark m "C4")
        id #?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid))
        p (edits/plan m {:edit/type :xline/create :xline/id id :role :beam
                         :start {:node/on c4 :node/station 2000 :node/from :start}
                         :end {:node/grid [:y2 :x1] :node/level "L1"}})
        patch (edits/patch m p)]
    (is (= [[:assembly/create id]] (:writes p)))
    (is (= "B3" (get-in p [:model :xlines id :assembly/mark])))
    (is (= [id] (:created patch)))
    (is (contains? (:upsert patch) id))
    (is (v/nearly= [6000 8000 2000] (get-in patch [:resolved id :start])))
    (testing "it belongs to the level nearest its start"
      (is (= "GL" (some #(when (= (get-in p [:model :xlines id :assembly/level]) (:xt/id %)) (:level/name %))
                        (:levels m)))))))

(deftest level-load-sets
  (let [{m :model} (seeded)
        [gl l1] (map :xt/id (:levels m))]
    (testing "L1 holds the beams, and loads the columns they hang off"
      (is (= #{"B1" "B2" "C1" "C2" "C3" "C4"}
             (set (map #(get-in m [:xlines % :assembly/mark]) (edits/load-set m l1))))))
    (is (= 5 (count (edits/load-set m gl))))
    (is (= 7 (count (edits/load-set m nil))))))

(deftest write-docs
  (let [{m :model building :building} (seeded)
        c1 (by-mark m "C1")
        p (edits/plan m {:edit/type :xline/set-rotation :xline/id c1 :rotation 90})
        [doc] (records/write-docs building (:model p) (:writes p))]
    (is (= :update (:db/op doc)))
    (is (= :assembly (:db/doc-type doc)))
    (is (= 90 (:xline/rotation doc)))
    (is (= building (:assembly/building doc)))))

(defn- template-named [m name]
  (some (fn [[id t]] (when (= name (:template/name t)) id)) (:templates m)))

(deftest applying-templates
  (let [{m :model building :building} (seeded)
        b1 (by-mark m "B1")
        tid (template-named m "Beam, end plates both ends")
        p (edits/plan m {:edit/type :assembly/apply-template :xline/ids [b1] :template/id tid
                         :size "250UB31.4"})]
    (is (nil? (:error p)))
    (is (= #{b1} (:changed p)))
    (testing "the existing connections are reused, so nothing is deleted"
      (is (= 2 (count (filter #(= :connection (first %)) (:writes p)))))
      (is (empty? (filter #(= :connection/delete (first %)) (:writes p)))))
    (testing "the patch carries the plates and bolts"
      (let [patch (edits/patch m p)
            kids (get-in patch [:upsert b1 :child :children])]
        (is (= #{:xline :slice :subtract :group}
               (set (map :op kids))))))
    (testing "write docs for connections replace whole documents"
      (let [docs (records/write-docs building (:model p) (:writes p))]
        (is (every? #(not= :update (:db/op %)) (filter #(= :connection (:db/doc-type %)) docs)))))
    (testing "refused: unknown template or size"
      (is (= :edit/unknown-template
             (get-in (edits/plan m {:edit/type :assembly/apply-template :xline/ids [b1] :template/id (random-uuid)})
                     [:error :error/type])))
      (is (= :template/unknown-size
             (get-in (edits/plan m {:edit/type :assembly/apply-template :xline/ids [b1] :template/id tid
                                    :size "999UB1"})
                     [:error :error/type]))))))

(deftest saving-templates
  (let [{m :model building :building} (seeded)
        b2 (by-mark m "B2")
        ep (template-named m "End plate, 4 bolts")]
    (testing "editing a connection template regenerates the members using it"
      (let [t (assoc-in (get-in m [:templates ep]) [:template/params :plate-t] 16)
            p (edits/plan m {:edit/type :template/save :template t})
            plate (some #(when (= :plate-end (:id %)) %)
                        (get-in (edits/patch m p) [:upsert b2 :child :children]))]
        (is (= #{b2} (:changed p)))
        (is (= 16 (get-in plate [:part/size :t])))
        (let [project (random-uuid)
              [doc] (records/write-docs building (:model p) (:writes p) :project project)]
          (is (= project (:template/project doc))))))
    (testing "a new assembly template gets an id from the server"
      (let [id (random-uuid)
            p (edits/plan m {:edit/type :template/save :new-id (constantly id)
                             :template {:template/kind :assembly :template/name "Beam, cut one end"
                                        :template/main {:part/insert :top-centre}
                                        :template/start {:connection/template ep}}})]
        (is (= [[:template id]] (:writes p)))))
    (testing "refused: unknown action, another project's template id, missing connection template"
      (is (= :unknown-action
             (get-in (edits/plan m {:edit/type :template/save
                                    :template {:template/kind :connection :template/name "x"
                                               :template/actions [{:action/type :weld-everything}]}})
                     [:error :reason])))
      (is (= :unknown-template
             (get-in (edits/plan m {:edit/type :template/save
                                    :template (assoc (get-in m [:templates ep]) :template/id (random-uuid))})
                     [:error :reason])))
      (is (= :unknown-connection-template
             (get-in (edits/plan m {:edit/type :template/save
                                    :template {:template/kind :assembly :template/name "x"
                                               :template/start {:connection/template (random-uuid)}}})
                     [:error :reason]))))))

