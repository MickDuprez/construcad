(ns construcad.edits-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.edits :as edits]
            [construcad.records :as records]
            [construcad.seed :as seed]
            [construcad.template-edit :as te]
            [modeller.template]
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
    (testing "B2 is copied from the end plate template, with plates and bolts"
      (is (= "Beam, end plates both ends"
             (get-in m [:xlines (by-mark m "B2") :assembly/from-template :template/name])))
      (is (every? :connection/parts (vals (:connections m))) "every seeded connection holds its own copy, a tree")
      (is (= [:b-2-xline :main :start/plate :start/bolts :end/plate :end/bolts]
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
    (testing "editing a connection template changes the library only: B2 keeps its own copies"
      (let [t (assoc-in (get-in m [:templates ep]) [:template/params :plate-t] 16)
            p (edits/plan m {:edit/type :template/save :template t})
            patch (edits/patch m p)]
        (is (= #{} (:changed p)))
        (is (= [[:template ep]] (:writes p)))
        (is (empty? (:upsert patch)))
        (is (= 16 (get-in patch [:templates ep :template/params :plate-t])) "the patch carries the library")
        (is (= (get-in (edits/view m [b2]) [:instructions b2])
               (get-in (edits/view (:model p) [b2]) [:instructions b2])))
        (let [project (random-uuid)
              [doc] (records/write-docs building (:model p) (:writes p) :project project)]
          (is (= project (:template/project doc))))))
    (testing "nor does editing an assembly template"
      (let [bp (template-named m "Beam, end plates both ends")
            t (assoc (get-in m [:templates bp]) :template/end nil)
            p (edits/plan m {:edit/type :template/save :template t})]
        (is (= #{} (:changed p)))
        (is (= (get-in m [:connections]) (get-in p [:model :connections])))))
    (testing "a thumbnail is kept with the template, if it is a small image"
      (let [png "data:image/png;base64,iVBORw0KGgo="
            p (edits/plan m {:edit/type :template/save :template (assoc (get-in m [:templates ep]) :template/thumbnail png)})]
        (is (= png (get-in p [:model :templates ep :template/thumbnail])))
        (is (= :bad-thumbnail
               (get-in (edits/plan m {:edit/type :template/save
                                      :template (assoc (get-in m [:templates ep]) :template/thumbnail "javascript:x")})
                       [:error :reason])))))
    (testing "a new assembly template gets an id from the server"
      (let [id (random-uuid)
            p (edits/plan m {:edit/type :template/save :new-id (constantly id)
                             :template {:template/kind :assembly :template/name "Beam, cut one end"
                                        :template/main {:part/insert :top-centre}
                                        :template/start {:connection/template ep}}})]
        (is (= [[:template id]] (:writes p)))))
    (testing "an old action list is stored as its tree"
      (let [id (random-uuid)
            p (edits/plan m {:edit/type :template/save :new-id (constantly id)
                             :template {:template/kind :connection :template/name "Old"
                                        :template/params {:gap 5}
                                        :template/actions [{:action/type :cut-to-face}]}})
            t (get-in p [:model :templates id])]
        (is (= :cut (get-in t [:template/parts 0 :prim/type])))
        (is (not (contains? t :template/actions)))))
    (testing "refused: unknown action or prim, another project's template id, missing connection template"
      (is (= :unknown-type
             (get-in (edits/plan m {:edit/type :template/save
                                    :template {:template/kind :connection :template/name "x"
                                               :template/parts [{:prim/id :w :prim/type :weld-everything}]}})
                     [:error :reason])))
      (is (= :duplicate-id
             (get-in (edits/plan m {:edit/type :template/save
                                    :template {:template/kind :connection :template/name "x"
                                               :template/parts [{:prim/id :c :prim/type :cut}
                                                                {:prim/id :p :prim/type :plate
                                                                 :prim/children [{:prim/id :c :prim/type :cut}]}]}})
                     [:error :reason])))
      (is (= :unknown-type
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


(deftest assembly-page-edits
  (let [{m :model} (seeded)
        b2 (by-mark m "B2")
        state (modeller.template/assembly-state m b2)
        p (edits/plan m {:edit/type :assembly/update :state (assoc state :assembly/size "250UB31.4")})]
    (is (nil? (:error p)))
    (is (= #{b2} (:changed p)))
    (is (= "250UB31.4" (get-in p [:model :xlines b2 :assembly/size])))
    (testing "refused: unknown role or connection template"
      (is (= :edit/bad-role (get-in (edits/plan m {:edit/type :assembly/update :state (assoc state :xline/role :spaceship)})
                                    [:error :error/type])))
      (is (= :edit/unknown-template
             (get-in (edits/plan m {:edit/type :assembly/update
                                    :state (assoc state :start {:connection/template (random-uuid)})})
                     [:error :error/type]))))
    (testing "new roles can be created"
      (let [c1 (by-mark m "C1") c2 (by-mark m "C2")
            p (edits/plan m {:edit/type :xline/create :xline/id (random-uuid) :role :joist
                             :start {:node/of c1 :node/end :end} :end {:node/of c2 :node/end :end}})]
        (is (nil? (:error p)))
        (is (= "J1" (:assembly/mark (get-in (:model p) [:xlines (first (:changed p))]))))))))

(deftest editing-one-connection
  (let [{m :model building :building} (seeded)
        b1 (by-mark m "B1") b2 (by-mark m "B2")
        c (some #(when (and (= b2 (:connection/owner %)) (= :end (:connection/end %))) %) (vals (:connections m)))
        column (first (:connection/refs c))
        copy (assoc (select-keys c [:connection/name :connection/parts :connection/params])
                    :connection/params (assoc (:connection/params c) :plate-t 20 :rows 3))
        p (edits/plan m {:edit/type :connection/update :connection/id (:connection/id c) :xline/id b2
                         :connection copy})
        patch (edits/patch m p)]
    (is (nil? (:error p)))
    (is (= #{b2} (:changed p)))
    (is (= [[:connection (:connection/id c)]] (:writes p)))
    (testing "B2 and the columns it bolts into regenerate, nothing else"
      (is (= (into #{b2} (vals (get-in (edits/assembly-page m b2) [:state :supports]))) (set (keys (:upsert patch)))))
      (is (contains? (:upsert patch) column))
      (is (not (contains? (:upsert patch) b1))))
    (testing "B2's end plate takes the new copy; its start and the library do not change"
      (let [part (fn [id] (some #(when (= id (:id %)) %) (get-in patch [:upsert b2 :child :children])))]
        (is (= 20 (get-in (part :end/plate) [:part/size :t])))
        (is (= 6 (get-in (part :end/bolts) [:part/size :count])))
        (is (= 12 (get-in (part :start/plate) [:part/size :t]))))
      (is (= (:templates m) (:templates (:model p)))))
    (testing "the column is drilled for six bolts from this end"
      (let [col (some #(when (= :subtract (:op %)) %) (get-in patch [:upsert column :child :children]))]
        (is (= 6 (count (:tools col))))))
    (testing "the stored connection keeps its provenance"
      (let [[doc] (records/write-docs building (:model p) (:writes p))]
        (is (= "End plate, 4 bolts" (get-in doc [:connection/from-template :template/name])))
        (is (= 20 (get-in doc [:connection/params :plate-t])))))
    (testing "refused: someone else's connection, unknown connection, bad recipe"
      (let [plan #(edits/plan m (merge {:edit/type :connection/update :connection/id (:connection/id c)
                                        :xline/id b2 :connection copy} %))]
        (is (= :edit/not-the-owner (get-in (plan {:xline/id b1}) [:error :error/type])))
        (is (= :edit/unknown-connection (get-in (plan {:connection/id (random-uuid)}) [:error :error/type])))
        (is (= :unknown-type (get-in (plan {:connection (assoc copy :connection/parts [{:prim/id :g :prim/type :glue}])})
                                     [:error :reason])))
        (is (= :no-parts (get-in (plan {:connection (assoc copy :connection/parts [])}) [:error :reason])))
        (is (= :no-parts (get-in (plan {:connection (dissoc copy :connection/parts)}) [:error :reason])))))))

(deftest updating-a-connection-with-a-parts-tree
  (let [{m :model building :building} (seeded)
        b2 (by-mark m "B2")
        c (some #(when (and (= b2 (:connection/owner %)) (= :end (:connection/end %))) %) (vals (:connections m)))
        column (first (:connection/refs c))
        tree [{:prim/id :cut :prim/type :cut :prim/place {:at :support-face :offset [0 0 12]}}
              {:prim/id :plate :prim/type :plate :prim/place {:at :support-face}
               :prim/params {:t 12 :shape {:rect ["bf" "(+ d 200)"]}}
               :prim/children [{:prim/id :bolts :prim/type :hole-group
                                :prim/params {:rows 3 :cols 2 :pitch 100 :through [:parent :support] :bolts? true}}
                               {:prim/id :g1 :prim/type :gusset :prim/place {:offset [0 "(/ d 2)" 12] :rotation 90}}]}]
        p (edits/plan m {:edit/type :connection/update :connection/id (:connection/id c) :xline/id b2
                         :connection {:connection/name "Plate and gusset" :connection/params {}
                                      :connection/parts tree}})
        patch (edits/patch m p)
        part (fn [id] (some #(when (= id (:id %)) %) (get-in patch [:upsert b2 :child :children])))]
    (is (nil? (:error p)))
    (is (= tree (get-in p [:model :connections (:connection/id c) :connection/parts])))
    (testing "B2's end is made from the tree"
      (is (= {:t 12 :w 165.0 :h 504.0} (:part/size (part :end/plate))))
      (is (= :plate (:part/kind (part :end/g1))))
      (is (= 6 (get-in (part :end/bolts) [:part/size :count]))))
    (testing "and the column it bolts into is drilled for six"
      (let [col (some #(when (= :subtract (:op %)) %) (get-in patch [:upsert column :child :children]))]
        (is (= 6 (count (:tools col))))))
    (testing "the stored document holds the tree"
      (let [[doc] (records/write-docs building (:model p) (:writes p))]
        (is (= tree (:connection/parts doc)))
        (is (not (contains? doc :connection/actions)))))))

(deftest reapplying-from-the-library
  (let [{m :model} (seeded)
        b2 (by-mark m "B2")
        bp (get-in m [:xlines b2 :assembly/from-template :template/id])
        ep (some (fn [[id t]] (when (= "End plate, 4 bolts" (:template/name t)) id)) (:templates m))
        m (:model (edits/plan m {:edit/type :template/save
                                 :template (assoc-in (get-in m [:templates ep]) [:template/params :plate-t] 16)}))
        p (edits/plan m {:edit/type :assembly/apply-template :xline/ids [b2] :template/id bp :size "310UB40.4"})]
    (is (every? #(= 16 (get-in % [:connection/params :plate-t]))
                (filter #(= b2 (:connection/owner %)) (vals (:connections (:model p)))))
        "re-applying copies the library as it is now")))

(deftest assembly-page
  (let [{m :model} (seeded)
        b2 (by-mark m "B2")
        {:keys [state supports]} (edits/assembly-page m b2)]
    (is (= b2 (:xline/id state)))
    (is (= "B2" (:assembly/mark state)))
    (testing "each end names the member it sits on, with its size, for resolving parameters"
      (is (= #{:start :end} (set (keys supports))))
      (doseq [end [:start :end]]
        (is (= (get-in state [:supports end]) (get-in supports [end :xline/id])))
        (is (= :column (get-in supports [end :xline/role])))
        (is (string? (get-in supports [end :size])))))
    (is (nil? (edits/assembly-page m (random-uuid))))))

(deftest assembly-preview-context
  (let [{m :model} (seeded)
        b2 (by-mark m "B2")
        {:keys [state context]} (edits/assembly-page m b2)
        supports (set (vals (:supports state)))]
    (testing "the context holds the member and what its nodes hang off, nothing else"
      (is (= (conj supports b2) (set (keys (:xlines context)))))
      (is (every? #(= b2 (:connection/owner %)) (vals (:connections context))))
      (is (= 2 (count (:connections context)))))
    (testing "regenerated in the browser, the member alone matches the server's result"
      (let [{:keys [instructions nodes errors]} (te/instance-preview lib (:templates m) context state)
            real (get-in (edits/view m [b2]) [:instructions b2])]
        (is (empty? errors))
        (is (= [b2] (keys instructions)))
        (is (= real (get instructions b2)))
        (is (= 2 (count nodes)))))
    (testing "a draft change shows before it is saved"
      (let [{:keys [instructions]} (te/instance-preview lib (:templates m) context
                                                        (assoc state :assembly/size "250UB31.4" :start nil))
            parts (set (map :id (get-in instructions [b2 :child :children])))]
        (is (not (contains? parts :start/plate)))))))

(deftest a-supports-page-shows-the-holes-others-bolt-into-it
  (let [{m :model} (seeded)
        b2 (by-mark m "B2")
        c2 (some #(when (= b2 (:connection/owner %)) (first (:connection/refs %))) (vals (:connections m)))
        {:keys [state context]} (edits/assembly-page m c2)]
    (testing "the context carries B2's connection into the column, and B2 to work it out"
      (is (contains? (set (keys (:xlines context))) b2))
      (is (= [b2] (distinct (map :connection/owner (vals (:connections context)))))))
    (testing "the seed's column names no insertion point: the page shows it where the core puts it"
      (is (= :centre (get-in state [:main :part/insert]))))
    (testing "regenerated in the browser, the column matches the server's, holes and all"
      (let [{:keys [instructions errors]} (te/instance-preview lib (:templates m) context state)
            real (get-in (edits/view m [c2]) [:instructions c2])]
        (is (empty? errors))
        (is (= [c2] (keys instructions)))
        (is (= :subtract (get-in real [:child :children 1 :op])))
        (is (= real (get instructions c2)))))))

