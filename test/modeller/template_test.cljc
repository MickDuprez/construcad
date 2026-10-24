(ns modeller.template-test
  (:require [clojure.test :refer [deftest is testing]]
            [modeller.assembly :as assembly]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.frame :as frame]
            [modeller.model :as model]
            [modeller.params :as params]
            [modeller.prim :as prim]
            [modeller.template :as template]
            [modeller.vec :as v]))

(defn- part [instr id]
  (some #(when (= id (:id %)) %) (get-in instr [:child :children])))

(defn- counter []
  (let [n (atom 0)] #(keyword (str "conn-" (swap! n inc)))))

;; the fixture's library templates are old action lists, read as trees
(def square-cut-parts (prim/template-parts (get-in fx/model [:templates :square-cut])))

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
    (is (= {:template/id :tpl/beam-end-plates :template/name "Beam, end plates both ends"}
           (:assembly/from-template b2))
        "the assembly notes where it came from")
    (is (not (contains? b2 :assembly/template)) "and holds no live link")
    (is (= "310UB40.4" (:assembly/size b2)))
    (testing "each connection is a copy of the connection template, not a link to it"
      (let [c (get-in model [:connections :conn-1])
            t (get-in model [:templates :conn/end-plate])]
        (is (= "End plate, 4 bolts" (:connection/name c)))
        (is (= (prim/template-parts t) (:connection/parts c)) "its own tree, read from the template's old actions")
        (is (not (contains? c :connection/actions)))
        (is (= (:template/params t) (:connection/params c)))
        (is (= {:template/id :conn/end-plate :template/name "End plate, 4 bolts"} (:connection/from-template c)))
        (is (not (contains? c :connection/template)))))
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
        plate (part b2 :end/plate)
        bolts (part b2 :end/bolts)]
    (is (empty? errors))
    (testing "beam cut back by the plate thickness behind column C-B2's face (8000 - 101.7)"
      (is (v/nearly= 7886.3 (get-in beam [:plane :origin 2])))
      (is (v/nearly= [0 0 -1] (get-in beam [:plane :normal]))))
    (testing "plate sits on the face, centred on the section, sized from formulas"
      (is (= {:t 12 :w 165.0 :h 324.0} (:part/size plate)))
      (is (= :subtract (:op plate)))
      (is (v/nearly= [0 -152.0 7898.3] (get-in plate [:target :frame :o])))
      (is (v/nearly= [0 0 -1] (get-in plate [:target :frame :z])))
      (is (= 4 (count (:tools plate))))
      (testing "hole cutters overrun both faces by 10, bolts keep their own length"
        (let [tool (first (:tools plate))]
          (is (v/nearly= 32.0 (get-in tool [:child :length])))
          (is (v/nearly= 7908.3 (get-in tool [:frame :o 2]))))))
    (testing "four bolts at gauge 90 and pitch d - 2tf - 80"
      (is (= 4 (get-in bolts [:part/size :count])))
      (let [ys (sort (distinct (map #(Math/round (* 10 (get-in % [:frame :o 1]))) (:children bolts))))]
        (is (= [(Math/round (* 10 (- -152 101.8))) (Math/round (* 10 (+ -152 101.8)))] ys))))
    (testing "both ends get a plate"
      (is (part b2 :start/plate))
      (is (part b2 :start/bolts)))))

(deftest library-edits-change-no-assembly
  (let [{:keys [model]} (applied)
        before (:b-2 (:instructions (core/regenerate model [:b-2])))
        ;; the library's end plate grows to 20 thick, the assembly template's end becomes a square cut
        m' (-> model
               (assoc-in [:templates :conn/end-plate :template/params :plate-t] 20)
               (assoc-in [:templates :tpl/beam-end-plates :template/end] {:connection/template :square-cut}))]
    (testing "B2 is built from its own copies, so it regenerates as it was"
      (is (= before (:b-2 (:instructions (core/regenerate m' [:b-2]))))))
    (testing "re-applying from the library copies the current version"
      (let [r (template/apply-template m' :b-2 (get-in m' [:templates :tpl/beam-end-plates]) "310UB40.4" (counter))
            conns (get-in r [:model :connections])]
        (is (= 20 (get-in conns [:conn-1 :connection/params :plate-t])))
        (is (= square-cut-parts (get-in conns [:conn-2 :connection/parts])))
        (is (empty? (get-in r [:connections :remove])) "each end keeps its connection id")))))

(deftest a-connection-uses-its-own-recipe
  (let [{:keys [model]} (applied)
        built #(:b-2 (:instructions (core/regenerate % [:b-2])))
        ids #(set (map :id (get-in % [:child :children])))]
    (testing "its own parts: a square cut copy has no plate, whatever the library says"
      (let [m (-> model
                  (assoc-in [:connections :conn-2 :connection/parts] square-cut-parts)
                  (assoc-in [:connections :conn-2 :connection/params] {:gap 10}))]
        (is (contains? (ids (built model)) :end/plate))
        (is (not (contains? (ids (built m)) :end/plate)))
        (is (contains? (ids (built m)) :start/plate))))
    (testing "its own parameters"
      (let [m (assoc-in model [:connections :conn-2 :connection/params :plate-t] 25)]
        (is (= 25 (get-in (part (built m) :end/plate) [:part/size :t])))))
    (testing "a connection from before copies is read through its library template"
      (let [c (get-in fx/model [:connections :b-1-start])
            {:keys [instructions errors]} (core/regenerate fx/model [:b-1])]
        (is (not (contains? c :connection/actions)))
        (is (empty? errors))
        (is (= :slice (:op (part (:b-1 instructions) :main))))
        (is (= {:connection/name nil :connection/parts square-cut-parts
                :connection/params {:gap 10}
                :connection/from-template {:template/id :square-cut :template/name nil}}
               (assembly/own-recipe fx/model c)))))))

(deftest updating-one-connection
  (let [{:keys [model]} (applied)
        copy {:connection/name "Heavy plate" :connection/actions [{:action/type :end-plate}]
              :connection/params {:plate-t 30}}
        tree-copy (assoc copy :connection/parts (prim/actions->parts [{:action/type :end-plate}] {:plate-t 30}))
        {m' :model :keys [changed]} (template/update-connection model :conn-2 copy)
        c (get-in m' [:connections :conn-2])]
    (is (= #{:b-2} changed))
    (is (= "Heavy plate" (:connection/name c)))
    (is (= {:plate-t 30} (:connection/params c)))
    (is (= :end (:connection/end c)))
    (is (= {:template/id :conn/end-plate :template/name "End plate, 4 bolts"} (:connection/from-template c))
        "provenance stays unless the copy gives one")
    (is (= (get-in model [:connections :conn-1]) (get-in m' [:connections :conn-1])) "the other end is untouched")
    (testing "an old connection becomes a copy"
      (let [c (get-in (template/update-connection fx/model :b-1-end {:connection/params {:gap 4}})
                      [:model :connections :b-1-end])]
        (is (not (contains? c :connection/template)))
        (is (= square-cut-parts (:connection/parts c)))
        (is (not (contains? c :connection/actions)))
        (is (= {:gap 4} (:connection/params c)))))
    (testing "a copy sent as an old action list is stored as its tree"
      (let [c (get-in m' [:connections :conn-2])]
        (is (= (:connection/parts tree-copy) (:connection/parts c)))
        (is (not (contains? c :connection/actions)))))
    (testing "a copy with a parts tree is stored as it is"
      (let [tree [{:prim/id :cut :prim/type :cut :prim/place {:at :node :offset [0 0 50]}}]
            c (get-in (template/update-connection model :conn-2 {:connection/parts tree})
                      [:model :connections :conn-2])]
        (is (= tree (:connection/parts c)))
        (is (= "End plate, 4 bolts" (:connection/name c)) "what the copy doesn't give is kept")))
    (is (:error (template/update-connection model :nope copy)))))

(deftest templates-keep-the-member-role
  (let [m (assoc-in fx/model [:xlines :b-2 :xline/role] :joist)
        r (template/apply-template m :b-2 (get-in m [:templates :tpl/beam-end-plates]) "310UB40.4" (counter))]
    (is (= :joist (get-in r [:model :xlines :b-2 :xline/role])))))

(deftest editing-one-assembly
  (let [{:keys [model]} (applied)
        state (template/assembly-state model :b-2)]
    (testing "the page's state reads back what the template made"
      (is (= "310UB40.4" (:assembly/size state)))
      (is (= :top-centre (get-in state [:main :part/insert])))
      (is (= "End plate, 4 bolts" (get-in state [:start :connection/name])))
      (is (= :conn-1 (get-in state [:start :connection/id])))
      (is (= :tpl/beam-end-plates (get-in state [:assembly/from-template :template/id])))
      (is (= {:start :c-a2 :end :c-b2} (:supports state))))
    (testing "an edit changes size, placement and one end, and keeps its provenance"
      (let [r (template/update-assembly model (-> state
                                                  (assoc :assembly/size "250UB31.4" :assembly/mark "B9")
                                                  (assoc-in [:main :part/rotation] 180)
                                                  (assoc :end {:connection/template :square-cut}))
                                        (counter))
            x (get-in r [:model :xlines :b-2])]
        (is (= "250UB31.4" (:assembly/size x)))
        (is (= "B9" (:assembly/mark x)))
        (is (= 180 (get-in x [:assembly/parts 0 :part/rotation])))
        (is (= :tpl/beam-end-plates (get-in x [:assembly/from-template :template/id])))
        (is (= :tpl/beam-end-plates (get-in (template/assembly-state (:model r) :b-2) [:assembly/from-template :template/id]))
            "the page says where the assembly came from")
        (testing "an end given a library template gets a copy of it, on the same connection"
          (is (= square-cut-parts (get-in r [:model :connections :conn-2 :connection/parts])))
          (is (= :square-cut (get-in r [:model :connections :conn-2 :connection/from-template :template/id]))))
        (testing "the other end keeps its own copy"
          (is (= (get-in model [:connections :conn-1]) (get-in r [:model :connections :conn-1]))))))
    (testing "a legacy assembly's bare template id reads as provenance"
      (let [m (assoc-in fx/model [:xlines :b-1 :assembly/template] :tpl/beam-end-plates)]
        (is (= {:template/id :tpl/beam-end-plates :template/name "Beam, end plates both ends"}
               (:assembly/from-template (template/assembly-state m :b-1))))))
    (testing "an end set to nothing loses its connection"
      (let [r (template/update-assembly model (assoc state :start nil) (counter))]
        (is (= [:conn-1] (get-in r [:connections :remove])))))))

(deftest bolt-holes-in-the-support
  (let [{:keys [model]} (applied)
        {:keys [instructions errors]} (core/regenerate model [:b-2 :c-b2])
        col (part (:c-b2 instructions) :main)
        b2 (:b-2 instructions)
        world (fn [instr local-frame] (frame/compose (:frame instr) local-frame))]
    (is (empty? errors))
    (testing "column C-B2's main part is drilled for the four end-plate bolts"
      (is (= :subtract (:op col)))
      (is (= 4 (count (:tools col)))))
    (testing "each hole lines up with a bolt, through plate gap and flange"
      (let [holes (for [t (:tools col)] (world (:c-b2 instructions) (:frame t)))
            bolts (for [b (:children (part b2 :end/bolts))] (world b2 (:frame b)))
            on-axis? (fn [hole bolt]
                       (and (v/parallel? (:z hole) (:z bolt))
                            (v/nearly-zero? (v/cross (v/v- (:o hole) (:o bolt)) (:z bolt)))))]
        (is (every? (fn [b] (some #(on-axis? % b) holes)) bolts))
        (testing "the cutter runs 10 past the plate's support face and 10 past the flange"
          (let [bolt (first bolts)
                hole (some #(when (on-axis? % bolt) %) holes)
                ;; the bolt starts at the plate's outer face, 12 from its support face
                along (v/dot (v/v- (:o hole) (:o bolt)) (:z bolt))]
            (is (v/nearly= (+ 12 11.0 10) along 1e-6))))
        (is (v/nearly= (+ 10 0 11.0 10) (get-in (first (:tools col)) [:child :length])))))
    (testing "editing the beam regenerates the column it is bolted to"
      (is (contains? (model/affected model [:b-2]) :c-b2)))))
