(ns modeller.connection-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.seed :as seed]
            [modeller.assembly :as assembly]
            [modeller.connection :as connection]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.frame :as frame]
            [modeller.geometry-sig :as sig]
            [modeller.golden-b2 :as golden]
            [modeller.model :as model]
            [modeller.prim :as prim]
            [modeller.template :as template]
            [modeller.vec :as v]))

;; B-2 runs from column C-A2 (6000, 0) to C-B2 (6000, 8000) at 4200, a
;; 310UB40.4 hung by its top centre. In its construction space Z runs along
;; it (world +Y), Y is up, X is world -X; its end node is at z = 8000, and
;; the face of C-B2 it runs into at z = 7898.3.

(def resolved (:resolved (model/resolve-xlines fx/model)))

(defn- conn [end parts params]
  {:connection/id :t :connection/owner :b-2 :connection/end end
   :connection/refs [(if (= :end end) :c-b2 :c-a2)]
   :connection/name "Test" :connection/parts parts :connection/params (or params {})})

(defn- with-conn [end parts & [params]]
  (assoc-in fx/model [:connections :t] (conn end parts params)))

(defn- ctx [end & [params]]
  (let [c (conn end [] params)]
    (connection/context fx/model resolved :b-2 (get-in fx/model [:xlines :b-2]) c (assembly/own-recipe fx/model c) :main)))

(defn- b2 [m] (core/regenerate m [:b-2 :c-b2]))

(defn- part [instr id]
  (some #(when (= id (:id %)) %) (get-in instr [:child :children])))

(defn- near-frame? [a b]
  (every? #(v/nearly= (get a %) (get b %) 1e-6) [:o :x :y :z]))

(defn- detail [m] (assembly/connection-detail m resolved :t))

(defn- owner-space
  "A prim's ECS from connection-detail, back in B-2's construction space."
  [m id]
  (frame/relative (get-in resolved [:b-2 :frame]) (get-in (detail m) [:frames id])))

;; --- anchors -------------------------------------------------------------------

(deftest anchors
  (testing ":node: Z into the owner, Y the XLine's, X = Y x Z"
    (is (near-frame? {:o [0 0 0] :x [1 0 0] :y [0 1 0] :z [0 0 1]}
                     (connection/anchor-frame (ctx :start) {:at :node})))
    (is (near-frame? {:o [0 0 8000.0] :x [-1 0 0] :y [0 1 0] :z [0 0 -1]}
                     (connection/anchor-frame (ctx :end) {:at :node}))
        "a half turn about Y from the start's, so a detail comes out the same way up at both ends"))
  (testing ":support-face: on the owner's centre line, Z out of the support"
    (is (near-frame? {:o [0 -152.0 7898.3] :x [-1 0 0] :y [0 1 0] :z [0 0 -1]}
                     (connection/anchor-frame (ctx :end) {:at :support-face})))
    (is (near-frame? {:o [0 -152.0 101.7] :x [1 0 0] :y [0 1 0] :z [0 0 1]}
                     (connection/anchor-frame (ctx :start) {:at :support-face :face :auto})))
    (testing "a named face"
      ;; named in C-B2's own section: B-2 runs into its bottom flange (local -Y)
      (let [f (connection/anchor-frame (ctx :end) {:at :support-face :face :flange-bottom-inner})]
        (is (v/nearly= 7909.3 (get-in f [:o 2]) 1e-6) "the inner face of that flange, 11 further in")
        (is (v/nearly= [0 0 1] (:z f) 1e-9)))))
  (testing ":owner-face: a face of the owner's section, at the node"
    (is (near-frame? {:o [3.05 -152.0 8000.0] :x [0 0 -1] :y [0 1 0] :z [1 0 0]}
                     (connection/anchor-frame (ctx :end) {:at :owner-face :face :web-right})))
    (is (near-frame? {:o [0 0 8000.0] :x [1 0 0] :y [0 0 -1] :z [0 1 0]}
                     (connection/anchor-frame (ctx :end) {:at :owner-face :face :top}))
        "on a top face Y runs along the XLine, into the owner"))
  (testing "every anchor is a valid right-handed frame"
    (doseq [end [:start :end]
            place [{:at :node} {:at :support-face} {:at :owner-face} {:at :owner-face :face :bottom}
                   {:at :support-face :face :left}]]
      (is (frame/valid? (connection/anchor-frame (ctx end) place)) (pr-str end place))))
  (testing "no anchor: an unknown one, a parent at the top level"
    (is (nil? (connection/anchor-frame (ctx :end) {:at :moon})))
    (is (nil? (connection/anchor-frame (ctx :end) {:at :parent})))))

(deftest placement
  (let [c (ctx :end {:dz 30})
        node (connection/anchor-frame c {:at :node})]
    (testing "offset in the anchor ECS, then rotation about its Z, then the flip"
      (let [f (connection/place-frame c node {:offset [10 20 :dz] :rotation 90 :flip? true})]
        (is (near-frame? {:o [-10.0 20.0 7970.0] :x [0 1 0] :y [-1 0 0] :z [0 0 1]} f))
        (is (frame/valid? f))))
    (testing "values may be numbers, parameter names or formulas"
      (is (v/nearly= [0.0 -152.0 7970.0]
                     (:o (connection/place-frame c node {:offset [0 "(- (/ d 2))" "dz"]}))
                     1e-9)))))

(deftest values
  (let [c (assoc (ctx :end {:t 12 :h "(+ d 20)"}) :errs (volatile! []))]
    (is (= 5 (connection/value c 5 0)))
    (is (= 12 (connection/value c :t 0)))
    (is (= 12 (connection/value c "t" 0)))
    (is (v/nearly= 324.0 (connection/value c :h 0)) "a formula parameter, resolved")
    (is (v/nearly= 165.0 (connection/value c "(* 1 bf)" 0)) "the owner's section data")
    (is (v/nearly= 22.0 (connection/value c "(* 2 (:tf support))" 0)) "the support's")
    (is (= 7 (connection/value c nil 7)))
    (is (empty? @(:errs c)))
    (testing "an error is recorded and gives the default"
      (is (= 3 (connection/value c :nope 3)))
      (is (= 0 (connection/value c "(+ d" nil)))
      (is (= [:prim/unknown-param :param/formula-failed] (map :error/type @(:errs c)))))))

;; --- prim types --------------------------------------------------------------------

(deftest cuts
  (testing "at the node, pushed out by the offset, keeping the owner's side"
    (let [beam (part (get-in (b2 (with-conn :end [{:prim/id :c :prim/type :cut
                                                   :prim/place {:at :node :offset [0 0 500]}}]))
                             [:instructions :b-2])
                     :main)]
      (is (= :slice (:op beam)))
      (is (v/nearly= 7500.0 (get-in beam [:plane :origin 2])))
      (is (v/nearly= [0 0 -1] (get-in beam [:plane :normal])))
      (is (= :positive (:keep beam)))))
  (testing "cut to face = a cut at the support face with a gap"
    (let [cut-at #(get-in (part (get-in (b2 (with-conn :end [%] {:gap 25})) [:instructions :b-2]) :main)
                          [:plane :origin 2])]
      (is (v/nearly= 7873.3 (cut-at {:prim/id :c :prim/type :cut :prim/place {:at :support-face :offset [0 0 :gap]}})))
      (is (v/nearly= 7873.3 (cut-at {:prim/id :c :prim/type :cut :prim/place {:at :support-face}
                                     :prim/params {:gap :gap}}))
          ":gap is the same as an offset along Z")))
  (testing "flipped, it keeps the other side"
    (let [beam (part (get-in (b2 (with-conn :end [{:prim/id :c :prim/type :cut
                                                   :prim/place {:at :node :offset [0 0 500] :flip? true}}]))
                             [:instructions :b-2])
                     :main)]
      (is (v/nearly= [0 0 1] (get-in beam [:plane :normal])))))
  (testing "a cut of its parent plate"
    (let [plate (part (get-in (b2 (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :node}
                                                    :prim/children [{:prim/id :snipe :prim/type :cut
                                                                     :prim/place {:offset [0 100 0] :rotation 180}
                                                                     :prim/params {:target :parent :plane :xz}}]}]))
                              [:instructions :b-2])
                      :end/p)]
      (is (= :slice (:op plate)))
      (is (= :plate (:part/kind plate)))
      (is (v/nearly= [0 -1 0] (get-in plate [:plane :normal]) 1e-9)))))

(deftest plates
  (let [m (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :node :offset [0 0 20]}
                            :prim/params {:t 8 :shape {:rect [200 "(+ d 20)"]}}}
                           {:prim/id :tri :prim/type :plate :prim/place {:at :node}
                            :prim/params {:shape {:polygon [[0 0] [100 0] [0 50]]}}}])
        {:keys [instructions errors]} (b2 m)
        p (part (:b-2 instructions) :end/p)
        tri (part (:b-2 instructions) :end/tri)]
    (is (empty? errors))
    (testing "a rectangle, centred on its ECS, extruded t along +Z"
      (is (= {:t 8 :w 200 :h 324.0} (:part/size p)))
      (is (= :plate (:part/kind p)))
      (is (near-frame? {:o [0 0 7980.0] :x [-1 0 0] :y [0 1 0] :z [0 0 -1]} (:frame p)))
      (is (= 8 (get-in p [:child :length]))))
    (testing "a polygon, as drawn, 10 thick by default"
      (is (= {:t 10 :w 100 :h 50} (:part/size tri)))
      (is (= [[0 0] [100 0] [0 50]] (get-in tri [:child :profile :outer]))))
    (testing "the default rectangle is the owner's flange width by its depth"
      (let [p (part (get-in (b2 (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :node}}]))
                            [:instructions :b-2])
                    :end/p)]
        (is (= {:t 10 :w 165.0 :h 304.0} (:part/size p)))))))

(deftest gussets
  (let [m (with-conn :end [{:prim/id :g :prim/type :gusset :prim/place {:at :node}
                            :prim/params {:w 120 :h 80 :t 10}}])
        g (part (get-in (b2 m) [:instructions :b-2]) :end/g)]
    (is (= {:t 10 :w 120 :h 80} (:part/size g)))
    (testing "drawn in its ECS's XZ plane, legs along X and Z, thickness centred on that plane"
      (is (near-frame? {:o [0 5.0 8000.0] :x [-1 0 0] :y [0 0 -1] :z [0 -1 0]} (:frame g)))
      (is (= [[0 0] [120 0] [0 80]] (get-in g [:child :profile :outer]))))
    (testing "its children see its profile's ECS: holes run through its thickness"
      (let [m (with-conn :end [{:prim/id :g :prim/type :gusset :prim/place {:at :node}
                                :prim/children [{:prim/id :h :prim/type :hole-group
                                                 :prim/place {:offset [30 20 0]}
                                                 :prim/params {:rows 1 :cols 1}}]}])
            g (part (get-in (b2 m) [:instructions :b-2]) :end/g)
            tool (first (:tools g))]
        (is (= :subtract (:op g)))
        (is (v/nearly= [0 -1 0] (get-in tool [:frame :z]) 1e-9))
        (is (v/nearly= 30.0 (get-in tool [:child :length])) "10 thick, overrun 10 each side")))))

(deftest hole-groups
  (testing "through the parent plate, rows x cols, with bolts"
    (let [m (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :node} :prim/params {:t 12}
                              :prim/children [{:prim/id :h :prim/type :hole-group
                                               :prim/params {:rows 3 :cols 2 :bolt-dia 16 :bolts? true}}]}])
          {:keys [instructions errors]} (b2 m)
          plate (part (:b-2 instructions) :end/p)
          bolts (part (:b-2 instructions) :end/h)]
      (is (empty? errors))
      (is (= 6 (count (:tools plate))))
      (is (v/nearly= 9.0 (get-in plate [:tools 0 :child :radius])) "bolt + 2")
      (is (v/nearly= 32.0 (get-in plate [:tools 0 :child :length])))
      (is (= {:dia 16 :length 42 :count 6} (:part/size bolts)) "the plate plus 30")
      (is (= :bolts (:part/kind bolts)))))
  (testing "through the owner's web from a fin plate on its face"
    (let [m (with-conn :end [{:prim/id :fin :prim/type :plate :prim/place {:at :owner-face :face :web-right}
                              :prim/params {:t 10 :shape {:rect [100 200]}}
                              :prim/children [{:prim/id :h :prim/type :hole-group
                                               :prim/params {:rows 2 :cols 1 :through [:parent :owner-main]
                                                             :bolts? true}}]}])
          {:keys [instructions errors]} (b2 m)
          beam (part (:b-2 instructions) :main)
          bolts (part (:b-2 instructions) :end/h)]
      (is (empty? errors))
      (is (= :subtract (:op beam)))
      (is (= 2 (count (:tools beam))))
      (is (v/nearly= 26.1 (get-in beam [:tools 0 :child :length]) 1e-9) "the web, 6.1, overrun both sides")
      (is (v/nearly= 46.1 (get-in bolts [:part/size :length]) 1e-9) "plate and web, plus 30")))
  (testing "through the support: its own assembly is drilled"
    (let [m (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :support-face}
                              :prim/children [{:prim/id :h :prim/type :hole-group
                                               :prim/params {:rows 2 :cols 3 :through [:parent :support]}}]}])
          col (part (get-in (b2 m) [:instructions :c-b2]) :main)]
      (is (= :subtract (:op col)))
      (is (= 6 (count (:tools col))))))
  (testing "errors: nothing to drill, or drilling along a face"
    (is (= [:prim/missing-input]
           (map :error/type (:errors (b2 (with-conn :end [{:prim/id :h :prim/type :hole-group :prim/place {:at :node}}]))))))
    (is (= :h (-> (b2 (with-conn :end [{:prim/id :h :prim/type :hole-group :prim/place {:at :node}}])) :errors first :prim/id))
        "an error names its prim")
    (is (= [:prim/hole-along-face]
           (map :error/type (:errors (b2 (with-conn :end [{:prim/id :h :prim/type :hole-group
                                                            :prim/place {:at :owner-face :face :web-right}
                                                            :prim/params {:through [:support]}}]))))))))

(deftest nesting
  (testing "holes on a plate turned 90 degrees turn with it"
    (let [holes (fn [rotation]
                  (let [m (with-conn :end [{:prim/id :p :prim/type :plate
                                            :prim/place {:at :node :rotation rotation}
                                            :prim/children [{:prim/id :h :prim/type :hole-group
                                                             :prim/params {:rows 1 :cols 2 :gauge 100}}]}])]
                    (set (for [t (:tools (part (get-in (b2 m) [:instructions :b-2]) :end/p))]
                           (mapv #(Math/round (double %)) (get-in t [:frame :o]))))))]
      (is (= #{[50 0 8010] [-50 0 8010]} (holes 0)) "across the plate's X, the owner's X reversed at the end")
      (is (= #{[0 50 8010] [0 -50 8010]} (holes 90)) "up the owner's Y once the plate turns")))
  (testing "a child's placement is in its parent's ECS"
    (let [m (with-conn :end [{:prim/id :p :prim/type :plate :prim/place {:at :node :offset [0 0 100] :rotation 90}
                              :prim/params {:t 12}
                              :prim/children [{:prim/id :h :prim/type :hole-group :prim/place {:offset [40 0 12]}
                                               :prim/params {:rows 1 :cols 1}}]}])]
      (is (near-frame? {:o [0 40.0 7888.0] :x [0 1 0] :y [1 0 0] :z [0 0 -1]} (owner-space m :h))))))

(def starter (seed/starter-templates {:square-cut :sc :end-plate :ep :beam-cut :bc :beam-plates :bp :column :col}))

(deftest connection-details
  (let [{:template/keys [parts params]} (:ep starter)
        {:keys [frames outputs errors]} (detail (with-conn :end parts params))]
    (is (empty? errors))
    (testing "the part each prim makes or works on"
      (is (= {:plate :end/plate :bolts :end/bolts} outputs)))
    (testing "each prim's ECS, in world space"
      (is (= #{:cut :plate :bolts} (set (keys frames))))
      (is (v/nearly= [6000.0 7898.3 4048.0] (:o (:plate frames)) 1e-6))
      (is (v/nearly= [0 -1 0] (:z (:plate frames)) 1e-9)))))

(deftest unknown-types
  (is (= [:prim/unknown-type]
         (map :error/type (:errors (b2 (with-conn :end [{:prim/id :w :prim/type :weld}]))))))
  (is (= [:prim/no-anchor]
         (map :error/type (:errors (b2 (with-conn :end [{:prim/id :c :prim/type :cut :prim/place {:at :parent}}])))))))

;; --- connections saved as action lists ---------------------------------------------

(deftest actions-as-parts
  (let [params {:gap 0 :plate-t 12 :plate-w "bf" :plate-h "(+ d 20)" :rows 2 :cols 2 :gauge 90
                :pitch "(- d (* 2 tf) 80)" :bolt-dia 20}
        ;; the end plate template's tree before its values moved onto the parts
        starter [{:prim/id :cut :prim/type :cut
                  :prim/place {:at :support-face :offset [0 0 "(+ gap plate-t)"]}}
                 {:prim/id :plate :prim/type :plate
                  :prim/place {:at :support-face :offset [0 0 :gap]}
                  :prim/params {:t :plate-t :shape {:rect [:plate-w :plate-h]}}
                  :prim/children [{:prim/id :bolts :prim/type :hole-group :prim/place {:at :parent}
                                   :prim/params {:rows :rows :cols :cols :gauge :gauge :pitch :pitch
                                                 :bolt-dia :bolt-dia :through [:parent :support] :bolts? true}}]}]
        strip (fn strip [parts] (mapv #(cond-> (dissoc % :prim/name)
                                         (:prim/children %) (update :prim/children strip)
                                         (= {} (:prim/params %)) (dissoc :prim/params))
                                      parts))]
    (testing "an end plate and a bolt group: a cut, a plate on the face, holes through it and the support"
      (is (= (strip starter)
             (strip (prim/actions->parts [{:action/type :end-plate} {:action/type :bolt-group}] params)))
          "the old end plate template's tree is what the old actions read as"))
    (testing "a cut to face reads its gap parameter, or the action's own"
      (is (= [0 0 :gap] (get-in (prim/actions->parts [{:action/type :cut-to-face}] {:gap 10}) [0 :prim/place :offset])))
      (is (= [0 0 5] (get-in (prim/actions->parts [{:action/type :cut-to-face :action/gap 5}] {}) [0 :prim/place :offset])))
      (is (= :top (get-in (prim/actions->parts [{:action/type :cut-to-face :action/face :top}] {}) [0 :prim/place :face]))))
    (testing "missing parameters fall back to the old defaults"
      (let [[cut plate] (prim/actions->parts [{:action/type :end-plate} {:action/type :bolt-group}] {})]
        (is (= [0 0 10] (get-in cut [:prim/place :offset])))
        (is (= 10 (get-in plate [:prim/params :t])))
        (is (= {:rows 2 :cols 2 :gauge 90 :pitch 70 :bolt-dia 20 :through [:parent :support] :bolts? true}
               (get-in plate [:prim/children 0 :prim/params])))))
    (testing "a bolt group with no plate before it is a top-level hole group, which then has nothing to drill"
      (is (= :hole-group (get-in (prim/actions->parts [{:action/type :bolt-group}] {}) [0 :prim/type]))))
    (testing "read as trees, ids stay unique"
      (is (= [:cut :cut-2 :plate]
             (map :prim/id (prim/template-parts {:template/actions [{:action/type :cut-to-face} {:action/type :end-plate}]})))))))

(deftest same-geometry-as-the-actions
  (let [counter #(let [n (atom 0)] (fn [] (keyword (str "conn-" (swap! n inc)))))
        bay (fn [m] (let [{:keys [model]} (template/apply-template m :b-2 (get-in m [:templates :tpl/beam-end-plates])
                                                                   "310UB40.4" (counter))
                          {:keys [instructions errors]} (core/regenerate model [:b-2 :c-b2 :b-1])]
                      (is (empty? errors))
                      (update-vals instructions sig/signature)))]
    (testing "B2's end plates, the holes they drill in C-B2 and B1's square cuts are what the action-based core made"
      (is (= golden/signature (bay fx/model)) "the fixture's library still holds old action lists"))
    (testing "and so are the starter library's trees"
      (let [starter (seed/starter-templates {:square-cut :square-cut :end-plate :conn/end-plate :beam-cut :bc
                                             :beam-plates :bp :column :col})
            m (-> fx/model
                  (assoc-in [:templates :conn/end-plate] (:conn/end-plate starter))
                  (assoc-in [:templates :square-cut] (:square-cut starter)))]
        (is (contains? (get-in m [:templates :conn/end-plate]) :template/parts))
        (is (= golden/signature (bay m)))))))
