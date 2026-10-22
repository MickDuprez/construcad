(ns construcad.seed
  "The demo project: the fixture bay from the core tests (four columns on a
  6 m x 8 m grid, two beams with square-cut connections, a tilt panel), with
  record ids as UUIDs the way stored records have them."
  (:require [construcad.records :as records]
            [modeller.grid :as g]))

(defn- column [id axes]
  {:xline/id id :xline/role :column
   :xline/start {:node/grid axes :node/level "GL"}
   :xline/end {:node/grid axes :node/level "L1"}
   :xline/rotation 0
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile "200UC46.2"}]})

(defn- beam [id from to]
  {:xline/id id :xline/role :beam
   :xline/start {:node/of from :node/end :end}
   :xline/end {:node/of to :node/end :end}
   :xline/rotation 0
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile "310UB40.4"
                     :part/insert :top-centre}]})

(defn demo
  "{:project-id :building-id :docs} for the demo bay."
  []
  (let [[project building grid-id gl l1 a1 a2 b1 b2 bm1 bm2 panel tpl c1 c2]
        (repeatedly 15 random-uuid)
        grid (assoc (g/make-grid {:id grid-id :x-spacings [6000] :y-spacings [8000]})
                    :grid/point-name "{y}{x}")
        on (fn [level x] (assoc x :assembly/level level))
        model {:grid grid
               :levels [{:xt/id gl :level/name "GL" :level/elevation 0}
                        {:xt/id l1 :level/name "L1" :level/elevation 4200}]
               :xlines (into {}
                             (map (juxt :xline/id identity))
                             [(on gl (assoc (column a1 [:y1 :x1]) :assembly/mark "C1"))
                              (on gl (assoc (column a2 [:y1 :x2]) :assembly/mark "C2"))
                              (on gl (assoc (column b1 [:y2 :x1]) :assembly/mark "C3"))
                              (on gl (assoc (column b2 [:y2 :x2]) :assembly/mark "C4"))
                              (on l1 (assoc (beam bm1 a1 b1) :assembly/mark "B1"))
                              (on l1 (assoc (beam bm2 a2 b2) :assembly/mark "B2"))
                              (on gl {:xline/id panel :xline/role :panel :assembly/mark "P1"
                                      :xline/start {:node/grid [:y1 :x1] :node/level "GL" :node/offset [0 -300 0]}
                                      :xline/end {:node/grid [:y1 :x2] :node/level "GL" :node/offset [0 -300 0]}
                                      :assembly/parts [{:part/id :panel :part/kind :extrusion
                                                        :part/profile {:section/builder :rect :b 150 :d 4800}
                                                        :part/insert :bottom-centre}]})])
               :templates {tpl {:template/id tpl :template/name "square-cut" :template/kind :connection
                                :template/params {:gap 10}
                                :template/actions [{:action/type :cut-to-face :action/face :auto}]}}
               :connections {c1 {:connection/id c1 :connection/owner bm1 :connection/end :start
                                 :connection/refs [a1] :connection/template tpl}
                             c2 {:connection/id c2 :connection/owner bm1 :connection/end :end
                                 :connection/refs [b1] :connection/template tpl}}}]
    {:project-id project
     :building-id building
     :docs (concat [(records/project-doc {:id project :name "Demo project"})
                    (records/building-doc {:id building :project project :name "Bay 1" :grid-id grid-id})]
                   (records/model-docs project building model))}))
