(ns construcad.seed
  "The demo project: the fixture bay from the core tests (four columns on a
  6 m x 8 m grid, two beams with square-cut connections, a tilt panel), with
  record ids as UUIDs the way stored records have them."
  (:require [construcad.records :as records]
            [modeller.grid :as g]
            [modeller.template :as template]))

(defn starter-templates
  "The library a new project starts with: connection templates, and assembly
  templates built from them. Ids are passed in so stored records keep UUIDs."
  [{:keys [square-cut end-plate beam-cut beam-plates column]}]
  {square-cut {:template/id square-cut :template/kind :connection
               :template/name "Square cut to face"
               :template/params {:gap 10}
               :template/actions [{:action/type :cut-to-face :action/face :auto}]}
   end-plate {:template/id end-plate :template/kind :connection
              :template/name "End plate, 4 bolts"
              :template/params {:gap 0 :plate-t 12 :plate-w "bf" :plate-h "(+ d 20)"
                                :rows 2 :cols 2 :gauge 90 :pitch "(- d (* 2 tf) 80)"
                                :bolt-dia 20}
              :template/actions [{:action/type :end-plate} {:action/type :bolt-group}]}
   beam-cut {:template/id beam-cut :template/kind :assembly
             :template/name "Beam, square cut both ends" :template/role :beam
             :template/main {:part/insert :top-centre}
             :template/start {:connection/template square-cut}
             :template/end {:connection/template square-cut}}
   beam-plates {:template/id beam-plates :template/kind :assembly
                :template/name "Beam, end plates both ends" :template/role :beam
                :template/main {:part/insert :top-centre}
                :template/start {:connection/template end-plate}
                :template/end {:connection/template end-plate}}
   column {:template/id column :template/kind :assembly
           :template/name "Column, plain" :template/role :column
           :template/main {:part/insert :centre}}})

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
  (let [[project building grid-id gl l1 a1 a2 b1 b2 bm1 bm2 panel tpl c1 c2 ep bc bp col]
        (repeatedly 19 random-uuid)
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
               :templates (starter-templates {:square-cut tpl :end-plate ep :beam-cut bc
                                              :beam-plates bp :column col})}
        ;; B1's ends are square cut, each with its own copy of the template
        square-cut (template/connection-copy (:templates model) {:connection/template tpl})
        model (assoc model :connections
                     {c1 (merge {:connection/id c1 :connection/owner bm1 :connection/end :start
                                 :connection/refs [a1]} square-cut)
                      c2 (merge {:connection/id c2 :connection/owner bm1 :connection/end :end
                                 :connection/refs [b1]} square-cut)})
        ;; B2 is built from the end plate assembly template
        model (:model (template/apply-template model bm2 (get-in model [:templates bp]) "310UB40.4"))]
    {:project-id project
     :building-id building
     :docs (concat [(records/project-doc {:id project :name "Demo project"})
                    (records/building-doc {:id building :project project :name "Bay 1" :grid-id grid-id})]
                   (records/model-docs project building model))}))
