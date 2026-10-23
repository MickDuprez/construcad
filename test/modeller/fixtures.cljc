(ns modeller.fixtures
  "A small portal frame bay: four columns on a 6 m x 8 m grid, two beams
  between column tops, and a tilt panel along grid A."
  (:require [modeller.grid :as g]
            [modeller.library :as library]
            [modeller.sections-au :as au]))

(def grid
  (g/make-grid {:id :g1 :x-spacings [6000] :y-spacings [8000]}))

(def lib (library/library {} au/rows))

(def levels
  [{:level/name "GL" :level/elevation 0}
   {:level/name "L1" :level/elevation 4200}])

(defn column [id axes]
  {:xline/id id
   :xline/role :column
   :xline/start {:node/grid axes :node/level "GL"}
   :xline/end {:node/grid axes :node/level "L1"}
   :xline/rotation 0
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile "200UC46.2"}]})

(defn beam [id from to]
  {:xline/id id
   :xline/role :beam
   :xline/start {:node/of from :node/end :end}
   :xline/end {:node/of to :node/end :end}
   :xline/rotation 0
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile "310UB40.4"
                     :part/insert :top-centre}]})

(def model
  {:grid grid
   :builders (:builders lib)
   :sections (:sections lib)
   :levels levels
   :xlines (into {}
                 (map (juxt :xline/id identity))
                 [(column :c-a1 [:y1 :x1])
                  (column :c-a2 [:y1 :x2])
                  (column :c-b1 [:y2 :x1])
                  (column :c-b2 [:y2 :x2])
                  (beam :b-1 :c-a1 :c-b1)
                  (beam :b-2 :c-a2 :c-b2)
                  {:xline/id :p-a
                   :xline/role :panel
                   :xline/start {:node/grid [:y1 :x1] :node/level "GL" :node/offset [0 -300 0]}
                   :xline/end {:node/grid [:y1 :x2] :node/level "GL" :node/offset [0 -300 0]}
                   :assembly/parts [{:part/id :panel :part/kind :extrusion
                                     :part/profile {:section/builder :rect :b 150 :d 4800}
                                     :part/insert :bottom-centre}]}])
   :templates {:conn/end-plate {:template/id :conn/end-plate
                                :template/kind :connection
                                :template/name "End plate, 4 bolts"
                                :template/params {:gap 0 :plate-t 12 :plate-w "bf" :plate-h "(+ d 20)"
                                                  :rows 2 :cols 2 :gauge 90 :pitch "(- d (* 2 tf) 80)"
                                                  :bolt-dia 20}
                                :template/actions [{:action/type :end-plate} {:action/type :bolt-group}]}
               :tpl/beam-end-plates {:template/id :tpl/beam-end-plates
                                     :template/kind :assembly
                                     :template/name "Beam, end plates both ends"
                                     :template/role :beam
                                     :template/main {:part/insert :top-centre}
                                     :template/start {:connection/template :conn/end-plate}
                                     :template/end {:connection/template :conn/end-plate}}
               :square-cut {:template/id :square-cut
                            :template/params {:gap 10}
                            :template/actions [{:action/type :cut-to-face :action/face :auto}]}}
   :connections {:b-1-start {:connection/id :b-1-start :connection/owner :b-1 :connection/end :start
                             :connection/refs [:c-a1] :connection/template :square-cut}
                 :b-1-end {:connection/id :b-1-end :connection/owner :b-1 :connection/end :end
                           :connection/refs [:c-b1] :connection/template :square-cut}}})
