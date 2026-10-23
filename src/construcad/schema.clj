(ns construcad.schema
  "Malli schemas Biff checks every transaction document against. Open maps:
  records carry the core's own keys, which the core validates itself.")

(def node-ref [:map {:closed false}])

(def schema
  {:project [:map {:closed false}
             [:xt/id :uuid]
             [:project/name :string]
             [:project/units :keyword]]
   :building [:map {:closed false}
              [:xt/id :uuid]
              [:building/project :uuid]
              [:building/name :string]
              [:building/grid :uuid]]
   :grid [:map {:closed false}
          [:xt/id :uuid]
          [:grid/building :uuid]
          [:grid/origin [:vector number?]]
          [:grid/x-axes [:vector [:map [:id :keyword] [:label :string] [:offset number?]]]]
          [:grid/y-axes [:vector [:map [:id :keyword] [:label :string] [:offset number?]]]]]
   :level [:map {:closed false}
           [:xt/id :uuid]
           [:level/building :uuid]
           [:level/name :string]
           [:level/elevation number?]]
   :assembly [:map {:closed false}
              [:xt/id :uuid]
              [:xline/id :uuid]
              [:assembly/building :uuid]
              [:assembly/level {:optional true} [:maybe :uuid]]
              [:xline/start node-ref]
              [:xline/end node-ref]
              [:assembly/parts [:vector [:map {:closed false} [:part/id :keyword]]]]]
   :connection [:map {:closed false}
                [:xt/id :uuid]
                [:connection/building :uuid]
                [:connection/owner :uuid]
                [:connection/end [:enum :start :end]]
                [:connection/refs [:vector :uuid]]
                ;; its own copy of the recipe; connections saved before copies
                ;; name a library template instead
                [:connection/name {:optional true} :string]
                [:connection/actions {:optional true} [:vector [:map {:closed false} [:action/type :keyword]]]]
                [:connection/template {:optional true} :uuid]]
   :template [:map {:closed false}
              [:xt/id :uuid]
              [:template/project :uuid]
              [:template/kind [:enum :connection :assembly]]
              [:template/name :string]
              [:template/actions {:optional true} [:vector [:map {:closed false} [:action/type :keyword]]]]
              ;; a small PNG preview (data URL), captured when it is saved
              [:template/thumbnail {:optional true} :string]]})

(def module {:schema schema})
