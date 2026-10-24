(ns modeller.template
  "Assembly templates: a reusable recipe for an XLine-rooted assembly. A
  template fixes how the main member sits on its XLine and which connection
  template each end gets; the section size is chosen when it is applied.

    {:template/id    :tpl/beam-end-plates
     :template/kind  :assembly
     :template/name  \"Beam, end plates both ends\"
     :template/role  :beam          ; a hint for size pickers only
     :template/main  {:part/insert :top-centre :part/offset [0 0] :part/rotation 0
                      :part/start-ext 0 :part/end-ext 0}
     :template/start {:connection/template :conn/end-plate :connection/params {}}
     :template/end   {:connection/template :conn/end-plate :connection/params {}}}

  Templates are starting points, copied and never linked. Applying one to
  an XLine writes the main part with the chosen size and makes one
  connection per end whose node sits on another XLine (the supporting XLine
  is read from the node itself), each with its own copy of the connection
  template's name, actions and parameters. The assembly and each connection
  note where they came from (:assembly/from-template,
  :connection/from-template: id and name) and nothing more, so editing a
  library template later changes no assembly. Re-applying it to an
  assembly copies its current version again.

  An end condition names a library connection template (with optional
  parameter overrides), or is itself a copy ({:connection/name
  :connection/parts :connection/params}), as an assembly's own ends are.

  A connection template knows nothing about member roles: only the two ECSs
  it joins and how (cut, plate, fin plate...). So the same template serves a
  beam to column or a joist to beam, and applying a template never changes
  the member's role, which is set when the assembly is created."
  (:require [modeller.assembly :as assembly]
            [modeller.prim :as prim]
            [modeller.node :as node]))

(defn provenance
  "A note of the template something was copied from: id and name."
  [t]
  (when t {:template/id (:template/id t) :template/name (:template/name t)}))

(defn connection-copy
  "The connection an end condition makes: its own name, parts and
  parameters, and where they came from. A condition naming a library
  template is copied from it (its parameters with the condition's overrides
  on top); a condition that is already a copy is kept as it is. nil for no
  condition or an unknown template."
  [templates condition]
  (cond
    (nil? condition) nil
    (assembly/copy? condition) (assembly/normalise-copy condition)
    :else (when-let [t (get templates (:connection/template condition))]
            (when (= :connection (:template/kind t :connection))
              {:connection/name (:template/name t)
               :connection/parts (prim/template-parts t)
               :connection/params (merge (:template/params t) (:connection/params condition))
               :connection/from-template (provenance t)}))))

(defn copy->template
  "A connection's copy as a connection template draft (no id), to edit in
  the connection editor or save to the library."
  [{:connection/keys [name params] :as copy}]
  {:template/kind :connection :template/name name
   :template/parts (prim/connection-parts copy) :template/params (or params {})})

(defn template->copy
  "A connection template draft back as a connection's copy, `from` its
  provenance."
  [t from]
  (cond-> {:connection/name (:template/name t)
           :connection/parts (prim/template-parts t)
           :connection/params (or (:template/params t) {})}
    from (assoc :connection/from-template from)))

(defn assembly-provenance
  "Where an assembly came from, {:template/id :template/name}, or nil.
  Assemblies saved before templates were copied hold a bare id, in
  :assembly/template (then a live link) or :assembly/from-template."
  [m x]
  (let [p (or (:assembly/from-template x) (:assembly/template x))]
    (cond
      (map? p) p
      (some? p) {:template/id p :template/name (get-in m [:templates p :template/name])})))

(defn supporting-xline
  "The XLine an end node sits on (:node/of or :node/on), or nil."
  [xline end]
  (let [ref (get xline (if (= :start end) :xline/start :xline/end))]
    (first (node/depends-on ref))))

(defn existing-connection
  "The connection already on this XLine end, or nil."
  [m xline-id end]
  (some #(when (and (= xline-id (:connection/owner %)) (= end (:connection/end %))) %)
        (vals (:connections m))))

(defn apply-template
  "Model with `template` applied to XLine `xline-id` at section `size`: the
  main part and both ends copied from it. Returns {:model m' :changed #{id}
  :connections {:upsert [ids] :remove [ids]} :warnings [...]} or {:error e}.
  New connection ids come from `new-id` (default: a random UUID); an end
  that already has a connection keeps its id."
  ([m xline-id template size] (apply-template m xline-id template size random-uuid))
  ([m xline-id template size new-id]
  (let [xline (get-in m [:xlines xline-id])
        condition #(get template (if (= :start %) :template/start :template/end))
        copies (into {} (for [end [:start :end]] [end (connection-copy (:templates m) (condition end))]))]
    (cond
      (nil? xline) {:error {:error/type :template/unknown-xline :xline/id xline-id}}
      (not= :assembly (:template/kind template)) {:error {:error/type :template/not-an-assembly}}
      (and (string? size) (:sections m) (not (contains? (:sections m) size)))
      {:error {:error/type :template/unknown-size :size size}}
      (some #(and (condition %) (nil? (copies %))) [:start :end])
      {:error {:error/type :template/unknown-connection-template}}
      :else
      (let [main (merge {:part/id :main :part/kind :extrusion}
                        (:template/main template)
                        {:part/profile size})
            xline' (cond-> (-> xline
                               (dissoc :assembly/template)
                               (assoc :assembly/parts (into [main] (:template/extra-parts template))
                                      :assembly/size size))
                     (:template/id template) (assoc :assembly/from-template (provenance template)))
            ends (for [end [:start :end]
                       :let [cid (or (:connection/id (existing-connection m xline-id end)) (new-id))
                             support (supporting-xline xline end)]]
                   {:end end :cid cid :copy (copies end) :support support})
            owned-old (set (for [c (vals (:connections m))
                                 :when (= xline-id (:connection/owner c))]
                             (:connection/id c)))
            new-conns (for [{:keys [end cid copy support]} ends
                            :when (and copy support (contains? (:xlines m) support))]
                        (merge {:connection/id cid :connection/owner xline-id :connection/end end
                                :connection/refs [support]}
                               copy))
            new-ids (set (map :connection/id new-conns))
            removed (remove new-ids owned-old)
            m' (-> m
                   (assoc-in [:xlines xline-id] xline')
                   (update :connections #(apply dissoc % removed))
                   (update :connections into (map (juxt :connection/id identity)) new-conns))]
        {:model m'
         :changed #{xline-id}
         :connections {:upsert (vec new-ids) :remove (vec removed)}
         :warnings (vec (for [{:keys [end copy support]} ends
                              :when (and copy (not support))]
                          {:warning/type :template/end-not-on-xline :xline/id xline-id :end end}))})))))

(defn update-connection
  "Replace one connection's own copy: name, parts and parameters, and its
  provenance when `copy` gives one. A connection from before copies (a
  library id and overrides), or holding an old action list, becomes a copy
  with a tree of parts. {:model m' :changed #{owner}} or
  {:error e}."
  [m id copy]
  (if-let [c (get-in m [:connections id])]
    (let [copy (cond-> copy (assembly/copy? copy) (assoc :connection/parts (prim/connection-parts copy)))
          recipe (merge (assembly/own-recipe m c) (select-keys copy assembly/recipe-keys))]
      {:model (assoc-in m [:connections id]
                        (merge (apply dissoc c :connection/template :connection/actions assembly/recipe-keys) recipe))
       :changed #{(:connection/owner c)}})
    {:error {:error/type :connection/unknown :connection/id id}}))

;; --- editing one assembly ------------------------------------------------------

(def placement-keys
  [:part/insert :part/offset :part/rotation :part/start-ext :part/end-ext])

(defn assembly-state
  "One assembly as the assembly page shows it: mark, role, size, the main
  member's placement, each end's connection (its id and its own copy) and
  where the assembly came from."
  [m xline-id]
  (when-let [x (get-in m [:xlines xline-id])]
    (let [main (first (:assembly/parts x))
          end-state (fn [end]
                      (when-let [c (existing-connection m xline-id end)]
                        (assoc (assembly/own-recipe m c) :connection/id (:connection/id c))))]
      {:xline/id xline-id
       :assembly/mark (:assembly/mark x)
       :xline/role (:xline/role x)
       :assembly/from-template (assembly-provenance m x)
       :assembly/size (or (:assembly/size x) (:part/profile main))
       ;; a part without an insertion point sits at its centre (profile/insertion-shift)
       :main (merge {:part/insert :centre} (select-keys main placement-keys))
       :start (end-state :start)
       :end (end-state :end)
       :supports {:start (supporting-xline x :start) :end (supporting-xline x :end)}})))

(defn update-assembly
  "Apply the assembly page's state to one assembly: its placement and size,
  and each end's connection (the copy it has, or a library template to
  copy). It keeps its note of where it came from. Same result shape as
  apply-template."
  ([m state] (update-assembly m state random-uuid))
  ([m {id :xline/id :keys [main start end] mark :assembly/mark role :xline/role size :assembly/size}
    new-id]
   (let [x (get-in m [:xlines id])
         strip #(when % (dissoc % :connection/id))
         inline {:template/kind :assembly :template/id nil
                 :template/main main :template/start (strip start) :template/end (strip end)}
         r (apply-template m id inline size new-id)
         from (assembly-provenance m x)]
     (if (:error r)
       r
       (update-in r [:model :xlines id]
                  #(cond-> %
                     from (assoc :assembly/from-template from)
                     mark (assoc :assembly/mark mark)
                     role (assoc :xline/role role)))))))
