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

  Applying it to an XLine writes the main part with the chosen size and makes
  one connection per end whose node sits on another XLine; the supporting
  XLine is read from the node itself. The XLine remembers the template and
  size, so editing the template later re-applies it everywhere it is used.

  A connection template knows nothing about member roles: only the two ECSs
  it joins and how (cut, plate, fin plate...). So the same template serves a
  beam to column or a joist to beam, and applying a template never changes
  the member's role, which is set when the assembly is created."
  (:require [modeller.node :as node]))

(defn supporting-xline
  "The XLine an end node sits on (:node/of or :node/on), or nil."
  [xline end]
  (let [ref (get xline (if (= :start end) :xline/start :xline/end))]
    (first (node/depends-on ref))))

(defn- existing-connection
  "Id of the connection already on this XLine end, so applying a template
  again replaces it instead of adding another."
  [m xline-id end]
  (some #(when (and (= xline-id (:connection/owner %)) (= end (:connection/end %)))
           (:connection/id %))
        (vals (:connections m))))

(defn apply-template
  "Model with `template` applied to XLine `xline-id` at section `size`.
  Returns {:model m' :changed #{id} :connections {:upsert [ids] :remove [ids]}
  :warnings [...]} or {:error e}. New connection ids come from `new-id`
  (default: a random UUID)."
  ([m xline-id template size] (apply-template m xline-id template size random-uuid))
  ([m xline-id template size new-id]
  (let [xline (get-in m [:xlines xline-id])]
    (cond
      (nil? xline) {:error {:error/type :template/unknown-xline :xline/id xline-id}}
      (not= :assembly (:template/kind template)) {:error {:error/type :template/not-an-assembly}}
      (and (string? size) (:sections m) (not (contains? (:sections m) size)))
      {:error {:error/type :template/unknown-size :size size}}
      :else
      (let [main (merge {:part/id :main :part/kind :extrusion}
                        (:template/main template)
                        {:part/profile size})
            xline' (assoc xline
                          :assembly/parts (into [main] (:template/extra-parts template))
                          :assembly/template (:template/id template)
                          :assembly/size size)
            ends (for [end [:start :end]
                       :let [condition (get template (if (= :start end) :template/start :template/end))
                             cid (or (existing-connection m xline-id end) (new-id))
                             support (supporting-xline xline end)]]
                   {:end end :cid cid :condition condition :support support})
            owned-old (set (for [c (vals (:connections m))
                                 :when (= xline-id (:connection/owner c))]
                             (:connection/id c)))
            new-conns (for [{:keys [end cid condition support]} ends
                            :when (and condition support (contains? (:xlines m) support))]
                        {:connection/id cid :connection/owner xline-id :connection/end end
                         :connection/refs [support]
                         :connection/template (:connection/template condition)
                         :connection/params (:connection/params condition {})})
            new-ids (set (map :connection/id new-conns))
            removed (remove new-ids owned-old)
            m' (-> m
                   (assoc-in [:xlines xline-id] xline')
                   (update :connections #(apply dissoc % removed))
                   (update :connections into (map (juxt :connection/id identity)) new-conns))]
        {:model m'
         :changed #{xline-id}
         :connections {:upsert (vec new-ids) :remove (vec removed)}
         :warnings (vec (for [{:keys [end condition support]} ends
                              :when (and condition (not support))]
                          {:warning/type :template/end-not-on-xline :xline/id xline-id :end end}))})))))

(defn users
  "XLine ids built from template `id`."
  [m id]
  (set (for [[xid x] (:xlines m) :when (= id (:assembly/template x))] xid)))

(defn reapply
  "Re-apply a (changed) assembly template to every XLine that uses it, each at
  its own size. Returns the same shape as apply-template, merged."
  ([m template] (reapply m template random-uuid))
  ([m template new-id]
  (reduce (fn [acc xid]
            (let [r (apply-template (:model acc) xid template (get-in acc [:model :xlines xid :assembly/size]) new-id)]
              (if (:error r)
                (update acc :errors conj (:error r))
                (-> acc
                    (assoc :model (:model r))
                    (update :changed into (:changed r))
                    (update-in [:connections :upsert] into (get-in r [:connections :upsert]))
                    (update-in [:connections :remove] into (get-in r [:connections :remove]))
                    (update :warnings into (:warnings r))))))
          {:model m :changed #{} :connections {:upsert [] :remove []} :warnings [] :errors []}
          (sort-by str (users m (:template/id template))))))

;; --- editing one assembly ------------------------------------------------------

(def placement-keys
  [:part/insert :part/offset :part/rotation :part/start-ext :part/end-ext])

(defn assembly-state
  "One assembly as the assembly page shows it: mark, role, size, the main
  member's placement and each end's connection template and overrides."
  [m xline-id]
  (when-let [x (get-in m [:xlines xline-id])]
    (let [main (first (:assembly/parts x))
          end-state (fn [end]
                      (when-let [c (some #(when (and (= xline-id (:connection/owner %))
                                                     (= end (:connection/end %))) %)
                                         (vals (:connections m)))]
                        {:connection/template (:connection/template c)
                         :connection/params (:connection/params c {})}))]
      {:xline/id xline-id
       :assembly/mark (:assembly/mark x)
       :xline/role (:xline/role x)
       :assembly/template (:assembly/template x)
       :assembly/from-template (:assembly/from-template x)
       :assembly/size (or (:assembly/size x) (:part/profile main))
       :main (select-keys main placement-keys)
       :start (end-state :start)
       :end (end-state :end)
       :supports {:start (supporting-xline x :start) :end (supporting-xline x :end)}})))

(defn update-assembly
  "Apply the assembly page's state to one assembly. Edited by hand, the
  assembly no longer follows its template (it keeps a note of where it came
  from), so a later template edit will not overwrite these choices.
  Same result shape as apply-template."
  ([m state] (update-assembly m state random-uuid))
  ([m {id :xline/id :keys [main start end] mark :assembly/mark role :xline/role size :assembly/size}
    new-id]
   (let [x (get-in m [:xlines id])
         inline {:template/kind :assembly :template/id nil
                 :template/main main :template/start start :template/end end}
         r (apply-template m id inline size new-id)]
     (if (:error r)
       r
       (update-in r [:model :xlines id]
                  #(cond-> (assoc % :assembly/template nil
                                  :assembly/from-template (or (:assembly/template x) (:assembly/from-template x)))
                     mark (assoc :assembly/mark mark)
                     role (assoc :xline/role role)))))))
