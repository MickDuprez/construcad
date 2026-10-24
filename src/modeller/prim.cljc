(ns modeller.prim
  "Connections as trees of primitives: the data, editing it, and reading
  connections saved as flat action lists. No geometry here; the primitives
  are evaluated by modeller.connection.

    {:connection/name   \"End plate, 4 bolts\"
     :connection/params {:gap 0 :plate-t 12 ...}   ; numbers or SCI formulas
     :connection/parts  [prim ...]}

    prim = {:prim/id       :plate                 ; unique in its connection
            :prim/type     :plate                 ; :cut :plate :gusset :hole-group ...
            :prim/name     \"End plate\"
            :prim/place    {:at :support-face :face :auto
                            :offset [dx dy dz] :rotation deg :flip? false}
            :prim/params   {:t :plate-t :shape {:rect [:plate-w \"(+ d 20)\"]}}
            :prim/children [prim ...]}

  :prim/place is the same for every type: which ECS the primitive starts
  from (its anchor, see modeller.connection), then an offset in that ECS, a
  rotation about its Z and a flip, in that order. A value in :prim/params
  or :offset is a number, a parameter name (keyword, or a string naming
  one) or a formula string."
  (:require [clojure.string :as str]))

;; --- walking ---------------------------------------------------------------------

(defn walk
  "Every prim of a tree, depth first (parents before children)."
  [parts]
  (mapcat #(cons % (walk (:prim/children %))) parts))

(defn ids [parts] (set (map :prim/id (walk parts))))

(defn path-of
  "The index path ([i :prim/children j ...]) of prim `id` in `parts`, or nil."
  [parts id]
  (some (fn [[i p]]
          (if (= id (:prim/id p))
            [i]
            (when-let [sub (path-of (:prim/children p) id)]
              (into [i :prim/children] sub))))
        (map-indexed vector parts)))

(defn find-prim [parts id] (when-let [p (path-of parts id)] (get-in (vec parts) p)))

(defn parent-of
  "The id of prim `id`'s parent, or nil for a top-level prim (or none)."
  [parts id]
  (some (fn [p] (when (some #(= id (:prim/id %)) (:prim/children p)) (:prim/id p))) (walk parts)))

(defn depth-list
  "[[prim depth] ...] depth first, for drawing the tree."
  ([parts] (depth-list parts 0))
  ([parts d] (mapcat #(cons [% d] (depth-list (:prim/children %) (inc d))) parts)))

;; --- editing ---------------------------------------------------------------------

(defn- vecs [parts] (mapv #(cond-> % (:prim/children %) (update :prim/children vecs)) parts))

(defn update-prim
  "Parts with prim `id` replaced by (f prim & args)."
  [parts id f & args]
  (if-let [p (path-of parts id)]
    (apply update-in (vecs parts) p f args)
    parts))

(defn fresh-id
  "A new prim id for `type`, not yet in `parts`: :plate-1, :plate-2 ..."
  [parts type]
  (let [taken (ids parts)]
    (first (remove taken (map #(keyword (str (name type) "-" %)) (iterate inc 1))))))

(defn add-prim
  "Parts with `prim` added as the last child of `parent-id`, or at the top
  level when `parent-id` is nil."
  [parts parent-id prim]
  (if (nil? parent-id)
    (conj (vecs parts) prim)
    (update-prim parts parent-id update :prim/children (fnil conj []) prim)))

(defn- remove-from [parts id]
  (into [] (keep #(when (not= id (:prim/id %))
                    (cond-> % (:prim/children %) (update :prim/children remove-from id))))
        parts))

(defn remove-prim "Parts without prim `id` (and its children)." [parts id] (remove-from parts id))

(defn- swap-in [v i j] (if (and (< -1 i (count v)) (< -1 j (count v))) (assoc v i (v j) j (v i)) v))

(defn move-prim
  "Parts with prim `id` moved `delta` places among its siblings."
  [parts id delta]
  (if-let [p (path-of parts id)]
    (let [i (peek p) siblings (pop p)]
      (if (empty? siblings)
        (swap-in (vecs parts) i (+ i delta))
        (update-in (vecs parts) siblings swap-in i (+ i delta))))
    parts))

;; --- checking --------------------------------------------------------------------

(defn tree-errors
  "Why `parts` isn't a valid tree of known prim types (a set), as
  [{:reason kw ...}]; empty when it is."
  [parts known]
  (let [all (walk parts)]
    (concat
     (when-not (and (sequential? parts) (seq parts)) [{:reason :no-parts}])
     (for [p all :when (not (map? p))] {:reason :not-a-map})
     (for [p all :when (and (map? p) (not (known (:prim/type p))))] {:reason :unknown-type :prim/type (:prim/type p)})
     (for [p all :when (and (map? p) (not (keyword? (:prim/id p))))] {:reason :no-id})
     (for [p all :when (and (map? p) (some? (:prim/children p)) (not (sequential? (:prim/children p))))]
       {:reason :bad-children :prim/id (:prim/id p)})
     (for [[id n] (frequencies (keep :prim/id (filter map? all))) :when (> n 1)] {:reason :duplicate-id :prim/id id}))))

;; --- connections saved as action lists -------------------------------------------------

(defn- pv
  "A legacy action's value: the parameter it read, when the recipe has it,
  else what the action defaulted to."
  [params k default]
  (if (contains? params k) k default))

(defn- plus
  "A value for the sum of two values (numbers or parameter names)."
  [a b]
  (cond
    (and (number? a) (number? b)) (+ a b)
    (and (number? a) (zero? a)) b
    (and (number? b) (zero? b)) a
    :else (str "(+ " (if (keyword? a) (name a) a) " " (if (keyword? b) (name b) b) ")")))

(defn- face-place [action offset]
  (cond-> {:at :support-face :offset offset}
    (and (:action/face action) (not= :auto (:action/face action))) (assoc :face (:action/face action))))

(defn- hole-group [params]
  {:prim/id :bolts :prim/type :hole-group :prim/name "Bolts"
   :prim/place {:at :parent}
   :prim/params (cond-> {:rows (pv params :rows 2) :cols (pv params :cols 2)
                         :gauge (pv params :gauge 90) :pitch (pv params :pitch 70)
                         :bolt-dia (pv params :bolt-dia 20)
                         :through [:parent :support] :bolts? true}
                  (contains? params :hole-dia) (assoc :dia :hole-dia)
                  (contains? params :bolt-length) (assoc :bolt-length :bolt-length)
                  (contains? params :hole-overrun) (assoc :overrun :hole-overrun))})

(defn actions->parts
  "The tree of prims that builds what a connection's old action list
  (:cut-to-face, :end-plate, :bolt-group) built, reading the same
  parameters (`params`, the recipe's own): a cut to the support face; a cut
  behind a plate on the face; a hole group with bolts through the plate
  before it and the support. An action of another type becomes a prim of
  that type (which then fails as unknown)."
  [actions params]
  (let [params (or params {})]
    (reduce
     (fn [parts {t :action/type :as action}]
       (case t
         :cut-to-face
         (conj parts {:prim/id :cut :prim/type :cut :prim/name "Cut to face"
                      :prim/place (face-place action [0 0 (if (contains? params (:action/gap-param action :gap))
                                                            (:action/gap-param action :gap)
                                                            (:action/gap action 0))])
                      :prim/params (cond-> {} (:action/part action) (assoc :target (:action/part action)))})
         :end-plate
         (let [gap (pv params :gap 0) t (pv params :plate-t 10)]
           (conj parts
                 {:prim/id :cut :prim/type :cut :prim/name "Cut behind the plate"
                  :prim/place (face-place action [0 0 (plus gap t)])
                  :prim/params {}}
                 {:prim/id :plate :prim/type :plate :prim/name "End plate"
                  :prim/place (face-place action [(pv params :plate-dx 0) (pv params :plate-dy 0) gap])
                  :prim/params {:t t :shape {:rect [(pv params :plate-w nil) (pv params :plate-h nil)]}}
                  :prim/children []}))
         :bolt-group
         (if-let [i (last (keep-indexed #(when (= :plate (:prim/type %2)) %1) parts))]
           (update parts i update :prim/children (fnil conj []) (hole-group params))
           (conj parts (hole-group params)))
         (conj parts {:prim/id (keyword (str (name t) "-" (count parts))) :prim/type t
                      :prim/params (dissoc action :action/type)})))
     []
     actions)))

(defn- unique-ids
  "Parts with any repeated prim id renamed (:cut, :cut-2 ...)."
  [parts]
  (let [seen (atom #{})
        fix (fn fix [p]
              (let [id (:prim/id p)
                    id' (first (remove @seen (cons id (map #(keyword (str (name id) "-" %)) (iterate inc 2)))))]
                (swap! seen conj id')
                (cond-> (assoc p :prim/id id')
                  (:prim/children p) (update :prim/children #(mapv fix %)))))]
    (mapv fix parts)))

(defn recipe-parts
  "A recipe's tree: its :parts, or its old :actions read as one. `parts-k`,
  `actions-k` and `params-k` name the keys (connection or template)."
  [recipe parts-k actions-k params-k]
  (cond
    (contains? recipe parts-k) (vec (get recipe parts-k))
    (contains? recipe actions-k) (unique-ids (actions->parts (get recipe actions-k) (get recipe params-k)))
    :else []))

(defn connection-parts [c] (recipe-parts c :connection/parts :connection/actions :connection/params))
(defn template-parts [t] (recipe-parts t :template/parts :template/actions :template/params))

(defn summary
  "Prim types in tree order, for a card: [:cut :plate :hole-group]."
  [parts]
  (mapv :prim/type (walk parts)))

(defn type-label [t] (str/capitalize (str/replace (name t) "-" " ")))
