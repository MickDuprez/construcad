(ns modeller.node
  "Node references: how an XLine end says where it is. Positions are never
  free numbers when they can be a reference, so moving a grid line, a level or
  a supporting member moves everything that hangs off it.

    {:node/at [x y z]}                              absolute WCS
    {:node/grid [\"A\" \"1\"] :node/level \"L2\"
     :node/offset [dx dy dz]}                       grid intersection at a level,
                                                    offset along grid axes
    {:node/of :c1 :node/end :end}                   coincident with another XLine's end
    {:node/on :b1 :node/station 1500
     :node/from :start :node/offset [dx dy dz]}     at a distance along another XLine,
                                                    offset in that XLine's ECS
    {:node/rel [dx dy dz]}                          (end only) WCS offset from own start"
  (:require [modeller.frame :as frame]
            [modeller.grid :as grid]
            [modeller.vec :as v]))

(defn kind [ref]
  (cond
    (:node/at ref) :at
    (:node/grid ref) :grid
    (:node/of ref) :of
    (:node/on ref) :on
    (:node/rel ref) :rel))

(defn depends-on
  "XLine ids this node reference needs resolved first."
  [ref]
  (case (kind ref)
    :of #{(:node/of ref)}
    :on #{(:node/on ref)}
    #{}))

(defn- err [ref reason] {:error {:error/type :node/unresolved :node ref :reason reason}})

(defn resolve-ref
  "WCS point for a node reference. `ctx` holds :grid, :levels and :resolved
  (xline id -> {:start :end :frame :length}); `own-start` is used by :node/rel.
  Returns {:point p} or {:error e}."
  [{:keys [grid levels resolved]} ref own-start]
  (case (kind ref)
    :at {:point (:node/at ref)}

    :grid
    (let [z (if-let [l (:node/level ref)] (grid/level-elevation levels l) 0)]
      (cond
        (nil? grid) (err ref :no-grid)
        (nil? z) (err ref :unknown-level)
        :else (if-let [p (grid/grid-point grid (:node/grid ref) z (:node/offset ref))]
                {:point p}
                (err ref :unknown-grid-axis))))

    :of
    (if-let [r (get resolved (:node/of ref))]
      {:point (get r (if (= :start (:node/end ref)) :start :end))}
      (err ref :unresolved-xline))

    :on
    (if-let [{:keys [frame length]} (get resolved (:node/on ref))]
      (let [s (:node/station ref 0)
            z (if (= :end (:node/from ref)) (- length s) s)
            [dx dy dz] (or (:node/offset ref) [0 0 0])]
        {:point (frame/point->parent frame [dx dy (+ z dz)])})
      (err ref :unresolved-xline))

    :rel
    (if own-start
      {:point (v/v+ own-start (:node/rel ref))}
      (err ref :rel-without-start))

    (err ref :unknown-kind)))
