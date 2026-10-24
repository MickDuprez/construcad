(ns modeller.geometry-sig
  "A comparable signature of an assembly instruction's geometry, in world
  space and independent of how it was built: each part's solids as rounded
  points (extrusion outlines at both ends, cylinder axes as unordered
  end-point pairs) and its cut planes as (normal, distance from origin)."
  (:require [modeller.frame :as frame]
            [modeller.profile :as profile]
            [modeller.vec :as v]))

(defn- r [x] (/ (Math/round (* 100.0 (double x))) 100.0))
(defn- rv [p] (mapv r p))

(defn- solids [f instr]
  (case (:op instr)
    :transform (solids (frame/compose f (:frame instr)) (:child instr))
    :group (mapcat #(solids f %) (:children instr))
    :extrude [[:extrude (set (for [[x y] (profile/tessellate-loop (get-in instr [:profile :outer]) 4)
                                   z [0 (:length instr)]]
                               (rv (frame/point->parent f [x y z]))))]]
    :cylinder [[:cylinder (r (:radius instr))
                #{(rv (frame/point->parent f [0 0 0])) (rv (frame/point->parent f [0 0 (:length instr)]))}]]
    :slice (cons (let [{:keys [origin normal]} (frame/plane->parent f (:plane instr))
                       n (if (= :positive (:keep instr)) normal (v/neg normal))]
                   [:keep-side (rv n) (r (v/dot n origin))])
                 (solids f (:target instr)))
    :subtract (concat (solids f (:target instr)) (map #(vec (cons :tool %)) (mapcat #(solids f %) (:tools instr))))
    []))

(defn- part-key [id]
  (let [n (name id)]
    (cond (namespace id) [(keyword (namespace id)) (keyword n)]
          (re-find #"-(start|end)$" n) (let [[_ k e] (re-find #"^(.*)-(start|end)$" n)] [(keyword e) (keyword k)])
          :else [id])))

(defn signature
  "{part-key #{solid ...}} for an assembly instruction (world space)."
  [instr]
  (let [f (:frame instr)]
    (into {} (for [p (get-in instr [:child :children]) :when (:id p) :when (not= :xline (:op p))]
               [(part-key (:id p)) (set (solids f p))]))))
