(ns modeller.model
  "Resolving node references into XLine positions and frames, in dependency
  order, and working out what an edit affects.

  A model is the in-memory view of the records one regeneration needs:

    {:grid        grid
     :levels      [{:level/name \"L1\" :level/elevation 0} ...]
     :xlines      {id xline}
     :connections {id connection}
     :templates   {id template}}

  An XLine record:

    {:xline/id       :b1
     :xline/start    node-ref
     :xline/end      node-ref
     :xline/rotation 0           ; roll about the line, degrees
     :xline/ref-up   nil         ; optional override of the reference up
     :xline/role     :beam
     :assembly/mark  \"B1\"
     :assembly/parts [part ...]}"
  (:require [modeller.frame :as frame]
            [modeller.node :as node]))

(defn xline-deps [xline]
  (into (node/depends-on (:xline/start xline))
        (node/depends-on (:xline/end xline))))

(defn- topo-order
  "XLine ids with every node dependency before its dependents.
  Returns {:order [...]} or {:cycle ids}."
  [xlines]
  (loop [order [] done #{} pending (set (keys xlines))]
    (if (empty? pending)
      {:order order}
      (let [ready (filter #(every? (fn [d] (or (done d) (not (contains? xlines d))))
                                   (xline-deps (xlines %)))
                          pending)]
        (if (empty? ready)
          {:cycle pending}
          (let [ready (sort-by str ready)]
            (recur (into order ready) (into done ready) (reduce disj pending ready))))))))

(defn- resolve-one [ctx {:xline/keys [id start end rotation ref-up]}]
  (let [s (node/resolve-ref ctx start nil)
        e (when (:point s) (node/resolve-ref ctx end (:point s)))]
    (if-let [error (or (:error s) (:error e))]
      {:error (assoc error :xline/id id)}
      (let [{f :frame len :length error :error}
            (frame/xline-frame (:point s) (:point e) rotation ref-up)]
        (if error
          {:error (assoc error :xline/id id)}
          {:start (:point s) :end (:point e) :frame f :length len})))))

(defn resolve-xlines
  "Positions and ECS of every XLine. Returns {:resolved {id r} :errors [...]}.
  An XLine that fails, or depends on one that failed, is left out and reported."
  [{:keys [xlines grid levels]}]
  (let [{:keys [order cycle]} (topo-order xlines)]
    (if cycle
      {:resolved {} :errors [{:error/type :model/cyclic-dependency :xline/ids cycle}]}
      (reduce (fn [acc id]
                (let [r (resolve-one {:grid grid :levels levels :resolved (:resolved acc)}
                                     (xlines id))]
                  (if (:error r)
                    (update acc :errors conj (:error r))
                    (assoc-in acc [:resolved id] r))))
              {:resolved {} :errors []}
              order))))

(defn dependents
  "id -> ids that must regenerate when it changes: XLines whose nodes reference
  it, and owners of connections that reference it."
  [{:keys [xlines connections]}]
  (let [add (fn [m from to] (update m from (fnil conj #{}) to))]
    (as-> {} m
      (reduce (fn [m [id x]] (reduce #(add %1 %2 id) m (xline-deps x))) m xlines)
      (reduce (fn [m c] (reduce #(add %1 %2 (:connection/owner c)) m (:connection/refs c)))
              m (vals connections)))))

(defn affected
  "Every XLine id that regenerates after `changed` ids are edited, including
  the changed ones."
  [model changed]
  (let [deps (dependents model)]
    (loop [seen (set changed) queue (vec changed)]
      (if-let [id (first queue)]
        (let [new (remove seen (deps id))]
          (recur (into seen new) (into (subvec queue 1) new)))
        seen))))
