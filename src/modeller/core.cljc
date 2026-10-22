(ns modeller.core
  "Entry points: regenerate a whole model, or only what an edit affects.
  Pure functions from model data to reader instructions; the server runs them
  authoritatively and the browser can run the same code for a live preview."
  (:require [modeller.assembly :as assembly]
            [modeller.model :as model]))

(defn regenerate
  "Instructions for the given XLine ids (default: all).
  Returns {:resolved {id r} :instructions {id instr} :errors [...]}."
  ([m] (regenerate m (keys (:xlines m))))
  ([m ids]
   (let [{:keys [resolved errors]} (model/resolve-xlines m)
         built (into {} (for [id ids :when (resolved id)]
                          [id (assembly/assembly-instruction m resolved id)]))]
     {:resolved resolved
      :instructions (update-vals built :instr)
      :errors (into (vec errors) (mapcat :errors (vals built)))})))

(defn edit
  "Apply `f` to the model, then regenerate only the XLines `changed` ids
  affect. Returns the result of `regenerate` plus :model and :affected, the
  shape the server sends to readers as an upsert patch."
  [m changed f]
  (let [m' (f m)
        ids (model/affected m' changed)]
    (assoc (regenerate m' ids) :model m' :affected ids)))
