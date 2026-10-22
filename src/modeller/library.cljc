(ns modeller.library
  "The section library a model regenerates against: compiled builders and
  section records with their built outlines."
  (:require [modeller.profile-builder :as builder]))

(defn library
  "{:builders {name compiled} :sections {designation record} :errors [...]}
  from custom builder sources (name -> script) and section table rows."
  [custom-sources rows]
  (let [builders (builder/builders custom-sources)
        records (map #(builder/section-record builders %) rows)]
    {:builders builders
     :sections (into {} (map (juxt :section/designation identity)) records)
     :errors (into (vec (keep :error (vals builders))) (keep :section/error records))}))
