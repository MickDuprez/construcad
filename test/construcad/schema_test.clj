(ns construcad.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.records :as records]
            [construcad.schema :as schema]
            [construcad.seed :as seed]
            [malli.core :as malli]))

(defn- valid? [type doc] (malli/validate (get schema/schema type) doc))

(deftest seeded-documents-pass
  (let [{:keys [docs]} (seed/demo)]
    (doseq [doc docs :let [t (:db/doc-type doc)] :when (get schema/schema t)]
      (is (valid? t doc) (pr-str t (:xt/id doc))))
    (testing "the seed's connections and connection templates hold trees"
      (is (every? :connection/parts (filter #(= :connection (:db/doc-type %)) docs)))
      (is (every? :template/parts (filter #(and (= :template (:db/doc-type %)) (= :connection (:template/kind %))) docs))))))

(deftest connections-and-templates
  (let [building (random-uuid)
        base {:connection/id (random-uuid) :connection/owner (random-uuid) :connection/end :end
              :connection/refs [(random-uuid)] :connection/name "x"}
        doc #(records/connection-doc building (merge base %))]
    (is (valid? :connection (doc {:connection/parts [{:prim/id :cut :prim/type :cut}
                                                     {:prim/id :p :prim/type :plate
                                                      :prim/children [{:prim/id :h :prim/type :hole-group}]}]})))
    (is (valid? :connection (doc {:connection/actions [{:action/type :cut-to-face}]})) "an older action list")
    (is (not (valid? :connection (doc {:connection/parts [{:prim/type :cut}]}))) "a prim needs an id")
    (is (not (valid? :connection (doc {:connection/parts {:prim/id :cut :prim/type :cut}}))) "parts are a vector")
    (is (valid? :template (records/template-doc (random-uuid) {:template/id (random-uuid) :template/kind :connection
                                                              :template/name "t"
                                                              :template/parts [{:prim/id :cut :prim/type :cut}]})))))
