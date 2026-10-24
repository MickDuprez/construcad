(ns modeller.settings-test
  (:require [clojure.test :refer [deftest is testing]]
            [construcad.seed :as seed]
            [modeller.core :as core]
            [modeller.fixtures :as fx]
            [modeller.geometry-sig :as sig]
            [modeller.params :as params]
            [modeller.prim :as prim]
            [modeller.settings :as settings]
            [modeller.template :as template]
            [modeller.vec :as v]))

(deftest defaults
  (is (= {:bolt-dia 20 :hole-clearance 2 :gauge 90 :pitch 70 :edge-distance 35 :plate-t 10 :gusset-t 10
          :hole-overrun 10 :cut-gap 0 :bolt-projection 30}
         settings/defaults)
      "AU practice: M20 8.8/S, 2 mm clearance, 90 gauge, 70 pitch")
  (testing "the effective settings are what the project stored over the defaults"
    (is (= settings/defaults (settings/effective nil)))
    (is (= 100 (:gauge (settings/effective {:gauge 100}))))
    (is (= 90 (:gauge (settings/effective {:gauge nil}))) "nil is the default")
    (is (not (contains? (settings/effective {:nope 3}) :nope)) "an unknown key is dropped"))
  (testing "what can't be saved"
    (is (empty? (settings/errors {:gauge 100 :pitch nil})))
    (is (= [{:setting :nope :reason :unknown-setting}] (settings/errors {:nope 1})))
    (is (= [:not-a-number] (map :reason (settings/errors {:gauge "90"}))))
    (is (= [:negative] (map :reason (settings/errors {:gauge -1}))))
    (is (= [{:reason :not-a-map}] (settings/errors [1 2])))))

(def eff (settings/effective {:gauge 100}))

(defn- field [v & {:as opts}]
  (settings/field-value v (merge {:params {:pitch 75} :settings eff
                                  :binds (params/bindings {:d 304 :bf 165} nil {:pitch 75} eff)}
                                 opts)))

(deftest resolution-order
  (testing "1. the prim's own value: a number or a formula"
    (is (= {:value 80 :from :own} (field 80 :inherited 100)))
    (is (= {:value 324 :from :own} (field "(+ d 20)")))
    (is (= {:value 165 :from :own} (field "bf")) "a bare name of the section data"))
  (testing "2. a name: the connection's shared parameter, else the setting of that name"
    (is (= {:value 75 :from :param} (field :pitch :inherited 70)))
    (is (= {:value 75 :from :param} (field "pitch")))
    (is (= {:value 100 :from :settings} (field :gauge)) "no shared gauge: the setting")
    (is (= {:value 20 :from :settings} (field "bolt-dia"))))
  (testing "3. blank: what the field inherits from settings, 4. else the built-in default"
    (is (= {:value 100 :from :settings} (field nil :inherited 100 :default 90)))
    (is (= {:value 7 :from :default} (field nil :default 7))))
  (testing "formulas see the settings as `settings`"
    (is (= {:value 110 :from :own} (field "(+ (:gauge settings) 10)")))
    (is (= {:value 140 :from :own} (field "(* 2 (:edge-distance settings) 2)"))))
  (testing "errors give the default"
    (is (= :prim/unknown-param (get-in (field :nope :default 3) [:error :error/type])))
    (is (= 3 (:value (field :nope :default 3))))
    (is (= :param/formula-failed (get-in (field "(+ d" :default 1) [:error :error/type])))))

(deftest inherited-values
  (is (= 100 (settings/inherited eff :hole-group :gauge)))
  (is (= 70 (settings/inherited eff :hole-group :pitch)))
  (is (= 10 (settings/inherited eff :plate :t)))
  (is (= 10 (settings/inherited eff :gusset :t)))
  (is (= 0 (settings/inherited eff :cut :gap)))
  (is (nil? (settings/inherited eff :hole-group :rows)) "rows inherit nothing")
  (is (= 22 (settings/inherited eff :hole-group :dia)) "the setting's bolt + clearance")
  (is (= 26 (settings/inherited eff :hole-group :dia {:bolt 24})) "this group's own bolt + clearance")
  (is (= 54 (settings/inherited eff :hole-group :bolt-length {:stack 24})))
  (testing "what a blank field shows"
    (is (= "100 from settings" (settings/placeholder eff {:prim/type :hole-group} :gauge)))
    (is (= "22 = bolt + 2 from settings" (settings/placeholder eff {:prim/type :hole-group} :dia)))
    (is (= "26 = bolt + 2 from settings"
           (settings/placeholder eff {:prim/type :hole-group :prim/params {:bolt-dia 24}} :dia)))
    (is (nil? (settings/placeholder eff {:prim/type :hole-group} :rows)))))

(def starter (seed/starter-templates {:square-cut :sc :end-plate :ep :beam-cut :bc :beam-plates :bp :column :col}))

(defn- recipe [t] {:connection/parts (:template/parts t) :connection/params (:template/params t)})

(deftest which-settings-a-connection-reads
  (testing "the end plate inherits its gauge, bolt and hole sizes and bolt length; its pitch, plate and cut are its own"
    (is (= #{:gauge :bolt-dia :hole-clearance :hole-overrun :bolt-projection} (settings/uses (recipe (:ep starter))))))
  (testing "the square cut has its own gap"
    (is (= #{} (settings/uses (recipe (:sc starter))))))
  (testing "a name no shared parameter has reads the setting"
    (is (= #{:gauge :cut-gap} (settings/uses {:connection/parts [{:prim/id :c :prim/type :cut
                                                                  :prim/place {:offset [0 0 "gauge"]}}]}))))
  (testing "a formula over `settings` might read any of them"
    (is (= :all (settings/uses {:connection/parts [{:prim/id :c :prim/type :cut
                                                     :prim/params {:gap "(:cut-gap settings)"}}]}))))
  (is (settings/reads-any? :all #{:gauge}))
  (is (not (settings/reads-any? #{:pitch} #{:gauge})))
  (is (= #{:gauge} (settings/changed {:gauge 90 :pitch 70} {:gauge 100 :pitch 70})) "only what really changed"))

;; --- geometry ---------------------------------------------------------------------------

(defn- b2-from-starter
  "The fixture bay with B2 made from the starter end plate template, under
  stored `stored-settings`: B2's and C-B2's instructions."
  [stored-settings & [f]]
  (let [n (atom 0)
        m (-> fx/model
              (assoc :settings stored-settings)
              (update :templates merge {:ep (:ep starter) :bp (:bp starter)}))
        m (:model (template/apply-template m :b-2 (get-in m [:templates :bp]) "310UB40.4"
                                           #(keyword (str "conn-" (swap! n inc)))))
        m ((or f identity) m)
        {:keys [instructions errors]} (core/regenerate m [:b-2 :c-b2])]
    (is (empty? errors))
    instructions))

(defn- bolt-xs [instr id]
  (->> (get (sig/signature instr) id)
       (mapcat (fn [[_ _ ends]] (map first ends)))
       set))

(deftest settings-move-what-inherits-them
  (testing "B2's bolts sit at the settings' gauge"
    (is (= #{5955.0 6045.0} (bolt-xs (:b-2 (b2-from-starter nil)) [:end :bolts])) "90 by default")
    (is (= #{5950.0 6050.0} (bolt-xs (:b-2 (b2-from-starter {:gauge 100})) [:end :bolts])) "100 from settings"))
  (testing "and the holes in the column it bolts into move with them"
    (let [holes #(->> (get (sig/signature (:c-b2 %)) [:main]) (filter (comp #{:tool} first))
                      (mapcat (fn [[_ _ _ ends]] (map first ends))) set)]
      (is (= #{5950.0 6050.0} (holes (b2-from-starter {:gauge 100}))))))
  (testing "a bigger bolt: wider holes, by the clearance, and longer bolts"
    (let [radius #(->> (get (sig/signature (:b-2 %)) [:end :plate]) (keep (fn [[k _ r]] (when (= :tool k) r))) set)]
      (is (= #{11.0} (radius (b2-from-starter nil))))
      (is (= #{13.0} (radius (b2-from-starter {:bolt-dia 24}))))
      (is (= #{14.0} (radius (b2-from-starter {:bolt-dia 24 :hole-clearance 4}))))))
  (testing "a hole group with its own gauge keeps it"
    (let [own (fn [m] (update-in m [:connections :conn-2 :connection/parts] prim/update-prim :bolts
                                 assoc-in [:prim/params :gauge] 140))
          i (:b-2 (b2-from-starter {:gauge 100} own))]
      (is (= #{5930.0 6070.0} (bolt-xs i [:end :bolts])) "the end's own 140")
      (is (= #{5950.0 6050.0} (bolt-xs i [:start :bolts])) "the start still inherits 100"))))
