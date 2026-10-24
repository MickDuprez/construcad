(ns modeller.settings
  "Project detailing settings: the values a connection's parts take when
  their own fields are blank (bolt size, gauge, pitch, plate thickness...),
  so a project's standards live in one place and a template only writes
  what is particular to it.

  A prim field's value is resolved in this order (field-value):

    1. the prim's own value: a number or a formula;
    2. a name it references (:gauge, \"pitch\"): the connection's shared
       parameters (:connection/params) by that name, else the setting of
       that name;
    3. a blank field: the setting it inherits (prim-fields), worked out for
       the prim (inherited);
    4. the prim type's built-in default.

  Formulas see the settings as `settings`, e.g. \"(+ (:gauge settings) 10)\",
  alongside the section data and the shared parameters (modeller.params).
  The stored record holds only what the project changed; `effective` puts
  the defaults under it."
  (:require [clojure.string :as str]
            [modeller.params :as params]
            [modeller.prim :as prim]))

(def fields
  "Every setting, in the order the settings panel shows them:
  [key default label help]. Lengths in mm."
  [[:bolt-dia 20 "Bolt diameter" "M20 8.8/S"]
   [:hole-clearance 2 "Hole clearance" "hole = bolt + clearance"]
   [:gauge 90 "Gauge" "across a hole group (X)"]
   [:pitch 70 "Pitch" "up a hole group (Y)"]
   [:edge-distance 35 "Edge distance" "for formulas: (:edge-distance settings)"]
   [:plate-t 10 "Plate thickness" "plates"]
   [:gusset-t 10 "Gusset thickness" "gussets"]
   [:hole-overrun 10 "Hole overrun" "how far a hole cutter runs past each face"]
   [:cut-gap 0 "Cut gap" "a cut's gap, pushed out along its normal"]
   [:bolt-projection 30 "Bolt projection" "bolt length beyond what it passes through (nut stack)"]])

(def defaults (into {} (map (juxt first second)) fields))

(defn effective
  "The settings a project works with: its stored values over the defaults.
  A stored nil (or a key no longer known) falls back to the default."
  [stored]
  (merge defaults (into {} (filter (fn [[k v]] (and (contains? defaults k) (number? v)))) stored)))

(defn errors
  "Why a settings change can't be saved, as [{:setting k :reason kw}];
  empty when it can. A value is a number of zero or more, or nil (back to
  the default)."
  [changes]
  (if-not (map? changes)
    [{:reason :not-a-map}]
    (vec (for [[k v] changes
               :let [reason (cond (not (contains? defaults k)) :unknown-setting
                                  (nil? v) nil
                                  (not (number? v)) :not-a-number
                                  (neg? v) :negative)]
               :when reason]
           {:setting k :reason reason}))))

;; --- which field inherits which setting -------------------------------------------------

(def prim-fields
  "Prim type -> field -> the setting a blank field inherits. A hole
  group's :dia and :bolt-length are worked out from settings too (inherited)."
  {:hole-group {:gauge :gauge :pitch :pitch :bolt-dia :bolt-dia :overrun :hole-overrun}
   :plate {:t :plate-t}
   :gusset {:t :gusset-t}
   :cut {:gap :cut-gap}})

(defn inherited
  "What a blank field of a prim of `type` takes from `settings` (effective):
  a number, or nil when the field inherits nothing. `bolt` is the hole
  group's resolved bolt diameter (for :dia) and `stack` the thickness its
  bolts pass through (for :bolt-length)."
  ([settings type field] (inherited settings type field {}))
  ([settings type field {:keys [bolt stack]}]
   (cond
     (and (= :hole-group type) (= :dia field))
     (+ (or bolt (:bolt-dia settings)) (:hole-clearance settings))
     (and (= :hole-group type) (= :bolt-length field))
     (when stack (+ stack (:bolt-projection settings)))
     :else (some->> (get-in prim-fields [type field]) (get settings)))))

(defn- fmt [n] (if (and (number? n) (== n (Math/round (double n)))) (str (long n)) (str n)))

(defn placeholder
  "The hint a blank field shows in an editor, \"90 from settings\", or nil
  when it inherits nothing. `prim` is the prim being edited."
  [settings {t :prim/type :as prim} field]
  (let [own (get-in prim [:prim/params :bolt-dia] (:bolt-dia settings))]
    (cond
      (and (= :hole-group t) (= :dia field))
      (str (if (number? own) (fmt (+ own (:hole-clearance settings))) "bolt")
           " = bolt + " (fmt (:hole-clearance settings)) " from settings")
      (and (= :hole-group t) (= :bolt-length field))
      (str "grip + " (fmt (:bolt-projection settings)) " from settings")
      :else (when-let [v (inherited settings t field)] (str (fmt v) " from settings")))))

;; --- resolving one value ---------------------------------------------------------------

(defn- name-of
  "The parameter or setting name a value references, as a keyword, or nil."
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (str/trim v)] (when (re-matches #"[a-zA-Z][a-zA-Z0-9-]*" s) (keyword s)))))

(defn field-value
  "One field's value and where it came from, in the resolution order (see
  the ns doc): {:value n :from :own|:param|:settings|:default}, or with an
  :error when it can't be read, the value then `default` (or 0).
  `params` are the connection's resolved shared parameters, `settings` the
  effective settings, `binds` what formulas see (modeller.params/bindings),
  `inherited` what a blank field takes from settings (or nil)."
  [v {:keys [params settings binds inherited default]}]
  (let [fail (fn [e] {:value (or default 0) :from :default :error e})
        n (name-of v)]
    (cond
      (nil? v) (if (some? inherited) {:value inherited :from :settings} {:value default :from :default})
      (number? v) {:value v :from :own}
      (and n (number? (get params n))) {:value (get params n) :from :param}
      (and n (number? (get settings n))) {:value (get settings n) :from :settings}
      (keyword? v) (fail {:error/type :prim/unknown-param :param n})
      ;; a formula, or a bare name of the section data (\"bf\")
      (string? v) (let [{r :value e :error} (params/eval-formula (str/trim v) binds)]
                    (if e (fail e) {:value r :from :own}))
      :else (fail {:error/type :prim/bad-value :value v}))))

;; --- which settings a connection reads --------------------------------------------------

(defn- leaves
  "Every value in a prim's fields and placement (numbers, names,
  formulas), shapes and offsets included."
  [p]
  (filter #(or (keyword? %) (string? %))
          (tree-seq coll? seq (concat (vals (:prim/params p)) (vals (:prim/place p))))))

(defn uses
  "The settings a connection's recipe ({:connection/parts :connection/params})
  reads: those its blank fields inherit, names it references that no shared
  parameter has, and every setting (:all) if a formula mentions `settings`."
  [{:connection/keys [parts params]}]
  (let [params (or params {})
        prims (prim/walk parts)
        formula? #(and (string? %) (str/includes? % "settings"))
        named (for [p prims v (leaves p) :let [n (name-of v)]
                    :when (and n (not (contains? params n)) (contains? defaults n))]
                n)
        blank (for [{t :prim/type :as p} prims
                    [field setting] (concat (get prim-fields t)
                                            (when (= :hole-group t)
                                              (cond-> [[:dia :hole-clearance]]
                                                (nil? (get-in p [:prim/params :bolt-dia])) (conj [:dia :bolt-dia])
                                                (get-in p [:prim/params :bolts?]) (conj [:bolt-length :bolt-projection]))))
                    :when (nil? (get-in p [:prim/params field]))]
                setting)]
    (if (or (some formula? (mapcat leaves prims)) (some formula? (vals params)))
      :all
      (set (concat named blank)))))

(defn changed
  "The settings whose effective value differs between two stored records."
  [before after]
  (let [a (effective before) b (effective after)]
    (set (filter #(not= (a %) (b %)) (keys defaults)))))

(defn reads-any?
  "Does a connection that `uses` (a set, or :all) read any of `ks`?"
  [used ks]
  (or (and (= :all used) (seq ks)) (and (set? used) (some used ks))))
