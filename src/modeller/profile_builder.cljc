(ns modeller.profile-builder
  "Building outlines from section data. Standard shapes (I, channel, angle,
  hollow) are plain Clojure in modeller.shapes. Special extrusions, such as
  custom aluminium sections, are described by small user-editable scripts.
  Those scripts are data (source strings), so they can be stored, versioned
  and added without a release. They run in SCI, a sandboxed Clojure interpreter
  that works on the JVM and in the browser, with only the geometry helpers
  below exposed: no I/O, no interop, no eval.

  A built outline is stored on its section record, so regeneration reads a
  finished polyline and never runs a script."
  (:require [modeller.profile :as profile]
            [modeller.shapes :as shapes]
            [sci.core :as sci]))

(def example-builder
  "A custom builder as a user would write one: a rounded rectangle tube, as an
  extruded aluminium section might be described."
  "(fn [{:keys [b d t r]}]
     {:outer (round-corners (rect-loop b d) [r r r r])
      :holes [(reverse-loop (rect-loop (- b t t) (- d t t)))]})")

(def ^:private helpers
  {'rect-loop profile/rect-loop
   'circle-loop profile/circle-loop
   'round-corners profile/round-corners
   'reverse-loop profile/reverse-loop})

(defn- sci-ctx []
  (sci/init {:namespaces {'user helpers}
             :deny '[eval load-string slurp spit]
             :classes {}}))

(defn compile-builder
  "Evaluate builder source to a function. Returns {:fn f} or {:error e}."
  [source]
  (try
    (let [f (sci/eval-string* (sci-ctx) source)]
      (if (fn? f)
        {:fn f}
        {:error {:error/type :builder/not-a-function}}))
    (catch #?(:clj Exception :cljs :default) e
      {:error {:error/type :builder/compile-failed :message (ex-message e)}})))

(defn builders
  "Builder name -> {:fn f}: the standard shapes plus compiled custom scripts
  (name -> source). A custom builder may not replace a standard one."
  [custom-sources]
  (merge (update-vals custom-sources compile-builder)
         (update-vals shapes/standard (fn [f] {:fn f}))))

(defn- valid-loop? [loop]
  (and (vector? loop) (<= 2 (count loop))
       (every? #(and (<= 2 (count %) 3) (every? number? %)) loop)))

(defn build
  "Run a compiled builder over section data and centre the result.
  Returns {:outline o} or {:error e}."
  [compiled data]
  (if-let [f (:fn compiled)]
    (try
      (let [{:keys [outer holes] :as raw} (f data)]
        (if (and (valid-loop? outer) (every? valid-loop? (or holes [])))
          {:outline (profile/centre raw)}
          {:error {:error/type :builder/invalid-outline :data data}}))
      (catch #?(:clj Exception :cljs :default) e
        {:error {:error/type :builder/failed :message (ex-message e) :data data}}))
    {:error (or (:error compiled) {:error/type :builder/unknown})}))

(defn section-record
  "The stored record for one section: its table data, the builder used and the
  built outline. Rebuilt only when the data or the builder changes."
  [compiled-builders {:section/keys [designation builder] :as row}]
  (let [data (dissoc row :section/designation :section/builder :section/source)
        {:keys [outline error]} (build (get compiled-builders builder) data)]
    (cond-> {:section/designation designation
             :section/builder builder
             :section/data data}
      outline (assoc :section/outline outline)
      error (assoc :section/error (assoc error :section/designation designation)))))
