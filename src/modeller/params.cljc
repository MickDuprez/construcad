(ns modeller.params
  "Template parameters. A value is a number, or a formula string evaluated in
  SCI with the owner's section data bound by name (d bf tf tw r1 ...), the
  supporting member's section data as `support`, and every numeric parameter
  of the same template, e.g. {:plate-h \"(+ d 20)\" :pitch \"(- d (* 2 tf) 70)\"}.
  Formulas may use numeric parameters, not other formulas."
  (:require [sci.core :as sci]))

(defn bindings
  "What a formula sees: the owner's section data by name, the numeric
  parameters by name, and the support's data as `support`."
  [owner-data support-data numeric]
  (merge (into {} (for [[k v] owner-data :when (number? v)] [(symbol (name k)) v]))
         (into {} (for [[k v] numeric] [(symbol (name k)) v]))
         {'support (or support-data {})}))

(defn eval-formula
  "A formula's value with `binds` (see bindings): {:value n} or {:error e}."
  [src binds]
  (try
    (let [v (sci/eval-string src {:bindings binds :deny '[eval load-string slurp spit]})]
      (if (number? v) {:value v} {:error {:error/type :param/not-a-number :formula src}}))
    (catch #?(:clj Exception :cljs :default) e
      {:error {:error/type :param/formula-failed :formula src :message (ex-message e)}})))

(defn resolve-params
  "Numbers for every parameter. Returns {:params {k number} :errors [...]}."
  [params owner-data support-data]
  (let [numeric (into {} (filter (comp number? val)) params)
        binds (bindings owner-data support-data numeric)]
    (reduce (fn [acc [k v]]
              (cond
                (number? v) (assoc-in acc [:params k] v)
                (string? v) (let [{:keys [value error]} (eval-formula v binds)]
                              (if error
                                (update acc :errors conj (assoc error :param k))
                                (assoc-in acc [:params k] value)))
                :else (assoc-in acc [:params k] v)))
            {:params {} :errors []}
            params)))
