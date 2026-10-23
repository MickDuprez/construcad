(ns construcad.app.dom
  "A small DOM helper: (h :tag {:class .. :on-click f} children...), and the
  same for SVG elements."
  (:require [clojure.string :as str]))

(defn- build [el attrs children]
  (doseq [[k v] attrs :when (some? v)]
    (let [k (name k)]
      (cond
        (str/starts-with? k "on-") (.addEventListener el (subs k 3) v)
        (and (= k "class") (not (instance? js/SVGElement el))) (set! (.-className el) v)
        (#{"value" "checked" "disabled" "selected"} k) (aset el k v)
        :else (.setAttribute el k (str v)))))
  (doseq [c (flatten children) :when (some? c)]
    (.append el (if (string? c) c (if (number? c) (str c) c))))
  el)

(defn h [tag attrs & children]
  (build (.createElement js/document (name tag)) attrs children))

(defn s
  "An SVG element."
  [tag attrs & children]
  (build (.createElementNS js/document "http://www.w3.org/2000/svg" (name tag)) attrs children))

(defn replace-keeping-focus!
  "Replace el's children, then put focus back on the element carrying the
  same data-k as the one focused before, so re-rendering on change doesn't
  lose the user's place."
  [^js el children]
  (let [focused (some-> js/document .-activeElement (.getAttribute "data-k"))]
    (.apply (.-replaceChildren el) el (into-array (remove nil? children)))
    (when focused
      (some-> (.querySelector el (str "[data-k=\"" focused "\"]")) .focus))))
