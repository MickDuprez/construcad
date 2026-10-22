(ns construcad.ui
  (:require [com.biffweb :as biff]))

(defn editor-page [_ctx]
  (biff/render
   [:html {:lang "en"}
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:title "ConstruCAD"]
     [:link {:rel "stylesheet" :href "/css/construcad.css"}]]
    [:body
     [:div#app]
     [:script {:src "/cljs/main.js"}]]]))

(def module
  {:routes [["/" {:get editor-page}]]})
