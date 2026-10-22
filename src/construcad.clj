(ns construcad
  (:require [com.biffweb :as biff]
            [construcad.api :as api]
            [construcad.middleware :as mid]
            [construcad.schema :as schema]
            [construcad.store :as store]
            [construcad.ui :as ui]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :as tn-repl]
            [malli.core :as malc]
            [malli.registry :as malr]
            [nrepl.cmdline :as nrepl-cmd])
  (:gen-class))

(def modules
  [ui/module
   api/module
   schema/module])

(def routes [["" {:middleware [mid/wrap-site-defaults]}
              (keep :routes modules)]
             ["" {:middleware [mid/wrap-api-defaults]}
              (keep :api-routes modules)]])

(def handler (-> (biff/reitit-handler {:routes routes})
                 mid/wrap-base-defaults))

(defn generate-assets! [_ctx])

(defn on-save [ctx]
  (biff/add-libs ctx)
  (biff/eval-files! ctx))

(def malli-opts
  {:registry (malr/composite-registry
              malc/default-registry
              (apply biff/safe-merge (keep :schema modules)))})

(def initial-system
  {:biff/modules #'modules
   :biff/handler #'handler
   :biff/malli-opts #'malli-opts
   :biff.beholder/on-save #'on-save
   :biff.xtdb/tx-fns biff/tx-fns
   :construcad/viewers (atom {})})

(defonce system (atom {}))

(defn use-library
  "The section library: compiled builders and the AU sections, built once at
  startup (not stored in XTDB yet)."
  [ctx]
  (assoc ctx :construcad/library (store/load-library)))

(def components
  [biff/use-aero-config
   biff/use-xtdb
   biff/use-queues
   biff/use-xtdb-tx-listener
   use-library
   biff/use-jetty
   biff/use-beholder])

(defn start []
  (let [new-system (reduce (fn [system component]
                             (log/info "starting:" (str component))
                             (component system))
                           initial-system
                           components)]
    (reset! system new-system)
    (store/seed-if-empty! new-system)
    (log/info "System started.")
    (log/info "Go to" (:biff/base-url new-system))
    new-system))

(defn -main []
  (let [{:keys [biff.nrepl/args]} (start)]
    (apply nrepl-cmd/-main args)))

(defn refresh []
  (doseq [f (:biff/stop @system)]
    (log/info "stopping:" (str f))
    (f))
  (tn-repl/refresh :after `start)
  :done)
