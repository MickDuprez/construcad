(ns construcad.api
  "EDN over HTTP, plus a websocket per open viewer for pushed patches.

    GET  /api/projects                          projects, each with its buildings
    GET  /api/buildings                         building list
    GET  /api/buildings/:id/view?level=<uuid>   grid, levels, instructions
    GET  /api/buildings/:id/templates           template library + section designations
    GET  /api/buildings/:id/assemblies/:xid     one assembly as the assembly page edits it
    GET  /api/buildings/:id/settings            its project's detailing settings
    PUT  /api/buildings/:id/settings            {:settings {k v}}: change them (a
                                                :settings/update edit; nil = default)
    POST /api/buildings/:id/edits               one source edit -> upsert patch
    GET  /api/buildings/:id/ws                  patches from everyone's edits

  The browser only ever sends source edits (construcad.edits); geometry
  only travels from server to browser."
  (:require [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [com.biffweb :as biff]
            [construcad.store :as store]
            [ring.websocket :as ws]))

(defn- edn-response
  ([body] (edn-response 200 body))
  ([status body]
   {:status status
    :headers {"content-type" "application/edn; charset=utf-8"}
    :body (pr-str body)}))

(defn- path-uuid [ctx k]
  (some-> (get-in ctx [:path-params k]) parse-uuid))

(defn- read-body [{:keys [body-params body]}]
  (or body-params (some-> body slurp edn/read-string)))

(defn projects [{:keys [biff/db]}]
  (edn-response (store/projects db)))

(defn buildings [{:keys [biff/db]}]
  (edn-response (vec (store/buildings db))))

(defn view [ctx]
  (let [building-id (path-uuid ctx :id)
        level-id (some-> (get-in ctx [:params "level"]) parse-uuid)]
    (if-let [v (store/level-view ctx building-id level-id)]
      (edn-response v)
      (edn-response 404 {:error {:error/type :edit/unknown-building}}))))

(defn templates [ctx]
  (if-let [t (store/templates ctx (path-uuid ctx :id))]
    (edn-response t)
    (edn-response 404 {:error {:error/type :edit/unknown-building}})))

(defn assembly [ctx]
  (if-let [page (store/assembly-page ctx (path-uuid ctx :id) (get-in ctx [:path-params :xid]))]
    (edn-response page)
    (edn-response 404 {:error {:error/type :edit/unknown-xline}})))

(declare broadcast!)

(defn settings [ctx]
  (if-let [s (store/settings ctx (path-uuid ctx :id))]
    (edn-response s)
    (edn-response 404 {:error {:error/type :edit/unknown-building}})))

(defn put-settings
  "Change the settings of the building's own project, through the same
  edit as the editor's: assemblies that inherit a changed setting
  regenerate, and every open viewer of the building gets the patch."
  [ctx]
  (let [building-id (path-uuid ctx :id)
        body (read-body ctx)
        result (store/apply-edit! (biff/merge-context ctx) building-id
                                  {:edit/type :settings/update :settings (:settings body)})]
    (if (:error result)
      (edn-response (if (= :edit/unknown-building (get-in result [:error :error/type])) 404 422) result)
      (do (broadcast! ctx building-id result nil)
          (edn-response {:patch result})))))

(defn- broadcast! [{:keys [construcad/viewers]} building-id patch except]
  (doseq [socket (get @viewers building-id) :when (not= socket except)]
    (try (ws/send socket (pr-str {:patch patch}))
         (catch Exception e (log/warn e "Dropping viewer socket")))))

(defn edit [ctx]
  (let [building-id (path-uuid ctx :id)
        {:keys [edit]} (read-body ctx)
        result (store/apply-edit! (biff/merge-context ctx) building-id edit)]
    (if (:error result)
      (edn-response 422 result)
      (do (broadcast! ctx building-id result nil)
          (edn-response {:patch result})))))

(defn socket [{:keys [construcad/viewers] :as ctx}]
  (let [building-id (path-uuid ctx :id)]
    {:status 101
     :headers {"upgrade" "websocket" "connection" "upgrade"}
     ::ws/listener {:on-open (fn [s] (swap! viewers update building-id (fnil conj #{}) s))
                    :on-message (fn [_ _])
                    :on-close (fn [s _ _] (swap! viewers update building-id disj s))}}))

(def module
  {:api-routes [["/api"
                 ["/projects" {:get projects}]
                 ["/buildings" {:get buildings}]
                 ["/buildings/:id/view" {:get view}]
                 ["/buildings/:id/templates" {:get templates}]
                 ["/buildings/:id/assemblies/:xid" {:get assembly}]
                 ["/buildings/:id/settings" {:get settings :put put-settings}]
                 ["/buildings/:id/edits" {:post edit}]
                 ["/buildings/:id/ws" {:get socket}]]]})
