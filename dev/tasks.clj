(ns tasks
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [com.biffweb.task-runner :refer [run-task]]
            [com.biffweb.tasks :as tasks]))

(defn dev
  "Starts the app locally, with shadow-cljs watching the browser build.

   Like Biff's own dev task, minus Tailwind: styles are plain CSS in
   resources/public/css. Wait for `System started`, then open
   http://localhost:8080. Connect your editor to nREPL on port 7888.

   Pass --no-cljs to skip starting shadow-cljs (if you run it yourself)."
  [& args]
  (if-not (fs/exists? "target/resources")
    ;; target/resources must exist when the JVM starts to be on the classpath.
    (do (io/make-parents "target/resources/_")
        (process/shell "clojure" "-M:dev" "dev"))
    (let [{:keys [biff.tasks/main-ns biff.nrepl/port]} (force @(requiring-resolve 'com.biffweb.tasks/config))]
      (when-not (fs/exists? "config.env")
        (run-task "generate-config"))
      (when-not (fs/exists? "node_modules")
        (process/shell "npm" "install"))
      (when-not (some #{"--no-cljs"} args)
        (process/process {:inherit true} "npx" "shadow-cljs" "watch" "app"))
      (spit ".nrepl-port" port)
      ((requiring-resolve (symbol (str main-ns) "-main"))))))

(def tasks (assoc tasks/tasks "dev" #'dev))
