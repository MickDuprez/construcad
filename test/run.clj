(require '[clojure.test :as t])
(def nss (quote [modeller.frame-test modeller.model-test modeller.assembly-test modeller.template-test
                 construcad.edits-test construcad.snap-test construcad.template-edit-test construcad.edges-test]))
(apply require nss)
(let [{:keys [fail error]} (apply t/run-tests nss)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
