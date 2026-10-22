(ns modeller.instr
  "Reader instructions: the small, generic EDN solid algebra a viewer or CAD
  host realises. No domain words here; a beam is just an extrude.")

(defn extrude
  "Profile (local XY) swept along local +Z from 0 to length. Loops keep their
  bulges; the reader tessellates arcs (modeller.profile/tessellate-loop)."
  [profile length]
  {:op :extrude :profile (select-keys profile [:outer :holes]) :length length})

(defn transform [frame child] {:op :transform :frame frame :child child})
(defn group [children] {:op :group :children (vec children)})
(defn box [[sx sy sz]] {:op :box :size [sx sy sz]})
(defn cylinder [radius length] {:op :cylinder :radius radius :length length})

(defn slice
  "Keep the part of target on the :positive or :negative side of plane."
  [target plane keep]
  {:op :slice :target target :plane plane :keep keep})

(defn subtract [target tools] {:op :subtract :target target :tools (vec tools)})
(defn union [solids] {:op :union :solids (vec solids)})
(defn intersect [solids] {:op :intersect :solids (vec solids)})

(def ops #{:extrude :transform :group :box :cylinder :slice :subtract :union :intersect :xline})

(defn validate
  "Errors for unknown ops anywhere in the tree."
  [instr]
  (let [children (case (:op instr)
                   :transform [(:child instr)]
                   :group (:children instr)
                   :slice [(:target instr)]
                   :subtract (cons (:target instr) (:tools instr))
                   (:union :intersect) (:solids instr)
                   [])]
    (concat (when-not (ops (:op instr))
              [{:error/type :instr/unknown-op :op (:op instr)}])
            (mapcat validate children))))
