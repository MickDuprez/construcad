(ns modeller.part
  "One part of an assembly in its XLine's construction space: where its
  profile sits, its outline, its instruction.

    {:part/id        :main
     :part/kind      :extrusion
     :part/profile   \"310UB40.4\"    ; a library designation, or inline data such as
                                      ; {:section/builder :rect :b 200 :d 3000}
     :part/insert    :top-centre      ; which of the nine points sits on the XLine
     :part/offset    [0 0]            ; shift of that point off the XLine, local XY
     :part/rotation  0                ; roll of the section about that point, degrees
     :part/start-ext 0                ; length beyond the start node (negative = set back)
     :part/end-ext   0}"
  (:require [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.profile :as profile]
            [modeller.profile-builder :as builder]))

(defn part-frame
  "Frame of the part's profile origin in XLine construction space. The
  insertion point sits at the offset, and the section rolls about it."
  [part outline]
  (let [[ox oy] (or (:part/offset part) [0 0])
        rotated (frame/rotate-z (assoc frame/world :o [ox oy (- (:part/start-ext part 0))])
                                (:part/rotation part 0))
        [sx sy] (profile/insertion-shift outline (:part/insert part))]
    (frame/translate rotated [sx sy 0])))

(defn part-outline
  "Outline for a part's profile: the stored outline of a library section, or
  inline data built with the model's compiled builders. {:outline o} or {:error e}."
  [model {p :part/profile id :part/id}]
  (let [result (if (map? p)
                 (builder/build (get-in model [:builders (:section/builder p)])
                                (dissoc p :section/builder))
                 (if-let [o (get-in model [:sections p :section/outline])]
                   {:outline o}
                   {:error {:error/type :part/unknown-profile :profile p}}))]
    (cond-> result (:error result) (update :error assoc :part/id id))))

(defn part-instruction
  "Instruction for one extrusion part, or {:error ...}."
  [model part length]
  (let [{:keys [outline error]} (part-outline model part)]
    (if error
      {:error error}
      (let [l (+ length (:part/start-ext part 0) (:part/end-ext part 0))]
        (assoc (instr/transform (part-frame part outline) (instr/extrude outline l))
               :id (:part/id part))))))

(defn main-part [xline] (first (:assembly/parts xline)))

(defn section-data
  "Table data for a part's profile: the library row, or the inline map."
  [model part]
  (let [p (:part/profile part)]
    (if (map? p) p (get-in model [:sections p :section/data]))))

(defn section-builder
  "The builder a part's profile is made with (:i-section, :rect ...)."
  [model part]
  (let [p (:part/profile part)]
    (if (map? p) (:section/builder p) (get-in model [:sections p :section/builder]))))
