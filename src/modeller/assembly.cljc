(ns modeller.assembly
  "Turning an XLine and its parts into reader instructions, and applying the
  connection actions owned by that XLine. Everything is built in the XLine's
  construction space; the XLine frame is applied once, on the outside.

  A part:

    {:part/id        :main
     :part/kind      :extrusion
     :part/profile   \"310UB40.4\"    ; a library designation, or inline data such as
                                      ; {:section/builder :rect :b 200 :d 3000}
     :part/insert    :top-centre      ; which of the nine points sits on the XLine
     :part/offset    [0 0]            ; shift of that point off the XLine, local XY
     :part/rotation  0                ; roll of the section about that point, degrees
     :part/start-ext 0                ; length beyond the start node (negative = set back)
     :part/end-ext   0}

  A connection, owned by the XLine whose end it sits on:

    {:connection/id       :c-b1-end
     :connection/owner    :b1
     :connection/end      :end
     :connection/refs     [:c2]
     :connection/template :fin-cut
     :connection/params   {:gap 10}}

  A template is an ordered list of actions, each a map with :action/type."
  (:require [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.model :as model]
            [modeller.profile :as profile]
            [modeller.profile-builder :as builder]
            [modeller.vec :as v]))

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

;; --- connection actions -----------------------------------------------------

(defmulti apply-action
  "Apply one template action. Receives the connection context and the map of
  part id -> instruction, returns the updated map (or adds :errors)."
  (fn [_ctx action _parts] (:action/type action)))

(defmethod apply-action :default [_ctx action parts]
  (update parts ::errors conj {:error/type :action/unknown :action action}))

(defn- support-faces-in-owner
  "Bounding faces of the supporting member's main part, in the owner's
  construction space."
  [{:keys [model owner-frame ref-xline ref-resolved]}]
  (let [part (main-part ref-xline)
        outline (when part (:outline (part-outline model part)))]
    (when outline
      (let [to-world (frame/compose (:frame ref-resolved) (part-frame part outline))]
        (update-vals (profile/face-planes outline)
                     #(frame/plane->local owner-frame (frame/plane->parent to-world %)))))))

(defmethod apply-action :cut-to-face
  ;; Cut the owner's part square to the face of the supporting member that it
  ;; approaches, leaving a gap. {:action/type :cut-to-face :action/part :main
  ;;  :action/ref 0 :action/face :auto :action/gap 10}
  [{:keys [conn owner-length params] :as ctx} action parts]
  (let [part-id (:action/part action (::main parts))
        faces (support-faces-in-owner ctx)
        approach (if (= :start (:connection/end conn)) [0 0 -1] [0 0 1])
        face (:action/face action :auto)
        gap (get params (:action/gap-param action :gap) (:action/gap action 0))]
    (if-not (and faces (get parts part-id))
      (update parts ::errors conj {:error/type :action/missing-input :action action})
      (let [{:keys [origin normal]} (if (= :auto face)
                                      (apply min-key #(v/dot (:normal %) approach) (vals faces))
                                      (get faces face))
            plane {:origin (v/v+ origin (v/scale normal gap)) :normal normal}
            mid [0 0 (/ owner-length 2.0)]
            keep (if (pos? (v/dot (v/v- mid (:origin plane)) normal)) :positive :negative)]
        (update parts part-id
                #(assoc (instr/slice (dissoc % :id) plane keep) :id part-id))))))

(defn- connection-context [model resolved owner-id owner conn]
  (let [ref-id (first (:connection/refs conn))]
    {:model model
     :conn conn
     :owner owner
     :owner-frame (get-in resolved [owner-id :frame])
     :owner-length (get-in resolved [owner-id :length])
     :ref-xline (get-in model [:xlines ref-id])
     :ref-resolved (get resolved ref-id)
     :params (merge (get-in model [:templates (:connection/template conn) :template/params])
                    (:connection/params conn))}))

(defn assembly-instruction
  "Instructions for one XLine-rooted assembly. Returns {:instr i :errors [...]}."
  [model resolved id]
  (let [xline (get-in model [:xlines id])
        {:keys [frame length]} (get resolved id)
        built (map #(part-instruction model % length) (:assembly/parts xline))
        parts (into {::main (:part/id (main-part xline)) ::errors []}
                    (keep #(when (:id %) [(:id %) %]) built))
        owned (filter #(= id (:connection/owner %)) (vals (:connections model)))
        parts (reduce (fn [parts conn]
                        (if-let [e (model/connection-error model conn)]
                          (update parts ::errors conj e)
                          (let [ctx (connection-context model resolved id xline conn)
                                tpl (get-in model [:templates (:connection/template conn)])]
                            (reduce #(apply-action ctx %2 %1) parts (:template/actions tpl)))))
                      parts owned)
        order (map :part/id (:assembly/parts xline))]
    {:instr (assoc (instr/transform frame
                                    (instr/group (cons {:op :xline :id id :length length}
                                                       (keep parts order))))
                   :id id)
     :errors (concat (keep :error built) (::errors parts))}))
