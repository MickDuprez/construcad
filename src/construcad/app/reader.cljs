(ns construcad.app.reader
  "Realises reader instructions (modeller.instr) as three.js objects.

    :transform :group   -> THREE.Group (frame via modeller.frame/->matrix)
    :xline              -> the construction line, local +Z from 0 to length
    :extrude :box :cylinder :slice :subtract :union :intersect
                        -> a solid: one mesh plus its visible and hidden edges

  Solids are built as plain triangle soup (non-indexed, position only), so
  :slice can cut them (construcad.app.slice); :subtract :union :intersect go
  through three-bvh-csg. Every object carries a userData.renderRole the
  display modes switch on."
  (:require ["three" :as THREE]
            ["three-bvh-csg" :refer [Brush Evaluator SUBTRACTION ADDITION INTERSECTION]]
            [construcad.app.slice :as slice]
            [construcad.edges :as edges]
            [modeller.frame :as frame]
            [modeller.profile :as profile]))

(def colours
  {:face 0xc3ccd3 :edge 0x1f2a33 :hidden 0x5d6f7b :xline 0x0b8a96})

;; --- solids as triangle soup ---------------------------------------------------

(defn- soup
  "Position-only, non-indexed copy of a geometry."
  [^js geometry]
  (let [g (if (.-index geometry) (.toNonIndexed geometry) geometry)]
    (doto (THREE/BufferGeometry.)
      (.setAttribute "position" (.clone (.. g -attributes -position))))))

(defn- merge-soup [geometries]
  (let [arrays (map #(.. ^js % -attributes -position -array) geometries)
        out (js/Float32Array. (reduce + (map #(.-length %) arrays)))]
    (reduce (fn [offset a] (.set out a offset) (+ offset (.-length a))) 0 arrays)
    (doto (THREE/BufferGeometry.)
      (.setAttribute "position" (THREE/BufferAttribute. out 3)))))

(defn- loop-points [loop]
  (map (fn [[x y]] (THREE/Vector2. x y)) (profile/tessellate-loop loop)))

(defn extrude-geometry
  "A profile {:outer :holes} swept along local +Z from 0 to length."
  [{:keys [outer holes]} length]
  (let [shape (THREE/Shape. (into-array (loop-points outer)))]
    (doseq [h holes]
      (.push (.-holes shape) (THREE/Path. (into-array (loop-points h)))))
    (soup (THREE/ExtrudeGeometry. shape #js {:depth length :bevelEnabled false :steps 1}))))

(defn frame-matrix [f]
  (.fromArray (THREE/Matrix4.) (clj->js (frame/->matrix f))))

(declare ^js solid-geometry)

(defonce ^:private evaluator
  (delay (doto (Evaluator.)
           (set! -attributes #js ["position"])
           (set! -useGroups false))))

(defn- brush [^js geometry]
  (doto (Brush. geometry) (.updateMatrixWorld)))

(defn- csg
  "Fold `op` (SUBTRACTION, ADDITION, INTERSECTION) over solids' soups with
  three-bvh-csg. A failed boolean shows the first operand and says so."
  [instr op geometries]
  (try
    (let [^js result (reduce (fn [^js acc ^js g] (.evaluate @evaluator acc (brush g) op))
                             (brush (first geometries))
                             (rest geometries))]
      (soup (.-geometry result)))
    (catch :default e
      (js/console.warn "construcad: boolean failed; showing the first operand uncut"
                       (clj->js (select-keys instr [:id :op])) e)
      (first geometries))))

(defn ^js solid-geometry
  "Triangle soup for a solid instruction, in the instruction's own space."
  [{:keys [op] :as instr}]
  (case op
    :extrude (extrude-geometry (:profile instr) (:length instr))
    ;; Same convention as :extrude: centred on local XY, from 0 along +Z.
    :box (let [[sx sy sz] (:size instr)]
           (soup (.translate (THREE/BoxGeometry. sx sy sz) 0 0 (/ sz 2))))
    :cylinder (soup (-> (THREE/CylinderGeometry. (:radius instr) (:radius instr) (:length instr) 32)
                        (.rotateX (/ js/Math.PI 2))
                        (.translate 0 0 (/ (:length instr) 2))))
    :transform (let [^js g (solid-geometry (:child instr))] (.applyMatrix4 g (frame-matrix (:frame instr))))
    :group (merge-soup (keep #(when (not= :xline (:op %)) (solid-geometry %)) (:children instr)))
    :slice (slice/slice (solid-geometry (:target instr)) (:plane instr) (:keep instr))
    :subtract (csg instr SUBTRACTION (map solid-geometry (cons (:target instr) (:tools instr))))
    :union (csg instr ADDITION (map solid-geometry (:solids instr)))
    :intersect (csg instr INTERSECTION (map solid-geometry (:solids instr)))))

;; --- display objects -------------------------------------------------------------

(defn- edges-geometry
  "Feature edges of a solid's soup. Not THREE.EdgesGeometry: booleans leave
  T-junctions on flat faces that it draws as triangulation (construcad.edges)."
  [^js geometry]
  (let [segs (edges/feature-edges (.. geometry -attributes -position -array) 20)]
    (doto (THREE/BufferGeometry.)
      (.setAttribute "position" (THREE/Float32BufferAttribute. segs 3)))))

(defn- solid-object [instr]
  (let [geometry (doto (solid-geometry instr) (.computeVertexNormals))
        group (THREE/Group.)
        face (THREE/Mesh. geometry
                          (THREE/MeshStandardMaterial.
                           #js {:color (:face colours) :roughness 0.85 :metalness 0.05
                                :side THREE/DoubleSide
                                :polygonOffset true :polygonOffsetFactor 1 :polygonOffsetUnits 1}))
        edges-geometry (edges-geometry geometry)
        edges (THREE/LineSegments. edges-geometry (THREE/LineBasicMaterial. #js {:color (:edge colours)}))
        ;; Mick's hidden-line approach from schemacraft-viewer: the same edges
        ;; again, dashed, drawn only where something is in front of them.
        hidden (THREE/LineSegments. edges-geometry
                                    (THREE/LineDashedMaterial.
                                     #js {:color (:hidden colours) :dashSize 60 :gapSize 45
                                          :depthWrite false :depthFunc THREE/GreaterDepth}))]
    (set! (.-userData face) #js {:renderRole "face"})
    (set! (.-userData edges) #js {:renderRole "visible-edge"})
    (set! (.-userData hidden) #js {:renderRole "hidden-edge"})
    (set! (.-renderOrder edges) 1)
    (set! (.-renderOrder hidden) 2)
    (.computeLineDistances hidden)
    (.add group face edges hidden)
    group))

(defn- xline-object [{:keys [length]}]
  (let [geometry (.setFromPoints (THREE/BufferGeometry.)
                                 #js [(THREE/Vector3. 0 0 0) (THREE/Vector3. 0 0 length)])
        line (THREE/Line. geometry (THREE/LineBasicMaterial.
                                    #js {:color (:xline colours) :depthTest false :transparent true}))]
    (set! (.-userData line) #js {:renderRole "xline"})
    (set! (.-renderOrder line) 10)
    line))

(defn part-name
  "The three.js name of the object for a part id: :end/plate is \"end/plate\"."
  [id]
  (subs (str id) 1))

(defn realise
  "A three.js object for any instruction, named after its :id when it has one."
  [{:keys [op] :as instr}]
  (let [^js o (case op
                :transform (doto (THREE/Group.)
                             (.applyMatrix4 (frame-matrix (:frame instr)))
                             (.add (realise (:child instr))))
                :group (let [g (THREE/Group.)]
                         (doseq [c (:children instr)] (.add g (realise c)))
                         g)
                :xline (xline-object instr)
                (solid-object instr))]
    (when (keyword? (:id instr)) (set! (.-name o) (part-name (:id instr))))
    o))

(defn assembly-object
  "The root object for one assembly's instruction, tagged with its id."
  [id instr]
  (let [^js o (realise instr)]
    (set! (.-userData o) #js {:assemblyId (str id)})
    o))

(defn dispose! [^js object]
  (.traverse object (fn [^js o]
                      (some-> (.-geometry o) .dispose)
                      (some-> (.-material o) .dispose)))
  (some-> (.-parent object) (.remove object)))
