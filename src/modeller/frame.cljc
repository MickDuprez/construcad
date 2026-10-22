(ns modeller.frame
  "Rigid frames (ECS). A frame is {:o origin :x :y :z}, with orthonormal,
  right-handed axes expressed in its parent space. No scale: sizes belong to
  geometry parameters, never to a frame."
  (:require [modeller.vec :as v]))

(def world {:o [0 0 0] :x [1 0 0] :y [0 1 0] :z [0 0 1]})

(defn point->parent
  "A point given in frame f, expressed in f's parent space."
  [{:keys [o x y z]} [px py pz]]
  (v/v+ o (v/v+ (v/scale x px) (v/v+ (v/scale y py) (v/scale z pz)))))

(defn dir->parent
  "A direction given in frame f, expressed in f's parent space (no translation)."
  [{:keys [x y z]} [dx dy dz]]
  (v/v+ (v/scale x dx) (v/v+ (v/scale y dy) (v/scale z dz))))

(defn point->local [{:keys [o x y z]} p]
  (let [d (v/v- p o)] [(v/dot d x) (v/dot d y) (v/dot d z)]))

(defn dir->local [{:keys [x y z]} d]
  [(v/dot d x) (v/dot d y) (v/dot d z)])

(defn compose
  "Child frame (given in parent) expressed in parent's own parent space.
  (compose a b) is 'b inside a'."
  [parent child]
  {:o (point->parent parent (:o child))
   :x (dir->parent parent (:x child))
   :y (dir->parent parent (:y child))
   :z (dir->parent parent (:z child))})

(defn invert
  "The parent frame expressed in f, so (compose f (invert f)) is identity."
  [f]
  {:o (point->local f [0 0 0])
   :x (dir->local f [1 0 0])
   :y (dir->local f [0 1 0])
   :z (dir->local f [0 0 1])})

(defn relative
  "Frame b (in world) expressed in frame a's space: inverse(a) * b."
  [a b]
  (compose (invert a) b))

(defn translate [f offset] (update f :o v/v+ (dir->parent f offset)))

(defn rotate-z
  "Rotate f about its own Z axis by deg (counter-clockwise looking down -Z)."
  [f deg]
  (if (zero? deg)
    f
    (let [r (v/deg->rad deg) c (Math/cos r) s (Math/sin r)
          {:keys [x y]} f]
      (assoc f
             :x (v/v+ (v/scale x c) (v/scale y s))
             :y (v/v+ (v/scale x (- s)) (v/scale y c))))))

(defn plane->parent [f {:keys [origin normal]}]
  {:origin (point->parent f origin) :normal (dir->parent f normal)})

(defn plane->local [f {:keys [origin normal]}]
  {:origin (point->local f origin) :normal (dir->local f normal)})

(defn ->matrix
  "Column-major 4x4, the order THREE.Matrix4.fromArray expects."
  [{:keys [o x y z]}]
  (vec (concat x [0] y [0] z [0] o [1])))

(defn valid? [{:keys [x y z]}]
  (and (v/nearly= (v/magnitude x) 1.0 1e-9)
       (v/nearly= (v/magnitude y) 1.0 1e-9)
       (v/nearly= (v/dot x y) 0.0 1e-9)
       (v/nearly= (v/cross x y) z 1e-9)))

;; --- XLine frames -----------------------------------------------------------

(def ^:private vertical-ref
  "Reference 'up' used when the line itself is vertical."
  [0 1 0])

(defn xline-frame
  "ECS of a construction line: origin at start, +Z from start to end, +Y the
  reference up (world Z, or `ref-up`) made perpendicular to the line, +X
  completing a right-handed frame, then rolled about +Z by `rotation` degrees.
  Returns {:frame f :length l} or {:error ...}."
  ([start end rotation] (xline-frame start end rotation nil))
  ([start end rotation ref-up]
   (let [axis (v/v- end start)
         len (v/magnitude axis)
         z (v/normalize axis)]
     (if-not z
       {:error {:error/type :geometry/invalid-frame :reason :zero-length}}
       (let [ref (or (some-> ref-up v/normalize)
                     (if (v/parallel? z [0 0 1]) vertical-ref [0 0 1]))]
         (if (v/parallel? z ref)
           {:error {:error/type :geometry/invalid-frame :reason :parallel-ref-up}}
           (let [y (v/normalize (v/v- ref (v/scale z (v/dot ref z))))
                 x (v/cross y z)]
             {:length len
              :frame (rotate-z {:o start :x x :y y :z z} (or rotation 0))})))))))
