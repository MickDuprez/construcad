(ns construcad.app.slice
  "Planar slicing of closed triangle meshes, for the :slice instruction.
  Triangles are clipped against the plane, the cut edges are chained into
  loops, and the section is capped, so the result is closed again and its
  edges read correctly in hidden-line display.

  Geometry here is non-indexed, position only (see construcad.app.reader)."
  (:require ["three" :as THREE]))

(def ^:private key-precision 100.0) ; cut points within 0.01 mm are one point

(defn- point-key [x y z]
  (str (js/Math.round (* x key-precision)) ","
       (js/Math.round (* y key-precision)) ","
       (js/Math.round (* z key-precision))))

(defn- unit [[x y z]]
  (let [m (js/Math.sqrt (+ (* x x) (* y y) (* z z)))]
    [(/ x m) (/ y m) (/ z m)]))

(defn- chain-loops
  "Closed loops of points from undirected cut segments [[p q] ...]."
  [segments]
  (let [points (js/Map.)
        adjacent (js/Map.)
        link! (fn [a b]
                (when-not (.has adjacent a) (.set adjacent a #js []))
                (.push (.get adjacent a) b))]
    (doseq [[[ax ay az :as p] [bx by bz :as q]] segments]
      (let [a (point-key ax ay az) b (point-key bx by bz)]
        (when (not= a b)
          (.set points a p) (.set points b q)
          (link! a b) (link! b a))))
    (let [visited (js/Set.)]
      (vec
       (keep (fn [start]
               (when-not (.has visited start)
                 (loop [loop-keys [start] prev nil current start]
                   (.add visited current)
                   (let [next (some #(when (and (not= % prev) (not (.has visited %))) %)
                                    (array-seq (.get adjacent current)))]
                     (cond
                       next (recur (conj loop-keys next) current next)
                       ;; closed when we are back next to the start
                       (and (> (count loop-keys) 2)
                            (some #(= % start) (array-seq (.get adjacent current))))
                       (mapv #(.get points %) loop-keys)
                       :else nil)))))
             (array-seq (js/Array.from (.keys adjacent))))))))

(defn- signed-area [pts]
  (/ (reduce + (map (fn [[ax ay] [bx by]] (- (* ax by) (* bx ay)))
                    pts (concat (rest pts) [(first pts)])))
     2))

(defn- inside? [[px py] pts]
  (let [n (count pts)]
    (loop [i 0 j (dec n) in false]
      (if (= i n)
        in
        (let [[xi yi] (nth pts i) [xj yj] (nth pts j)
              crosses (and (not= (> yi py) (> yj py))
                           (< px (+ xi (/ (* (- xj xi) (- py yi)) (- yj yi)))))]
          (recur (inc i) i (if crosses (not in) in)))))))

(defn- cap-triangles
  "Triangles [[a b c] ...] covering the section loops, wound to face `outward`."
  [loops [nx ny nz :as n] outward]
  (let [[ux uy uz] (unit (if (> (js/Math.abs nx) 0.9) [0 1 0] [1 0 0]))
        ;; u perpendicular to n, v = n x u
        d (+ (* ux nx) (* uy ny) (* uz nz))
        [ux uy uz] (unit [(- ux (* d nx)) (- uy (* d ny)) (- uz (* d nz))])
        [vx vy vz] [(- (* ny uz) (* nz uy)) (- (* nz ux) (* nx uz)) (- (* nx uy) (* ny ux))]
        flat (fn [[x y z]] [(+ (* x ux) (* y uy) (* z uz)) (+ (* x vx) (* y vy) (* z vz))])
        loops2 (mapv #(mapv flat %) loops)
        depth (fn [i] (count (filter #(and (not= % i) (inside? (first (loops2 i)) (loops2 %)))
                                     (range (count loops2)))))
        depths (mapv depth (range (count loops2)))
        [ox oy oz] outward]
    (vec
     (for [o (range (count loops2))
           :when (even? (depths o))
           :let [holes (filter #(and (= (depths %) (inc (depths o)))
                                     (inside? (first (loops2 %)) (loops2 o)))
                               (range (count loops2)))
                 contour (loops2 o)
                 v2 (fn [pts] (into-array (map (fn [[x y]] (THREE/Vector2. x y)) pts)))
                 all3 (vec (apply concat (loops o) (map loops holes)))
                 faces (.triangulateShape THREE/ShapeUtils
                                          (v2 (cond-> contour (neg? (signed-area contour)) rseq))
                                          (into-array (map #(v2 (loops2 %)) holes)))
                 ;; triangulateShape indexes the contour as passed; keep 3D order in step
                 all3 (if (neg? (signed-area contour))
                        (vec (concat (rseq (loops o)) (mapcat loops holes)))
                        all3)]
           face (array-seq faces)]
       (let [[a b c] (map #(nth all3 %) (array-seq face))
             e1 (mapv - b a) e2 (mapv - c a)
             cx (- (* (e1 1) (e2 2)) (* (e1 2) (e2 1)))
             cy (- (* (e1 2) (e2 0)) (* (e1 0) (e2 2)))
             cz (- (* (e1 0) (e2 1)) (* (e1 1) (e2 0)))]
         (if (neg? (+ (* cx ox) (* cy oy) (* cz oz))) [a c b] [a b c]))))))

(defn slice
  "Keep the part of `geometry` on the :positive or :negative side of the
  plane {:origin :normal}. Returns a new non-indexed geometry."
  [^js geometry {:keys [origin normal]} keep]
  (let [[ox oy oz] origin
        [nx ny nz :as n] (unit normal)
        s (if (= :negative keep) -1 1)
        pos (.. geometry -attributes -position -array)
        out #js []
        segments (transient [])
        dist (fn [x y z] (* s (+ (* nx (- x ox)) (* ny (- y oy)) (* nz (- z oz)))))]
    (loop [i 0]
      (when (< i (.-length pos))
        (let [pts (vec (for [k (range 3)]
                         (let [x (aget pos (+ i (* 3 k))) y (aget pos (+ i (* 3 k) 1)) z (aget pos (+ i (* 3 k) 2))]
                           [x y z (dist x y z)])))
              poly (transient [])
              cut (transient [])]
          (doseq [k (range 3)]
            (let [[px py pz pd :as p] (pts k)
                  [qx qy qz qd] (pts (mod (inc k) 3))]
              (when (>= pd 0) (conj! poly p))
              (when (not= (>= pd 0) (>= qd 0))
                (let [t (/ pd (- pd qd))
                      r [(+ px (* t (- qx px))) (+ py (* t (- qy py))) (+ pz (* t (- qz pz))) 0]]
                  (conj! poly r)
                  (conj! cut r)))))
          (let [poly (persistent! poly) cut (persistent! cut)]
            (when (>= (count poly) 3)
              (doseq [k (range 1 (dec (count poly)))]
                (doseq [[x y z] [(poly 0) (poly k) (poly (inc k))]]
                  (.push out x y z))))
            (when (= 2 (count cut))
              (conj! segments [(subvec (cut 0) 0 3) (subvec (cut 1) 0 3)]))))
        (recur (+ i 9))))
    (let [loops (chain-loops (persistent! segments))
          outward (mapv #(* (- s) %) n)]
      (doseq [tri (cap-triangles loops n outward)
              [x y z] tri]
        (.push out x y z)))
    (doto (THREE/BufferGeometry.)
      (.setAttribute "position" (THREE/Float32BufferAttribute. out 3)))))
