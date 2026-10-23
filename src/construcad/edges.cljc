(ns construcad.edges
  "Feature edges of a triangle soup, robust to T-junctions.

  THREE.EdgesGeometry pairs triangle edges by identical endpoints. Boolean
  output (three-bvh-csg) leaves T-junctions on flat faces: a vertex of one
  triangle sits part way along an edge of its neighbour. Those edges never
  pair, count as open boundaries and draw the triangulation, most visibly
  around bolt holes.

  Here the triangles are grouped by plane. Within a plane every edge is split
  at the plane's vertices that lie on it, and the pieces used by two triangles
  cancel, leaving the outline of each flat region. Outline pieces are then
  split at each other's ends and matched across planes: a piece is drawn when
  only one plane has it, or when its planes meet at more than the threshold
  angle (so tessellated holes and root radii stay smooth).

  Input is a flat seq or array of coordinates, nine per triangle (a position
  attribute's array); output is a flat array, six per segment.

  This runs for every solid each time the model is drawn, so it works on
  welded vertex ids, flat arrays and host maps rather than vectors and seqs.")

(def ^:private tol 1e-3)

;; --- host collections --------------------------------------------------------------

(defn- hmap [] #?(:clj (java.util.HashMap.) :cljs (js/Map.)))
(defn- hget [m k] #?(:clj (.get ^java.util.HashMap m k) :cljs (.get m k)))
(defn- hput! [m k v] #?(:clj (.put ^java.util.HashMap m k v) :cljs (.set m k v)) v)
(defn- heach [m f]
  #?(:clj (doseq [^java.util.Map$Entry e (.entrySet ^java.util.HashMap m)] (f (.getKey e) (.getValue e)))
     :cljs (.forEach m (fn [v k] (f k v)))))

(defn- alist [] #?(:clj (java.util.ArrayList.) :cljs #js []))
(defn- push! [l x] #?(:clj (.add ^java.util.ArrayList l x) :cljs (.push l x)) l)
(defn- size [l] #?(:clj (.size ^java.util.ArrayList l) :cljs (alength l)))
(defn- at [l i] #?(:clj (.get ^java.util.ArrayList l (int i)) :cljs (aget l i)))
(defn- ati [l i] #?(:clj (long (.get ^java.util.ArrayList l (int i))) :cljs (aget l i)))

;; --- splitting ---------------------------------------------------------------------

(defn- stops
  "Vertex ids along a-b: a, each of cands[from, to) lying strictly inside the
  segment in order, then b. `P` holds vertex positions, three per id."
  [^doubles P a b cands from to]
  (let [ax (aget P (* 3 a)) ay (aget P (+ 1 (* 3 a))) az (aget P (+ 2 (* 3 a)))
        abx (- (aget P (* 3 b)) ax) aby (- (aget P (+ 1 (* 3 b))) ay) abz (- (aget P (+ 2 (* 3 b))) az)
        l2 (+ (* abx abx) (* aby aby) (* abz abz))]
    (loop [i from hits nil]
      (if (< i to)
        (let [p #?(:clj (long (.get ^java.util.ArrayList cands (int i))) :cljs (aget cands i))]
          (if (or (== p a) (== p b))
            (recur (inc i) hits)
            (let [px (- (aget P (* 3 p)) ax) py (- (aget P (+ 1 (* 3 p))) ay) pz (- (aget P (+ 2 (* 3 p))) az)
                  t (/ (+ (* px abx) (* py aby) (* pz abz)) l2)]
              (if (and (< 1e-9 t) (< t (- 1 1e-9)))
                (let [dx (- px (* t abx)) dy (- py (* t aby)) dz (- pz (* t abz))]
                  (recur (inc i) (if (< (+ (* dx dx) (* dy dy) (* dz dz)) (* tol tol))
                                   (conj (or hits []) [t p])
                                   hits)))
                (recur (inc i) hits)))))
        (if hits
          (-> [a] (into (map second) (sort-by first hits)) (conj b))
          [a b])))))

(defn- lower-bound
  "First index in sorted `vs` whose value is not below x."
  [^doubles vs x]
  (loop [lo 0 hi (alength vs)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (< (aget vs mid) x) (recur (inc mid) hi) (recur lo mid)))
      lo)))

(defn- each-piece!
  "Calls (f x y) for each non-degenerate piece between consecutive stops."
  [stops f]
  (loop [i 1]
    (when (< i (count stops))
      (let [x (nth stops (dec i)) y (nth stops i)]
        (when (not= x y) (f x y)))
      (recur (inc i)))))

;; --- feature edges -----------------------------------------------------------------

(defn feature-edges
  "Flat [x y z x y z ...] segments for the feature edges of a triangle soup.
  `threshold-deg` is the crease angle below which neighbouring planes count
  as one smooth surface."
  [coords threshold-deg]
  (let [^doubles c #?(:clj (double-array coords) :cljs (if (sequential? coords) (into-array coords) coords))
        cos-t (Math/cos (* threshold-deg (/ Math/PI 180)))
        q (fn [x] (Math/round (/ (double x) tol)))
        ;; Weld vertices within tol to integer ids.
        ids (hmap)
        pos (alist)
        vid (fn [o]
              (let [x (aget c o) y (aget c (+ o 1)) z (aget c (+ o 2))
                    k (str (q x) "," (q y) "," (q z))]
                (or (hget ids k)
                    (let [id (quot (size pos) 3)]
                      (push! pos x) (push! pos y) (push! pos z)
                      (hput! ids k id)))))
        ;; Triangles grouped by plane: per plane its unit normal and its
        ;; triangles' vertex ids, three per triangle.
        planes (hmap)
        plane-n (alist)
        plane-tris (alist)
        _ (dotimes [t (quot (alength c) 9)]
            (let [o (* 9 t)
                  ax (aget c o) ay (aget c (+ o 1)) az (aget c (+ o 2))
                  ux (- (aget c (+ o 3)) ax) uy (- (aget c (+ o 4)) ay) uz (- (aget c (+ o 5)) az)
                  vx (- (aget c (+ o 6)) ax) vy (- (aget c (+ o 7)) ay) vz (- (aget c (+ o 8)) az)
                  nx (- (* uy vz) (* uz vy)) ny (- (* uz vx) (* ux vz)) nz (- (* ux vy) (* uy vx))
                  l (Math/sqrt (+ (* nx nx) (* ny ny) (* nz nz)))]
              (when (> l (* tol tol))
                (let [nx (/ nx l) ny (/ ny l) nz (/ nz l)
                      d (+ (* nx ax) (* ny ay) (* nz az))
                      k (str (Math/round (* 1e4 nx)) "," (Math/round (* 1e4 ny)) ","
                             (Math/round (* 1e4 nz)) "," (Math/round (* 1e2 d)))
                      p (or (hget planes k)
                            (let [p (size plane-n)]
                              (push! plane-n [nx ny nz])
                              (push! plane-tris (alist))
                              (hput! planes k p)))
                      tv (at plane-tris p)]
                  (push! tv (vid o)) (push! tv (vid (+ o 3))) (push! tv (vid (+ o 6)))))))
        ^doubles P #?(:clj (double-array pos) :cljs pos)
        nv (quot (alength P) 3)
        pair-key (fn [a b] (if (< a b) (+ (* a nv) b) (+ (* b nv) a)))
        ;; Per plane, split each triangle edge at the plane's vertices on it;
        ;; pieces used an odd number of times are the plane's outline.
        mark (long-array nv -1)
        outline (alist) ; lo, hi, plane: three per piece
        _ (dotimes [p (size plane-tris)]
            (let [tv (at plane-tris p)
                  verts (alist)
                  counts (hmap)]
              (dotimes [i (size tv)]
                (let [v (ati tv i)]
                  (when-not (== p (aget ^longs mark v))
                    (aset ^longs mark v (long p))
                    (push! verts v))))
              (let [n-verts (size verts)
                    count! (fn [x y]
                             (let [k (pair-key x y)]
                               (hput! counts k (inc (or (hget counts k) 0)))))
                    edge! (fn [u w] (when (not= u w) (each-piece! (stops P u w verts 0 n-verts) count!)))]
                (dotimes [t (quot (size tv) 3)]
                  (let [a (ati tv (* 3 t)) b (ati tv (+ 1 (* 3 t))) d (ati tv (+ 2 (* 3 t)))]
                    (edge! a b) (edge! b d) (edge! d a))))
              (heach counts (fn [k n]
                              (when (odd? n)
                                (push! outline (quot k nv)) (push! outline (mod k nv)) (push! outline p))))))
        ;; Outline ends, sorted along each axis, to find those on a piece.
        ends (let [seen (hmap)]
               (dotimes [i (quot (size outline) 3)]
                 (hput! seen (at outline (* 3 i)) true)
                 (hput! seen (at outline (+ 1 (* 3 i))) true))
               (let [l (alist)] (heach seen (fn [k _] (push! l k))) l))
        by-axis (vec (for [axis (range 3)]
                       (let [sorted (sort-by #(aget P (+ axis (* 3 %))) (seq ends))]
                         {:ids #?(:clj (java.util.ArrayList. ^java.util.Collection sorted) :cljs (to-array sorted))
                          :vals (double-array (map #(aget P (+ axis (* 3 %))) sorted))})))
        ;; Split outline pieces at the ends on them, then list each piece's planes.
        piece-planes (hmap)
        _ (dotimes [i (quot (size outline) 3)]
            (let [a (ati outline (* 3 i)) b (ati outline (+ 1 (* 3 i))) p (at outline (+ 2 (* 3 i)))
                  span (fn [axis] (abs (- (aget P (+ axis (* 3 a))) (aget P (+ axis (* 3 b))))))
                  ;; Search along the axis the piece spans least.
                  axis (let [sx (span 0) sy (span 1) sz (span 2)]
                         (cond (and (<= sx sy) (<= sx sz)) 0 (<= sy sz) 1 :else 2))
                  x (aget P (+ axis (* 3 a))) y (aget P (+ axis (* 3 b)))
                  {ids :ids vs :vals} (by-axis axis)
                  from (lower-bound vs (- (min x y) tol))
                  to (lower-bound vs (+ (max x y) tol))]
              (each-piece! (stops P a b ids from to)
                           (fn [x y]
                             (let [k (pair-key x y)]
                               (push! (or (hget piece-planes k) (hput! piece-planes k (alist))) p))))))
        out (alist)]
    (heach piece-planes
           (fn [k ps]
             (let [n (size ps)
                   crease? (fn [i j]
                             (let [[ax ay az] (at plane-n (ati ps i))
                                   [bx by bz] (at plane-n (ati ps j))]
                               (< (+ (* ax bx) (* ay by) (* az bz)) cos-t)))]
               (when (or (== 1 n)
                         (loop [i 0 j 1]
                           (cond (>= i n) false
                                 (>= j n) (recur (inc i) (+ i 2))
                                 (crease? i j) true
                                 :else (recur i (inc j)))))
                 (let [a (* 3 (quot k nv)) b (* 3 (mod k nv))]
                   (push! out (aget P a)) (push! out (aget P (+ a 1))) (push! out (aget P (+ a 2)))
                   (push! out (aget P b)) (push! out (aget P (+ b 1))) (push! out (aget P (+ b 2))))))))
    #?(:clj (double-array out) :cljs out)))
