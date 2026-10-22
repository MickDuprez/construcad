(ns modeller.profile
  "Section outlines as closed loops in local XY, +Y the section depth (\"up\"
  for a beam), centred on the bounding box. Loops are polyline vertices
  [x y] or [x y bulge], the bulge meaning what it does in a CAD lwpolyline:
  tan(sweep/4) of the arc from this vertex to the next, positive
  counter-clockwise. Root radii stay exact arcs; readers tessellate.

    {:outer [[x y b] ...] :holes [[[x y b] ...]] :b width :d depth}"
)

;; --- building loops -----------------------------------------------------------

(defn- dir2 [[ax ay] [bx by]]
  (let [dx (- bx ax) dy (- by ay) m (Math/sqrt (+ (* dx dx) (* dy dy)))]
    [(/ dx m) (/ dy m)]))

(defn- round-corner
  "Replace corner `p` (between `prev` and `next`) with a tangent arc of radius r:
  two vertices, the first carrying the arc's bulge."
  [prev p next r]
  (let [[ix iy] (dir2 prev p)
        [ox oy] (dir2 p next)
        cross (- (* ix oy) (* iy ox))
        turn (Math/atan2 cross (+ (* ix ox) (* iy oy)))
        t (* r (Math/tan (/ (Math/abs turn) 2)))
        [px py] p]
    [[(- px (* ix t)) (- py (* iy t)) (Math/tan (/ turn 4))]
     [(+ px (* ox t)) (+ py (* oy t))]]))

(defn round-corners
  "Closed loop of [x y] points with a radius per vertex (0 or nil = sharp)."
  [pts radii]
  (let [n (count pts)]
    (vec (mapcat (fn [i]
                   (let [p (nth pts i) r (nth radii i 0)]
                     (if (and r (pos? r))
                       (round-corner (nth pts (mod (dec i) n)) p (nth pts (mod (inc i) n)) r)
                       [p])))
                 (range n)))))

(defn rect-loop [b d]
  (let [hb (/ b 2.0) hd (/ d 2.0)]
    [[(- hb) (- hd)] [hb (- hd)] [hb hd] [(- hb) hd]]))

(defn circle-loop
  "A full circle as two half-circle arcs (bulge 1), counter-clockwise."
  [r]
  [[r 0 1.0] [(- r) 0 1.0]])

(defn reverse-loop
  "Same loop, opposite direction; bulges move to the other end and flip sign."
  [loop]
  (let [n (count loop)
        bulge #(nth % 2 0)]
    (vec (for [i (range n)]
           (let [p (nth loop (- n 1 i))
                 before (nth loop (mod (- n 2 i) n))
                 b (- (bulge before))]
             (if (zero? b) [(p 0) (p 1)] [(p 0) (p 1) b]))))))

;; --- reading loops ------------------------------------------------------------

(defn- arc-points
  "Points strictly between a and b along the arc with bulge `bulge`."
  [[ax ay] [bx by] bulge segments]
  (let [sweep (* 4 (Math/atan bulge))
        n (max 1 (int (Math/ceil (* segments (/ (Math/abs sweep) (/ Math/PI 2))))))
        dx (- bx ax) dy (- by ay) c (Math/sqrt (+ (* dx dx) (* dy dy)))
        k (/ (- 1 (* bulge bulge)) (* 4 bulge))
        cx (- (/ (+ ax bx) 2) (* k dy))
        cy (+ (/ (+ ay by) 2) (* k dx))
        r (Math/sqrt (+ (* (- ax cx) (- ax cx)) (* (- ay cy) (- ay cy))))
        a0 (Math/atan2 (- ay cy) (- ax cx))]
    (when (pos? c)
      (for [i (range 1 n)]
        (let [a (+ a0 (* sweep (/ i n)))]
          [(+ cx (* r (Math/cos a))) (+ cy (* r (Math/sin a)))])))))

(defn tessellate-loop
  "Plain [x y] points, arcs split into `segments` pieces per quarter turn."
  ([loop] (tessellate-loop loop 8))
  ([loop segments]
   (let [n (count loop)]
     (vec (mapcat (fn [i]
                    (let [p (nth loop i) q (nth loop (mod (inc i) n)) b (nth p 2 0)]
                      (cons [(p 0) (p 1)]
                            (when-not (zero? b) (arc-points p q b segments)))))
                  (range n))))))

(defn- signed-area [pts]
  (/ (reduce + (map (fn [[ax ay] [bx by]] (- (* ax by) (* bx ay)))
                    pts (concat (rest pts) [(first pts)])))
     2.0))

(defn area
  "Net area (outer less holes), via a fine tessellation."
  [{:keys [outer holes]}]
  (- (Math/abs (signed-area (tessellate-loop outer 64)))
     (reduce + (map #(Math/abs (signed-area (tessellate-loop % 64))) holes))))

(defn bounds [{:keys [outer]}]
  (let [pts (tessellate-loop outer 16)
        xs (map first pts) ys (map second pts)]
    {:min [(apply min xs) (apply min ys)] :max [(apply max xs) (apply max ys)]}))

(defn centre
  "Shift every loop so the bounding box is centred on the origin, and record
  its width :b and depth :d."
  [{:keys [outer holes] :as outline}]
  (let [{[x0 y0] :min [x1 y1] :max} (bounds outline)
        cx (/ (+ x0 x1) 2) cy (/ (+ y0 y1) 2)
        shift (fn [loop] (mapv (fn [[x y & b]] (into [(- x cx) (- y cy)] b)) loop))]
    {:outer (shift outer) :holes (mapv shift (or holes []))
     :b (- x1 x0) :d (- y1 y0)}))

;; --- insertion points -----------------------------------------------------------

(def insertion-points
  "The nine points an XLine can pass through, as fractions of [b d] measured
  from the bounding-box centre. Seen end-on with the top flange up."
  {:top-left [-0.5 0.5] :top-centre [0 0.5] :top-right [0.5 0.5]
   :mid-left [-0.5 0] :centre [0 0] :mid-right [0.5 0]
   :bottom-left [-0.5 -0.5] :bottom-centre [0 -0.5] :bottom-right [0.5 -0.5]})

(defn insertion-shift
  "How far to move the profile so `point` lands on the XLine."
  [{:keys [b d]} point]
  (let [[fx fy] (get insertion-points (or point :centre) [0 0])]
    [(- (* fx b)) (- (* fy d))]))

(defn face-planes
  "Bounding faces of a profile in its own XY, outward normals. Good for I,
  channel, hollow and rectangular sections; refined per type later."
  [{:keys [b d]}]
  (let [hb (/ b 2.0) hd (/ d 2.0)]
    {:top    {:origin [0 hd 0] :normal [0 1 0]}
     :bottom {:origin [0 (- hd) 0] :normal [0 -1 0]}
     :right  {:origin [hb 0 0] :normal [1 0 0]}
     :left   {:origin [(- hb) 0 0] :normal [-1 0 0]}}))

