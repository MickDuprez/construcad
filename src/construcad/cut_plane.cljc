(ns construcad.cut-plane
  "Sizing the display quad for a plane cut (construcad.app.reader draws it):
  a flat rectangle in the cut plane, centred on where the plane meets the
  solid it cuts and a margin bigger than that cross-section."
  (:require [modeller.vec :as v]))

(def default-margin
  "Each side of the cross-section: a fraction of its extent, at least :min mm."
  {:fraction 0.15 :min 25})

(defn plane-axes
  "Two unit axes [u v] in the plane with unit normal n, u x v = n."
  [n]
  (let [seed (if (> (Math/abs (double (n 0))) 0.9) [0 1 0] [1 0 0])
        u (v/normalize (v/v- seed (v/scale n (v/dot seed n))))]
    [u (v/cross n u)]))

(defn section-points
  "Where the triangle edges of a soup (flat coordinates, nine per triangle)
  cross the plane {:origin :normal}: the outline of the cross-section."
  [positions {:keys [origin normal]}]
  (let [n (v/normalize normal)
        pt (fn [i] [(nth positions i) (nth positions (+ i 1)) (nth positions (+ i 2))])
        dist (fn [p] (v/dot n (v/v- p origin)))]
    (vec
     (for [t (range 0 (- (count positions) 8) 9)
           :let [ps (mapv #(pt (+ t (* 3 %))) (range 3))
                 ds (mapv dist ps)]
           k (range 3)
           :let [a (ps k) b (ps (mod (inc k) 3)) da (ds k) db (ds (mod (inc k) 3))]
           :when (or (and (<= da 0) (> db 0)) (and (> da 0) (<= db 0)))]
       (v/lerp a b (/ da (- da db)))))))

(defn quad
  "The display quad for a cut on plane {:origin :normal} through a solid
  whose cross-section passes through `points` ([x y z]): {:centre :corners
  [a b c d] :size [w h]}, the section's extent in the plane's axes plus
  `margin` ({:fraction :min}) each side. nil with no points."
  ([plane points] (quad plane points default-margin))
  ([{:keys [origin normal]} points {:keys [fraction min]}]
   (when (seq points)
     (let [n (v/normalize normal)
           [u w] (plane-axes n)
           uv (map (fn [p] (let [d (v/v- p origin)] [(v/dot d u) (v/dot d w)])) points)
           us (map first uv) ws (map second uv)
           [u0 u1 w0 w1] [(reduce clojure.core/min us) (reduce max us) (reduce clojure.core/min ws) (reduce max ws)]
           mu (max min (* fraction (- u1 u0)))
           mw (max min (* fraction (- w1 w0)))
           [u0 u1 w0 w1] [(- u0 mu) (+ u1 mu) (- w0 mw) (+ w1 mw)]
           at (fn [a b] (v/v+ origin (v/v+ (v/scale u a) (v/scale w b))))]
       {:centre (at (/ (+ u0 u1) 2) (/ (+ w0 w1) 2))
        :corners [(at u0 w0) (at u1 w0) (at u1 w1) (at u0 w1)]
        :size [(- u1 u0) (- w1 w0)]}))))
