(ns modeller.vec
  "3D vectors as plain [x y z] vectors, and the one tolerance policy.")

(def ^:const eps
  "Length tolerance in model units (mm)."
  1e-6)

(def ^:const angle-eps
  "Tolerance on unit-vector dot products, about 0.08 degrees."
  1e-6)

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))
(defn scale [a s] (mapv #(* % s) a))
(defn neg [a] (scale a -1))
(defn dot [a b] (reduce + (map * a b)))

(defn cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn magnitude [a] (Math/sqrt (dot a a)))
(defn distance [a b] (magnitude (v- b a)))
(defn nearly-zero? [a] (< (magnitude a) eps))

(defn normalize
  "Unit vector, or nil for a zero-length vector. Callers decide what nil means."
  [a]
  (let [m (magnitude a)]
    (when (> m eps) (scale a (/ 1.0 m)))))

(defn parallel?
  "True when unit vectors a and b point the same or opposite way."
  [a b]
  (< (- 1.0 (Math/abs (double (dot a b)))) angle-eps))

(defn lerp [a b t] (v+ a (scale (v- b a) t)))

(defn nearly= 
  ([a b] (nearly= a b eps))
  ([a b tol]
   (if (number? a)
     (< (Math/abs (double (- a b))) tol)
     (and (= (count a) (count b))
          (every? true? (map #(nearly= %1 %2 tol) a b))))))

(defn deg->rad [d] (* d (/ Math/PI 180.0)))
