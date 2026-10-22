(ns modeller.shapes
  "Builders for the standard section shapes, written in plain Clojure. Each
  takes a row of table data, named as the ASI tables print it, and returns
  {:outer loop :holes [loop ...]}. The library centres the result."
  (:require [modeller.profile :as p]))

(defn rect [{:keys [b d]}]
  {:outer (p/rect-loop b d)})

(defn i-section
  "UB, UC, UBP, WB, WC: symmetric I with root radius r1 at the four
  web-flange corners."
  [{:keys [d bf tf tw r1] :or {r1 0}}]
  (let [hb (/ bf 2) hd (/ d 2) hw (/ tw 2)]
    {:outer (p/round-corners
             [[(- hb) (- hd)] [hb (- hd)] [hb (+ (- hd) tf)] [hw (+ (- hd) tf)]
              [hw (- hd tf)] [hb (- hd tf)] [hb hd] [(- hb) hd]
              [(- hb) (- hd tf)] [(- hw) (- hd tf)] [(- hw) (+ (- hd) tf)] [(- hb) (+ (- hd) tf)]]
             [0 0 0 r1 r1 0 0 0 0 r1 r1 0])}))

(defn channel
  "PFC: web on the left, toes pointing right, root radius r1."
  [{:keys [d bf tf tw r1] :or {r1 0}}]
  (let [hd (/ d 2)]
    {:outer (p/round-corners
             [[0 (- hd)] [bf (- hd)] [bf (+ (- hd) tf)] [tw (+ (- hd) tf)]
              [tw (- hd tf)] [bf (- hd tf)] [bf hd] [0 hd]]
             [0 0 0 r1 r1 0 0 0])}))

(defn angle
  "EA/UA: heel at bottom-left, leg b up, leg d across (d defaults to b for an
  equal angle); root radius r1, toe radius r2."
  [{:keys [b d t r1 r2] :or {r1 0 r2 0}}]
  (let [d (or d b)]
    {:outer (p/round-corners
             [[0 0] [d 0] [d t] [t t] [t b] [0 b]]
             [0 0 r2 r1 r2 0])}))

(defn rhs
  "SHS/RHS: outer corner radius ro (default 2t), inner ri (default t)."
  [{:keys [b d t ro ri]}]
  (let [d (or d b) ro (or ro (* 2 t)) ri (or ri t)]
    {:outer (p/round-corners (p/rect-loop b d) [ro ro ro ro])
     :holes [(p/reverse-loop (p/round-corners (p/rect-loop (- b t t) (- d t t)) [ri ri ri ri]))]}))

(defn chs [{:keys [od t]}]
  {:outer (p/circle-loop (/ od 2))
   :holes (if t [(p/reverse-loop (p/circle-loop (- (/ od 2) t)))] [])})

(def standard
  "Builder name -> function, the shapes every library starts with."
  {:rect rect :i-section i-section :channel channel :angle angle :rhs rhs :chs chs})
