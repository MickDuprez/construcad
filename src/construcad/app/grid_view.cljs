(ns construcad.app.grid-view
  "Grid lines with axis bubbles at the working level. Labels come from the
  grid record, so relabelling (modeller.grid/relabel) shows here as soon as
  the patch arrives."
  (:require ["three" :as THREE]
            [construcad.app.reader :as reader]
            [construcad.app.viewport :as vp]
            [modeller.frame :as frame]
            [modeller.grid :as grid]))

(def overrun 1500)   ; how far lines run past the outer axes, mm
(def bubble 900)     ; bubble diameter, mm

(defn- bubble-sprite [label]
  (let [canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) 128)
    (set! (.-height canvas) 128)
    (set! (.-fillStyle ctx) "#ffffff")
    (set! (.-strokeStyle ctx) "#b03a2e")
    (set! (.-lineWidth ctx) 6)
    (.beginPath ctx)
    (.arc ctx 64 64 56 0 (* 2 js/Math.PI))
    (.fill ctx)
    (.stroke ctx)
    (set! (.-fillStyle ctx) "#7b241c")
    (set! (.-font ctx) "bold 58px sans-serif")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx label 64 68)
    (let [sprite (THREE/Sprite. (THREE/SpriteMaterial. #js {:map (THREE/CanvasTexture. canvas)
                                                            :depthTest false :transparent true}))]
      (.setScalar (.-scale sprite) bubble)
      (set! (.-renderOrder sprite) 900)
      sprite)))

(defn- axis-line [a b]
  (let [line (THREE/Line. (.setFromPoints (THREE/BufferGeometry.) #js [(vp/v3 a) (vp/v3 b)])
                          (THREE/LineDashedMaterial. #js {:color 0xb03a2e :dashSize 900 :gapSize 200
                                                          :transparent true :opacity 0.8 :depthWrite false}))]
    (.computeLineDistances line)
    (set! (.-renderOrder line) -1)
    line))

(defn draw!
  "Redraw the grid at elevation z."
  [g z]
  (let [^js root (:grid-root @vp/vp*)]
    (doseq [c (vec (array-seq (.-children root)))] (reader/dispose! c))
    (when g
      (let [f (grid/grid-frame g)
            at (fn [x y] (frame/point->parent f [x y z]))
            xs (map :offset (:grid/x-axes g))
            ys (map :offset (:grid/y-axes g))
            [x0 x1] [(- (apply min xs) overrun) (+ (apply max xs) overrun)]
            [y0 y1] [(- (apply min ys) overrun) (+ (apply max ys) overrun)]
            r (/ bubble 2)]
        (doseq [{:keys [offset label]} (:grid/x-axes g)]
          (.add root (axis-line (at offset y0) (at offset y1)))
          (doseq [[y side] [[y0 -1] [y1 1]]]
            (doto (bubble-sprite label)
              (-> .-position (.copy (vp/v3 (at offset (+ y (* side r))))))
              (->> (.add root)))))
        (doseq [{:keys [offset label]} (:grid/y-axes g)]
          (.add root (axis-line (at x0 offset) (at x1 offset)))
          (doseq [[x side] [[x0 -1] [x1 1]]]
            (doto (bubble-sprite label)
              (-> .-position (.copy (vp/v3 (at (+ x (* side r)) offset))))
              (->> (.add root)))))))))
