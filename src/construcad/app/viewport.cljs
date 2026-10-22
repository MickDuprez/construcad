(ns construcad.app.viewport
  "Renderer, orthographic camera, navigation, view cube and display modes.
  The camera, view cube and hidden-line handling are ported from
  schemacraft-viewer (core.cljs): Z up, orthographic by default, the left
  button kept for tools, middle/right drag to pan, Shift + middle/right drag
  to orbit, wheel to zoom at the cursor."
  (:require ["three" :as THREE]
            ["three/addons/controls/OrbitControls.js" :refer [OrbitControls]]
            [goog.object :as gobj]))

(defonce vp* (atom nil))

(defn v3 [[x y z]] (THREE/Vector3. (or x 0) (or y 0) (or z 0)))
(defn xyz [^js v] [(.-x v) (.-y v) (.-z v)])

(defn size []
  (let [el (:el @vp*)] [(max 1 (.-clientWidth el)) (max 1 (.-clientHeight el))]))

;; --- display modes -------------------------------------------------------------

(defn- apply-display! []
  (let [{:keys [^js model-root display suppressed? show-xlines?]} @vp*
        mode (if (and suppressed? (= :hidden-line display)) :shaded display)]
    (when model-root
      (.traverse model-root
                 (fn [^js node]
                   (case (gobj/get (.-userData node) "renderRole")
                     "face" (do (set! (.-visible node) (not= :wireframe mode))
                                (set! (.. node -material -colorWrite) (not= :hidden-line mode)))
                     "visible-edge" (set! (.-visible node) true)
                     "hidden-edge" (set! (.-visible node) (= :hidden-line mode))
                     "xline" (set! (.-visible node) show-xlines?)
                     nil))))))

(defn set-display! [mode] (swap! vp* assoc :display mode) (apply-display!))
(defn show-xlines! [on?] (swap! vp* assoc :show-xlines? on?) (apply-display!))
(defn refresh-display! [] (apply-display!))

;; Orbiting in hidden-line mode is slow to read; show shaded while the view
;; moves, as schemacraft-viewer's suppress-hidden-line! does.
(defn- suppress! [] (swap! vp* assoc :suppressed? true) (apply-display!))
(defn- resume! [] (swap! vp* assoc :suppressed? false) (apply-display!))

;; --- camera ----------------------------------------------------------------------

(defn world-per-pixel []
  (let [^js camera (:camera @vp*) [_ h] (size)]
    (/ (- (.-top camera) (.-bottom camera)) (.-zoom camera) h)))

(defn- set-frustum! [^js camera half-height]
  (let [[w h] (size) a (/ w h)]
    (set! (.-top camera) half-height)
    (set! (.-bottom camera) (- half-height))
    (set! (.-left camera) (- (* half-height a)))
    (set! (.-right camera) (* half-height a))
    (.updateProjectionMatrix camera)))

(defn- resize! []
  (let [{:keys [^js renderer ^js camera]} @vp* [w h] (size)]
    (.setSize renderer w h false)
    (set-frustum! camera (.-top camera))))

(defn content-box []
  (let [box (THREE/Box3.)]
    (doseq [^js o [(:model-root @vp*) (:grid-root @vp*)]] (.expandByObject box o))
    box))

(def standard-directions
  {"top" [0 0 1] "bottom" [0 0 -1] "front" [0 -1 0] "back" [0 1 0]
   "left" [-1 0 0] "right" [1 0 0]
   "iso" [1 -1 1]
   "iso-top-front-right" [1 -1 1] "iso-top-front-left" [-1 -1 1]
   "iso-top-back-right" [1 1 1] "iso-top-back-left" [-1 1 1]
   "iso-bottom-front-right" [1 -1 -1] "iso-bottom-front-left" [-1 -1 -1]
   "iso-bottom-back-right" [1 1 -1] "iso-bottom-back-left" [-1 1 -1]})

(defn look!
  "Look along direction `dir` (from the eye towards the target, reversed) at
  the content, fitted to the view."
  ([dir] (look! dir (content-box)))
  ([[dx dy dz] ^js box]
   (let [{:keys [^js camera ^js controls]} @vp*
         empty? (.isEmpty box)
         centre (if empty? (THREE/Vector3.) (.getCenter box (THREE/Vector3.)))
         span (if empty? 10000 (max 1000 (.length (.getSize box (THREE/Vector3.)))))
         d (js/Math.sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
         eye (.addScaledVector (.clone centre) (THREE/Vector3. (/ dx d) (/ dy d) (/ dz d)) (* 2 span))]
     (.copy (.-position camera) eye)
     (.set (.-up camera) 0 0 1)
     (when (and (zero? dx) (zero? dy)) (.set (.-up camera) 0 1 0))
     ;; The eye sits 2 spans out; a positive near plane keeps the pick ray's
     ;; origin in front of everything (three.js starts it at the near plane).
     (set! (.-near camera) 1)
     (set! (.-far camera) (* 10 span))
     (set! (.-zoom camera) 1)
     (set-frustum! camera (* 0.42 span))
     (.copy (.-target controls) centre)
     (.lookAt camera centre)
     (.update controls)
     (resume!))))

(defn standard-view! [name]
  (when-let [dir (get standard-directions name)] (look! dir)))

(defn zoom-extents! []
  (let [^js camera (:camera @vp*)
        dir (.sub (.clone (.-position camera)) (.-target (:controls @vp*)))]
    (look! (xyz dir))))

;; --- view cube (ported from schemacraft-viewer) -----------------------------------

(def cube-faces
  [{:view "top" :label "TOP" :normal [0 0 1] :label-u [1 0 0] :label-v [0 -1 0]
    :points [[-0.6 -1 1] [0.6 -1 1] [1 -0.6 1] [1 0.6 1] [0.6 1 1] [-0.6 1 1] [-1 0.6 1] [-1 -0.6 1]]}
   {:view "bottom" :label "BOTTOM" :normal [0 0 -1] :label-u [-1 0 0] :label-v [0 -1 0]
    :points [[-1 -0.6 -1] [-1 0.6 -1] [-0.6 1 -1] [0.6 1 -1] [1 0.6 -1] [1 -0.6 -1] [0.6 -1 -1] [-0.6 -1 -1]]}
   {:view "front" :label "FRONT" :normal [0 -1 0] :label-u [1 0 0] :label-v [0 0 -1]
    :points [[-1 -1 -0.6] [-1 -1 0.6] [-0.6 -1 1] [0.6 -1 1] [1 -1 0.6] [1 -1 -0.6] [0.6 -1 -1] [-0.6 -1 -1]]}
   {:view "back" :label "BACK" :normal [0 1 0] :label-u [-1 0 0] :label-v [0 0 -1]
    :points [[-0.6 1 -1] [0.6 1 -1] [1 1 -0.6] [1 1 0.6] [0.6 1 1] [-0.6 1 1] [-1 1 0.6] [-1 1 -0.6]]}
   {:view "right" :label "RIGHT" :normal [1 0 0] :label-u [0 1 0] :label-v [0 0 -1]
    :points [[1 -0.6 -1] [1 0.6 -1] [1 1 -0.6] [1 1 0.6] [1 0.6 1] [1 -0.6 1] [1 -1 0.6] [1 -1 -0.6]]}
   {:view "left" :label "LEFT" :normal [-1 0 0] :label-u [0 -1 0] :label-v [0 0 -1]
    :points [[-1 -1 -0.6] [-1 -1 0.6] [-1 -0.6 1] [-1 0.6 1] [-1 1 0.6] [-1 1 -0.6] [-1 0.6 -1] [-1 -0.6 -1]]}])

(defn- corner-view [sx sy sz]
  (str "iso-" (if (pos? sz) "top" "bottom") "-" (if (neg? sy) "front" "back") "-" (if (pos? sx) "right" "left")))

(def cube-chamfers
  (vec (for [sx [-1 1] sy [-1 1] sz [-1 1]]
         {:view (corner-view sx sy sz) :label "ISO" :normal [sx sy sz]
          :points [[(* sx 0.6) sy sz] [sx (* sy 0.6) sz] [sx sy (* sz 0.6)]]})))

(defn- svg-el [tag] (.createElementNS js/document "http://www.w3.org/2000/svg" tag))

(defn- cube-surface [{:keys [view label] :as surface} chamfer?]
  (let [group (svg-el "g") polygon (svg-el "polygon") title (svg-el "title")
        text (when-not chamfer? (svg-el "text"))]
    (.setAttribute group "data-cube-view" view)
    (.setAttribute polygon "class" (str "cube-surface " (if chamfer? "cube-chamfer" "cube-face")))
    (set! (.-textContent title) (if chamfer? (str "Isometric: " view) label))
    (.append group title polygon)
    (when text
      (.setAttribute text "class" "cube-label")
      (set! (.-textContent text) label)
      (.append group text))
    {:surface surface :group group :polygon polygon :text text}))

(defn- project-cube-point [^js q point]
  (let [p (.applyQuaternion (v3 point) q)]
    {:x (+ 64 (* 31 (.-x p))) :y (- 55 (* 31 (.-y p))) :z (.-z p)}))

(defn- render-cube! []
  (let [{:keys [^js camera cube-svg cube-surfaces]} @vp*
        inverse (.invert (.clone (.-quaternion camera)))
        rendered (for [{:keys [surface polygon text] :as item} cube-surfaces
                       :let [projected (mapv #(project-cube-point inverse %) (:points surface))
                             n (count projected)
                             cx (/ (reduce + (map :x projected)) n)
                             cy (/ (reduce + (map :y projected)) n)
                             normal (.applyQuaternion (v3 (:normal surface)) inverse)]]
                   (do (.setAttribute polygon "points" (apply str (interpose " " (map #(str (:x %) "," (:y %)) projected))))
                       (when text
                         (let [u (project-cube-point inverse (:label-u surface))
                               v (project-cube-point inverse (:label-v surface))]
                           (.setAttribute text "transform"
                                          (str "matrix(" (/ (- (:x u) 64) 31) " " (/ (- (:y u) 55) 31) " "
                                               (/ (- (:x v) 64) 31) " " (/ (- (:y v) 55) 31) " " cx " " cy ")"))
                           (.setAttribute text "display" (if (> (.-z normal) 0.08) "block" "none"))))
                       (assoc item :depth (/ (reduce + (map :z projected)) n))))]
    (doseq [{:keys [group]} (sort-by :depth rendered)] (.append cube-svg group))))

(defn- orbit-by! [dx dy]
  (let [{:keys [^js camera ^js controls]} @vp*
        target (.-target controls)
        offset (.sub (.clone (.-position camera)) target)
        s (.setFromVector3 (THREE/Spherical.) offset)
        eps 0.01]
    ;; Z-up: Spherical works Y-up, so swap through a rotated offset.
    (.set offset (.-x offset) (.-z offset) (- (.-y offset)))
    (.setFromVector3 s offset)
    (set! (.-theta s) (- (.-theta s) (* dx 0.012)))
    (set! (.-phi s) (max eps (min (- js/Math.PI eps) (- (.-phi s) (* dy 0.012)))))
    (.setFromSpherical offset s)
    (.set offset (.-x offset) (- (.-z offset)) (.-y offset))
    (.set (.-up camera) 0 0 1)
    (.copy (.-position camera) (.add (.clone target) offset))
    (.lookAt camera target)
    (.update controls)))

(defn- make-cube! [parent]
  (let [box (.createElement js/document "div")
        svg (svg-el "svg")
        surfaces (vec (concat (map #(cube-surface % false) cube-faces)
                              (map #(cube-surface % true) cube-chamfers)))]
    (set! (.-className box) "view-cube")
    (.setAttribute svg "viewBox" "0 0 128 112")
    (doseq [{:keys [group]} surfaces] (.append svg group))
    (.append box svg)
    (.append parent box)
    (.addEventListener svg "pointerdown"
                       (fn [e]
                         (when-let [t (some-> (.-target e) (.closest "[data-cube-view]"))]
                           (.preventDefault e)
                           (.setPointerCapture svg (.-pointerId e))
                           (swap! vp* assoc :cube-pointer {:x (.-clientX e) :y (.-clientY e) :moved 0
                                                           :view (.getAttribute t "data-cube-view")}))))
    (.addEventListener svg "pointermove"
                       (fn [e]
                         (when-let [{:keys [x y moved] :as p} (:cube-pointer @vp*)]
                           (let [dx (- (.-clientX e) x) dy (- (.-clientY e) y)
                                 moved (+ moved (js/Math.abs dx) (js/Math.abs dy))]
                             (when (> moved 3) (orbit-by! dx dy))
                             (swap! vp* assoc :cube-pointer (assoc p :x (.-clientX e) :y (.-clientY e) :moved moved))))))
    (.addEventListener svg "pointerup"
                       (fn [_]
                         (when-let [{:keys [moved view]} (:cube-pointer @vp*)]
                           (when (<= moved 3) (standard-view! view))
                           (swap! vp* dissoc :cube-pointer))))
    (swap! vp* assoc :cube-svg svg :cube-surfaces surfaces)))

;; --- picking and projection ------------------------------------------------------

(defn pointer [^js e]
  (let [rect (.getBoundingClientRect (.-domElement (:renderer @vp*)))]
    [(- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect))]))

(defn project
  "WCS point -> canvas pixels."
  [p]
  (let [[w h] (size) v (.project (v3 p) (:camera @vp*))]
    [(* 0.5 w (+ 1 (.-x v))) (* 0.5 h (- 1 (.-y v)))]))

(defn- raycaster [[x y]]
  (let [[w h] (size) r (THREE/Raycaster.)]
    (.setFromCamera r (THREE/Vector2. (- (* 2 (/ x w)) 1) (- 1 (* 2 (/ y h)))) (:camera @vp*))
    (set! (.. r -params -Line -threshold) (* 6 (world-per-pixel)))
    r))

(defn on-plane
  "Where the pointer ray meets the horizontal plane at elevation z."
  [cursor z]
  (let [target (THREE/Vector3.)
        plane (THREE/Plane. (THREE/Vector3. 0 0 1) (- z))]
    (when (.intersectPlane (.-ray (raycaster cursor)) plane target)
      (xyz target))))

(defn- visible-chain? [^js o]
  (loop [o o] (cond (nil? o) true (not (.-visible o)) false :else (recur (.-parent o)))))

(defn- assembly-id [^js o]
  (loop [o o] (when o (or (gobj/get (.-userData o) "assemblyId") (recur (.-parent o))))))

(defn pick
  "Assembly id (as a string) under the cursor, or nil."
  [cursor]
  (let [hits (.intersectObject (raycaster cursor) (:model-root @vp*) true)]
    (some (fn [^js hit]
            (let [o (.-object hit)]
              (when (and (#{"face" "xline"} (gobj/get (.-userData o) "renderRole"))
                         (visible-chain? o))
                (assembly-id o))))
          (array-seq hits))))

;; --- markers drawn over everything (grips, snap marker, rubber band) ---------------

(defn marker [colour]
  (let [m (THREE/Mesh. (THREE/BoxGeometry. 1 1 1)
                       (THREE/MeshBasicMaterial. #js {:color colour :depthTest false :transparent true}))]
    (set! (.-renderOrder m) 1000)
    (set! (.. m -userData -pixels) 10)
    (.add (:overlay-root @vp*) m)
    m))

(defn rubber-band []
  (let [line (THREE/Line. (.setFromPoints (THREE/BufferGeometry.) #js [(THREE/Vector3.) (THREE/Vector3.)])
                          (THREE/LineDashedMaterial. #js {:color 0xd2691e :dashSize 1 :gapSize 1
                                                          :depthTest false :transparent true}))]
    (set! (.-renderOrder line) 1000)
    (set! (.-visible line) false)
    (.add (:overlay-root @vp*) line)
    line))

(defn set-line! [^js line a b]
  (let [wpp (world-per-pixel)]
    (.setFromPoints (.-geometry line) #js [(v3 a) (v3 b)])
    (set! (.. line -material -dashSize) (* 8 wpp))
    (set! (.. line -material -gapSize) (* 5 wpp))
    (.computeLineDistances line)
    (set! (.-visible line) true)))

(defn- scale-markers! []
  (let [wpp (world-per-pixel)]
    (doseq [^js m (array-seq (.-children (:overlay-root @vp*)))]
      (when-let [px (.. m -userData -pixels)]
        (.setScalar (.-scale m) (* px wpp))))))

;; --- lifecycle -------------------------------------------------------------------

(defn- animate! []
  (js/requestAnimationFrame animate!)
  (let [{:keys [^js renderer scene camera ^js controls on-frame]} @vp*]
    (.update controls)
    (scale-markers!)
    (when on-frame (on-frame))
    (render-cube!)
    (.render renderer scene camera)))

(defn init!
  "Build the viewport inside element `el`. Returns the canvas."
  [el {:keys [on-frame]}]
  (set! (.-DEFAULT_UP THREE/Object3D) (THREE/Vector3. 0 0 1))
  (let [renderer (THREE/WebGLRenderer. #js {:antialias true})
        scene (THREE/Scene.)
        camera (THREE/OrthographicCamera. -1 1 1 -1 -100000 100000)
        model-root (THREE/Group.)
        grid-root (THREE/Group.)
        overlay-root (THREE/Group.)
        sun (THREE/DirectionalLight. 0xffffff 2.2)]
    (.setPixelRatio renderer (min 2 (.-devicePixelRatio js/window)))
    (set! (.-background scene) (THREE/Color. 0xf3f5f6))
    (.set (.-position sun) 5000 -7000 10000)
    (.add scene (THREE/HemisphereLight. 0xffffff 0x52606d 2.0) sun grid-root model-root overlay-root)
    (.append el (.-domElement renderer))
    (let [controls (OrbitControls. camera (.-domElement renderer))]
      (set! (.-enableDamping controls) false)
      (set! (.-zoomToCursor controls) true)
      (set! (.. controls -mouseButtons -LEFT) nil)
      (set! (.. controls -mouseButtons -MIDDLE) THREE/MOUSE.PAN)
      (set! (.. controls -mouseButtons -RIGHT) THREE/MOUSE.PAN)
      (.addEventListener controls "start" suppress!)
      (.addEventListener controls "end" resume!)
      (reset! vp* {:el el :renderer renderer :scene scene :camera camera :controls controls
                   :model-root model-root :grid-root grid-root :overlay-root overlay-root
                   :display :hidden-line :show-xlines? true :on-frame on-frame}))
    (.addEventListener (.-domElement renderer) "contextmenu" #(.preventDefault %))
    (.observe (js/ResizeObserver. resize!) el)
    (resize!)
    (make-cube! el)
    (look! [1 -1 1])
    (animate!)
    (.-domElement renderer)))
