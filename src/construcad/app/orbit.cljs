(ns construcad.app.orbit
  "Orbit about what is under the cursor. OrbitControls rotates about its
  target, which a fit leaves at the middle of everything shown; zoomed in on
  one connection, that point can be metres away and the parts swing off
  screen. On each press the target moves along the line of sight to the
  depth of the nearest face under the cursor (or, failing that, under the
  middle of the view), and while the drag turns the view the camera and
  target slide together in the view plane so that the point pressed stays
  under the cursor: the view turns about that point. The camera does not
  move on the press itself, so the picture does not jump; pan and zoom do
  not turn the view and are left alone."
  (:require ["three" :as THREE]
            [goog.object :as gobj]))

(defn- face? [^js o]
  (and (.-isMesh o) (= "face" (gobj/get (.-userData o) "renderRole"))
       (loop [^js n o] (cond (nil? n) true (not (.-visible n)) false :else (recur (.-parent n))))))

(defn- hit-point
  "The nearest visible face point under normalised device point [x y], or nil."
  [^js camera ^js root [x y]]
  (let [r (THREE/Raycaster.)]
    (.setFromCamera r (THREE/Vector2. x y) camera)
    (some #(when (face? (.-object ^js %)) (.-point ^js %)) (.intersectObject r root true))))

(defn- ndc [^js camera ^js p]
  (let [v (.project (.clone p) camera)] [(.-x v) (.-y v)]))

(defn view-shift
  "Right/up distances to slide an orthographic camera (frustum left right
  top bottom, zoom) by so that a point drawn at normalised device point
  `now` is drawn at `want` again."
  [[left right top bottom zoom] [wx wy] [nx ny]]
  [(* (- nx wx) (/ (- right left) 2 zoom))
   (* (- ny wy) (/ (- top bottom) 2 zoom))])

(defn- re-pivot!
  "Move `controls`' target to the depth of the part under canvas pixel
  [px py]; returns the anchor {:point :want :quaternion}, or nil."
  [^js camera ^js controls ^js root ^js canvas [px py]]
  (let [w (max 1 (.-clientWidth canvas)) h (max 1 (.-clientHeight canvas))
        at [(- (* 2 (/ px w)) 1) (- 1 (* 2 (/ py h)))]]
    (when-let [^js p (or (hit-point camera root at) (hit-point camera root [0 0]))]
      (let [dir (.getWorldDirection camera (THREE/Vector3.))
            d (.dot (.sub (.clone p) (.-position camera)) dir)]
        (.copy (.-target controls) (.addScaledVector (.clone (.-position camera)) dir d))
        (.update controls)
        {:point (.clone p) :want (ndc camera p) :quaternion (.clone (.-quaternion camera))}))))

(defn- hold-anchor!
  "After OrbitControls turns the view, slide camera and target together so
  the anchor point is drawn where it was pressed. A pan leaves the
  orientation alone, give or take lookAt's rounding (about 1e-8 rad), so
  the turn is measured from the press and only a real one counts."
  [^js camera ^js controls {:keys [^js point want ^js quaternion]}]
  (.updateMatrixWorld camera)
  (when (and (.-isOrthographicCamera camera) (> (.angleTo quaternion (.-quaternion camera)) 1e-4))
    (let [[sx sy] (view-shift [(.-left camera) (.-right camera) (.-top camera) (.-bottom camera) (.-zoom camera)]
                              want (ndc camera point))
          e (.-elements (.-matrixWorld camera))
          s (-> (THREE/Vector3. (aget e 0) (aget e 1) (aget e 2)) (.multiplyScalar sx)
                (.add (.multiplyScalar (THREE/Vector3. (aget e 4) (aget e 5) (aget e 6)) sy)))]
      (.add (.-position camera) s)
      (.add (.-target controls) s)
      (.updateMatrixWorld camera))))

(defn install!
  "Re-pivot on every press on `canvas`, before OrbitControls sees it, and
  hold the pressed point still while the drag turns the view. `root-fn`
  returns the group holding the pickable faces."
  [^js canvas ^js camera ^js controls root-fn]
  (let [anchor (atom nil)
        release! #(reset! anchor nil)]
    (.addEventListener canvas "pointerdown"
                       (fn [^js e]
                         (let [rect (.getBoundingClientRect canvas)]
                           (reset! anchor
                                   (when-let [root (root-fn)]
                                     (re-pivot! camera controls root canvas
                                                [(- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect))])))))
                       #js {:capture true})
    (.addEventListener canvas "pointerup" release!)
    (.addEventListener canvas "pointercancel" release!)
    (.addEventListener controls "end" release!)
    (.addEventListener controls "change" #(some->> @anchor (hold-anchor! camera controls)))))
