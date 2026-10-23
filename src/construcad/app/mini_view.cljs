(ns construcad.app.mini-view
  "A small shaded three.js view for the template editors' previews: left drag
  orbits (about the part under the cursor), right or middle drag pans, the wheel zooms. Z up, orthographic, as
  the main viewport."
  (:require ["three" :as THREE]
            ["three/addons/controls/OrbitControls.js" :refer [OrbitControls]]
            [construcad.app.orbit :as orbit]
            [construcad.app.reader :as reader]
            [goog.object :as gobj]))

(defn- size [^js el] [(max 1 (.-clientWidth el)) (max 1 (.-clientHeight el))])

(defn- set-frustum! [view half-height & [aspect]]
  (let [{:keys [^js camera el]} @view
        [w h] (size el) a (or aspect (/ w h))]
    (set! (.-top camera) half-height)
    (set! (.-bottom camera) (- half-height))
    (set! (.-left camera) (- (* half-height a)))
    (set! (.-right camera) (* half-height a))
    (.updateProjectionMatrix camera)))

(defn look!
  "Look along `dir` (eye side) at everything shown, fitted to the view (or
  to a frame of `aspect`, width over height)."
  [view [dx dy dz :as dir] & {:keys [aspect]}]
  (let [{:keys [^js camera ^js controls ^js root]} @view
        box (.setFromObject (THREE/Box3.) root)
        empty? (.isEmpty box)
        centre (if empty? (THREE/Vector3.) (.getCenter box (THREE/Vector3.)))
        span (if empty? 2000 (max 200 (.length (.getSize box (THREE/Vector3.)))))
        d (js/Math.sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (.copy (.-position camera) (.addScaledVector (.clone centre) (THREE/Vector3. (/ dx d) (/ dy d) (/ dz d)) (* 2 span)))
    (.set (.-up camera) 0 0 1)
    (set! (.-near camera) 1)
    (set! (.-far camera) (* 10 span))
    (set! (.-zoom camera) 1)
    (.copy (.-target controls) centre)
    (.lookAt camera centre)
    (.updateMatrixWorld camera)
    ;; fit what is shown as the camera sees it, not its diagonal, so a long
    ;; member fills a wide view
    (let [[w h] (size (:el @view))
          a (or aspect (/ w h))
          inv (.-matrixWorldInverse camera)
          lo (.-min box) hi (.-max box)
          half (if empty?
                 (* 0.55 span)
                 (reduce max 1 (for [x [(.-x lo) (.-x hi)] y [(.-y lo) (.-y hi)] z [(.-z lo) (.-z hi)]
                                     :let [p (.applyMatrix4 (THREE/Vector3. x y z) inv)]]
                                 (max (js/Math.abs (.-y p)) (/ (js/Math.abs (.-x p)) a)))))]
      (set-frustum! view (* 1.12 half) a))
    (.update controls)
    (swap! view assoc :dir dir :moved? false)))

(defn- node-marker
  "A small ball at an XLine node, drawn through the solids like the XLine."
  [[x y z] radius]
  (let [o (THREE/Mesh. (THREE/SphereGeometry. radius 16 12)
                       (THREE/MeshBasicMaterial. #js {:color (:xline reader/colours) :depthTest false
                                                      :transparent true}))]
    (.set (.-position o) x y z)
    (set! (.-renderOrder o) 11)
    o))

(defn show!
  "Replace what the view shows with these instructions ({id instr}), and a
  marker at each of `nodes` (XLine nodes, [x y z]). Fits the view the first
  time, or when `fit?`."
  [view instructions & {:keys [fit? nodes]}]
  (let [{:keys [^js root shown?]} @view]
    (doseq [^js o (vec (array-seq (.-children root)))] (reader/dispose! o))
    (doseq [[id instr] instructions]
      (let [^js o (reader/assembly-object id instr)]
        (.traverse o (fn [^js n]
                       (when (= "hidden-edge" (gobj/get (.-userData n) "renderRole"))
                         (set! (.-visible n) false))))
        (.add root o)))
    (when (seq nodes)
      (let [span (reduce max 0 (for [a nodes b nodes] (js/Math.hypot (- (a 0) (b 0)) (- (a 1) (b 1)) (- (a 2) (b 2)))))
            r (max 12 (* 0.008 span))]
        (doseq [p nodes] (.add root (node-marker p r)))))
    (when (or fit? (not shown?))
      (look! view (:dir @view))
      (swap! view assoc :shown? true))))

(defn create!
  "A view rendering into a new canvas inside `el`, looking along `dir`."
  [^js el dir]
  (let [renderer (THREE/WebGLRenderer. #js {:antialias true})
        scene (THREE/Scene.)
        camera (THREE/OrthographicCamera. -1 1 1 -1 1 100000)
        root (THREE/Group.)
        sun (THREE/DirectionalLight. 0xffffff 2.2)
        _ (.set (.-up camera) 0 0 1)
        controls (OrbitControls. camera (.-domElement renderer))
        view (atom {:el el :renderer renderer :scene scene :camera camera :controls controls
                    :root root :dir dir :alive? true})]
    (.setPixelRatio renderer (min 2 (.-devicePixelRatio js/window)))
    (set! (.-background scene) (THREE/Color. 0xf3f5f6))
    (.set (.-position sun) 5000 -7000 10000)
    (.add scene (THREE/HemisphereLight. 0xffffff 0x52606d 2.0) sun root)
    (set! (.-zoomToCursor controls) true)
    (set! (.. controls -mouseButtons -MIDDLE) THREE/MOUSE.PAN)
    (set! (.. controls -mouseButtons -RIGHT) THREE/MOUSE.PAN)
    (.addEventListener (.-domElement renderer) "contextmenu" #(.preventDefault %))
    (.addEventListener controls "start" #(swap! view assoc :moved? true))
    (orbit/install! (.-domElement renderer) camera controls (fn [] root))
    (.append el (.-domElement renderer))
    (let [resize! (fn []
                    (let [[w h] (size el)
                          laid-out? (and (> w 1) (> h 1))]
                      (.setSize renderer w h false)
                      (set-frustum! view (.-top camera))
                      ;; shown before the canvas had a size, or resized before
                      ;; the user moved the camera: fit again
                      (when (and laid-out? (:shown? @view)
                                 (or (not (:fitted? @view)) (not (:moved? @view))))
                        (look! view (:dir @view)))
                      (when laid-out? (swap! view assoc :fitted? (boolean (:shown? @view))))))
          observer (js/ResizeObserver. resize!)]
      (.observe observer el)
      (swap! view assoc :observer observer)
      (resize!))
    (letfn [(frame! []
              (when (:alive? @view)
                (js/requestAnimationFrame frame!)
                (.update controls)
                (.render renderer scene camera)))]
      (frame!))
    view))

(defn snapshot
  "A small PNG data URL, `w` x `h`, of what the view shows, seen from where
  the camera is now but fitted to that frame, for a template's thumbnail.
  The view is put back as it was."
  [view w h]
  (let [{:keys [^js renderer ^js scene ^js camera ^js controls el]} @view
        [vw vh] (size el)
        eye (.sub (.clone (.-position camera)) (.-target controls))
        saved {:position (.clone (.-position camera)) :target (.clone (.-target controls))
               :zoom (.-zoom camera) :near (.-near camera) :far (.-far camera)
               :frustum [(.-left camera) (.-right camera) (.-top camera) (.-bottom camera)]}
        ^js src (.-domElement renderer)]
    (.setSize renderer (* 2 w) (* 2 h) false)
    (look! view [(.-x eye) (.-y eye) (.-z eye)] :aspect (/ w h))
    (.render renderer scene camera)
    (let [^js out (.createElement js/document "canvas")
          _ (set! (.-width out) w)
          _ (set! (.-height out) h)
          ^js g (.getContext out "2d")]
      (set! (.-imageSmoothingQuality g) "high")
      (.drawImage g src 0 0 w h)
      ;; back as it was
      (.setSize renderer vw vh false)
      (let [[l r t b] (:frustum saved)]
        (set! (.-left camera) l) (set! (.-right camera) r) (set! (.-top camera) t) (set! (.-bottom camera) b))
      (set! (.-zoom camera) (:zoom saved))
      (set! (.-near camera) (:near saved))
      (set! (.-far camera) (:far saved))
      (.copy (.-position camera) (:position saved))
      (.copy (.-target controls) (:target saved))
      (.lookAt camera (:target saved))
      (.updateProjectionMatrix camera)
      (.update controls)
      (.toDataURL out "image/png"))))

(defn dispose! [view]
  (when view
    (let [{:keys [^js renderer ^js root ^js controls ^js observer]} @view]
      (swap! view assoc :alive? false)
      (.disconnect observer)
      (.dispose controls)
      (doseq [^js o (vec (array-seq (.-children root)))] (reader/dispose! o))
      (.dispose renderer)
      (.forceContextLoss renderer)
      (some-> (.-domElement renderer) .remove))))
