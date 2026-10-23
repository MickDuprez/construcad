(ns construcad.app.mini-view
  "A small shaded three.js view for the template editors' previews: left drag
  orbits, right or middle drag pans, the wheel zooms. Z up, orthographic, as
  the main viewport."
  (:require ["three" :as THREE]
            ["three/addons/controls/OrbitControls.js" :refer [OrbitControls]]
            [construcad.app.reader :as reader]
            [goog.object :as gobj]))

(defn- size [^js el] [(max 1 (.-clientWidth el)) (max 1 (.-clientHeight el))])

(defn- set-frustum! [view half-height]
  (let [{:keys [^js camera el]} @view
        [w h] (size el) a (/ w h)]
    (set! (.-top camera) half-height)
    (set! (.-bottom camera) (- half-height))
    (set! (.-left camera) (- (* half-height a)))
    (set! (.-right camera) (* half-height a))
    (.updateProjectionMatrix camera)))

(defn look!
  "Look along `dir` (eye side) at everything shown, fitted to the view."
  [view [dx dy dz :as dir]]
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
    (set-frustum! view (* 0.55 span))
    (.copy (.-target controls) centre)
    (.lookAt camera centre)
    (.update controls)
    (swap! view assoc :dir dir)))

(defn show!
  "Replace what the view shows with these instructions ({id instr}). Fits
  the view the first time, or when `fit?`."
  [view instructions & {:keys [fit?]}]
  (let [{:keys [^js root shown?]} @view]
    (doseq [^js o (vec (array-seq (.-children root)))] (reader/dispose! o))
    (doseq [[id instr] instructions]
      (let [^js o (reader/assembly-object id instr)]
        (.traverse o (fn [^js n]
                       (when (= "hidden-edge" (gobj/get (.-userData n) "renderRole"))
                         (set! (.-visible n) false))))
        (.add root o)))
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
    (.append el (.-domElement renderer))
    (let [resize! (fn []
                    (let [[w h] (size el)]
                      (.setSize renderer w h false)
                      (set-frustum! view (.-top camera))))
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
