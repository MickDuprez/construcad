(ns construcad.app.main
  "The grid and XLine editor. The browser holds the instructions it was sent
  and the resolved XLine ends (for snapping); every change goes to the server
  as a source edit (construcad.edits) and comes back as an upsert patch,
  which every open viewer of the building also receives."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [construcad.app.dom :as dom :refer [h]]
            [construcad.app.grid-view :as grid-view]
            [construcad.app.reader :as instr-reader]
            [construcad.app.templates :as templates]
            [construcad.app.viewport :as vp]
            [construcad.navigator :as nav]
            [construcad.snap :as snap]
            [construcad.template-edit :as te]))

(defonce state*
  (atom {:projects [] :project-id nil :building-id nil
         :view nil          ; {:grid :levels :resolved :xlines :instructions :errors :building}
         :objects {}        ; assembly id -> three.js root
         :filter-level nil  ; level id the tree and viewport show, nil = every level
         :tree-query ""     ; navigator search text
         :collapsed #{}     ; navigator branches closed: [level-id] and [level-id role]
         :pages {}          ; assembly id -> its page data (properties: ends, supports)
         :props-open? true
         :popover nil       ; :levels, :templates or :settings, opened from the toolbar
         :active-level nil  ; level id the grid is drawn at and snaps to
         :tool :select
         :selected nil      ; the assembly the grips and properties are for
         :selection #{}     ; every selected assembly (Shift + click adds)
         :apply {}          ; {:template-id :size} chosen in the panel
         :xline-role :beam  ; role the XLine tool gives new members
         :xline-start nil   ; first pick of the XLine tool
         :drag nil          ; grip being dragged
         :command nil       ; grip waiting for typed coordinates
         :snap nil
         :status ""}))

(defonce ui* (atom {}))

(def snap-radius 14)

(declare render-panel! load-view! update-overlays! prompt!)

(defn set-status! [text]
  (swap! state* assoc :status text)
  (when-let [el (:status @ui*)] (set! (.-textContent el) text)))

;; --- server ----------------------------------------------------------------------

(defn- fetch-edn [url opts]
  (-> (js/fetch url (clj->js opts))
      (.then (fn [r] (.then (.text r) (fn [t] [(.-status r) (reader/read-string t)]))))))

(defn level [id] (some #(when (= id (:xt/id %)) %) (get-in @state* [:view :levels])))
(defn active-level [] (level (:active-level @state*)))

(defn- apply-level-filter!
  "Show only the assemblies on the filtered level (nil: all of them).
  Hidden ones can't be picked."
  []
  (let [{:keys [view objects filter-level]} @state*]
    (doseq [[id ^js o] objects]
      (set! (.-visible o) (or (nil? filter-level)
                              (= filter-level (nav/assembly-level (:levels view) (get-in view [:resolved id]))))))))

(defn- show-assembly! [id instr]
  (when-let [old (get-in @state* [:objects id])] (instr-reader/dispose! old))
  (let [o (instr-reader/assembly-object id instr)]
    (.add (:model-root @vp/vp*) o)
    (swap! state* assoc-in [:objects id] o)))

(defn- highlight! [id on?]
  (when-let [^js o (get-in @state* [:objects id])]
    (.traverse o (fn [^js n]
                   (case (some-> n .-userData .-renderRole)
                     "face" (.setHex (.. n -material -emissive) (if on? 0x0a5a73 0))
                     "visible-edge" (.setHex (.. n -material -color) (if on? 0x007a99 (:edge instr-reader/colours)))
                     nil)))))

(defn select!
  "Select one assembly (nil clears), or with `add?` toggle it in the selection."
  ([id] (select! id false))
  ([id add?]
   (let [{old :selection} @state*
         selection (cond
                     (and add? id) (if (contains? old id) (disj old id) (conj old id))
                     add? old
                     id #{id}
                     :else #{})]
     (doseq [o old] (highlight! o false))
     (doseq [o selection] (highlight! o true))
     (swap! state* assoc :selection selection :command nil
            :apply (if add? (:apply @state*) {})
            :selected (if (contains? selection id) id (first selection)))
     ;; open the tree down to the selected assembly
     (when-let [sel (:selected @state*)]
       (swap! state* update :collapsed #(apply disj % (nav/tree-path (:view @state*) sel))))
     (update-overlays!)
     (render-panel!)
     (some-> (:tree @ui*) (.querySelector ".tree-item.selected") (.scrollIntoView #js {:block "nearest"})))))

(defn- draw-grid! []
  (grid-view/draw! (get-in @state* [:view :grid]) (:level/elevation (active-level) 0)))

(defn apply-patch!
  "Upsert the affected assemblies this viewer shows (and new ones on its
  level), and take the grid and levels if the edit changed them."
  [{:keys [upsert resolved xlines created grid levels errors] :as patch}]
  (let [{:keys [objects]} @state*
        shown? #(or (contains? objects %) (some #{%} created))
        ids (filter shown? (keys upsert))]
    (doseq [id ids] (show-assembly! id (upsert id)))
    (swap! state* update :view
           (fn [v] (cond-> (-> v
                               (update :resolved merge (select-keys resolved ids))
                               (update :xlines merge (select-keys xlines ids))
                               (update :instructions merge (select-keys upsert ids))
                               (assoc :errors errors))
                     grid (assoc :grid grid)
                     levels (assoc :levels levels))))
    ;; page data (connections, supports) of a changed assembly is stale
    (swap! state* update :pages #(if (:templates patch) {} (apply dissoc % ids)))
    (apply-level-filter!)
    (when-let [t (:templates patch)] (templates/templates-changed! t))
    (when-let [s (:settings patch)] (templates/settings-changed! s))
    ;; snap points may have moved with the patch
    (swap! state* assoc :snap nil)
    (doseq [id (:selection @state*)] (highlight! id true))
    (vp/refresh-display!)
    (when (or grid levels) (draw-grid!))
    (update-overlays!)
    (render-panel!)))

(declare mark)

(defn- warning-text [{:keys [warning/type xline/id end] :as w}]
  (case type
    :template/end-not-on-xline (str (mark id) " " (name end) " is not on another XLine, so it has no connection")
    (pr-str w)))

(defn send-edit!
  "Send one source edit; the patch comes back and is applied. Returns a
  promise of true when the server took the edit."
  [edit]
  (set-status! "Saving…")
  (-> (fetch-edn (str "/api/buildings/" (:building-id @state*) "/edits")
                 {:method "POST" :headers {"content-type" "application/edn"}
                  :body (pr-str {:edit edit})})
      (.then (fn [[status body]]
               (if (= 200 status)
                 (let [p (:patch body)]
                   (apply-patch! p)
                   (set-status! (str "Saved; " (count (:upsert p)) " assemblies regenerated"
                                     (when-let [ws (seq (:warnings p))]
                                       (str ". Warning: " (str/join "; " (map warning-text ws))))))
                   true)
                 (do (set-status! (str "Refused: " (pr-str (:error body))))
                     false))))
      (.catch #(do (set-status! (str "Edit failed: " %)) false))))

(defn load-assembly!
  "A promise of one assembly's page data (construcad.edits/assembly-page), or nil."
  [id]
  (-> (fetch-edn (str "/api/buildings/" (:building-id @state*) "/assemblies/" id) {})
      (.then (fn [[status page]]
               (if (= 200 status)
                 page
                 (do (set-status! (str "Could not load the assembly: " (pr-str (:error page)))) nil))))))

(defn load-templates! []
  (-> (fetch-edn (str "/api/buildings/" (:building-id @state*) "/templates") {})
      (.then (fn [[status {:keys [templates sections settings]}]]
               (when (= 200 status)
                 (when settings (templates/settings-changed! settings))
                 (swap! templates/env* assoc :sections sections
                        :send! send-edit!
                        :load-assembly! load-assembly!)
                 (templates/templates-changed! templates)
                 (render-panel!))))))

(defn- connect-socket! [building-id]
  (when-let [old (:socket @ui*)] (set! (.-onclose old) nil) (.close old))
  (let [proto (if (= "https:" (.. js/location -protocol)) "wss" "ws")
        ws (js/WebSocket. (str proto "://" (.. js/location -host) "/api/buildings/" building-id "/ws"))]
    (set! (.-onmessage ws) (fn [e] (some-> (reader/read-string (.-data e)) :patch apply-patch!)))
    (set! (.-onclose ws) (fn [_] (js/setTimeout #(when (= building-id (:building-id @state*))
                                                   (connect-socket! building-id))
                                                2000)))
    (swap! ui* assoc :socket ws)))

(defn load-view!
  "Load every assembly of the building; the level filter only hides some."
  [& {:keys [fit?]}]
  (let [{:keys [building-id]} @state*]
    (-> (fetch-edn (str "/api/buildings/" building-id "/view") {})
        (.then (fn [[_ v]]
                 (doseq [[_ o] (:objects @state*)] (instr-reader/dispose! o))
                 (swap! state* (fn [st]
                                 (let [kept (set (filter (:xlines v) (:selection st)))]
                                   (assoc st :objects {} :view v :drag nil :command nil :xline-start nil
                                          :selection kept
                                          :selected (when (kept (:selected st)) (:selected st))))))
                 (when-not (level (:active-level @state*))
                   (swap! state* assoc :active-level (:xt/id (last (:levels v)))))
                 (doseq [[id instr] (:instructions v)] (show-assembly! id instr))
                 (apply-level-filter!)
                 (vp/refresh-display!)
                 (draw-grid!)
                 (doseq [id (:selection @state*)] (highlight! id true))
                 (when fit? (vp/look! [1 -1 1]))
                 (swap! state* assoc :pages {})
                 (update-overlays!)
                 (render-panel!)
                 (set-status! (str (count (:instructions v)) " assemblies loaded")))))))

;; --- snapping --------------------------------------------------------------------

(defn- snap-context [exclude level]
  (let [{:keys [resolved xlines grid]} (:view @state*)]
    {:grid grid :level level :resolved resolved :xlines xlines :exclude exclude}))

(defn- snap-at
  "Snap under the cursor, else a free point on the horizontal plane at z."
  [cursor exclude level z]
  (or (snap/pick (snap-context exclude level) vp/project cursor snap-radius)
      (some-> (vp/on-plane cursor z) snap/free)))

(defn- grip-level
  "Grid snaps for a dragged end use the level the end sits on, if it sits on
  one, so a column top drags across the grid at its own level."
  [[_ _ z]]
  (or (some #(when (< (js/Math.abs (- z (:level/elevation %))) 1) %) (get-in @state* [:view :levels]))
      (active-level)))

;; --- overlays: grips, snap marker, rubber band, command prompt ----------------------

(defn- ensure-overlays! []
  (when-not (:grips @ui*)
    (swap! ui* assoc
           :grips {:start (vp/marker 0xffb000) :end (vp/marker 0xffb000)}
           :snap-marker (doto (vp/marker 0x00c2e0) (-> .-userData .-pixels (set! 14)))
           :band (vp/rubber-band))))

(defn- resolved-end [id end] (get-in @state* [:view :resolved id end]))

(defn update-overlays! []
  (ensure-overlays!)
  (let [{:keys [selected drag snap xline-start tool]} @state*
        {:keys [grips ^js snap-marker band]} @ui*]
    (doseq [[end ^js g] grips]
      (if-let [p (and selected (= :select tool) (resolved-end selected end))]
        (do (.copy (.-position g) (vp/v3 (if (and drag (= end (:end drag)) snap) (:point snap) p)))
            (set! (.-visible g) true))
        (set! (.-visible g) false)))
    (set! (.-visible snap-marker) (boolean (and snap (not= :free (:kind snap)))))
    (when snap (.copy (.-position snap-marker) (vp/v3 (:point snap))))
    (cond
      (and drag snap) (vp/set-line! band (resolved-end (:id drag) (if (= :start (:end drag)) :end :start)) (:point snap))
      (and xline-start snap) (vp/set-line! band (:point xline-start) (:point snap))
      :else (set! (.-visible band) false))
    (prompt!)))

(defn- mark [id] (get-in @state* [:view :xlines id :assembly/mark] "XLine"))

(defn prompt! []
  (let [{:keys [tool xline-start command snap]} @state*
        text (cond
               command (str "MOVE " (mark (:id command)) " " (str/upper-case (name (:end command)))
                            ": type @dx,dy,dz or x,y,z")
               (and (= :xline tool) xline-start) (str "XLINE end: click, or type x,y,z or @dx,dy,dz"
                                                      "  (start " (:label xline-start) ")")
               (= :xline tool) "XLINE start: click a grid point, an XLine end or along an XLine, or type x,y,z"
               :else "Select: click a member; drag a yellow grip to move an end, or click it to type @dx,dy,dz")]
    (when-let [el (:prompt @ui*)]
      (set! (.-textContent el) (str text (when snap (str "   ·   snap: " (name (:kind snap)) " " (:label snap))))))))

;; --- tools -----------------------------------------------------------------------

(defn set-tool! [tool]
  (swap! state* assoc :tool tool :xline-start nil :command nil :drag nil :snap nil)
  (update-overlays!)
  (render-panel!)
  (some-> (:input @ui*) .focus))

(defn cancel! []
  (swap! state* assoc :xline-start nil :command nil :drag nil :snap nil)
  (update-overlays!))

(defn- create-xline! [start end]
  ;; Always the role picked for the tool, whichever way the XLine runs.
  (send-edit! {:edit/type :xline/create :start start :end end :role (:xline-role @state*)}))

(defn- grip-under [cursor]
  (let [{:keys [selected]} @state*]
    (when selected
      (some (fn [end]
              (when-let [p (resolved-end selected end)]
                (let [[sx sy] (vp/project p) [cx cy] cursor]
                  (when (< (+ (* (- sx cx) (- sx cx)) (* (- sy cy) (- sy cy))) 100) end))))
            [:end :start]))))

(defn- drag-snap [{:keys [id end]} cursor]
  (let [p (resolved-end id end)]
    (snap-at cursor (snap/excluded (get-in @state* [:view :xlines]) id) (grip-level p) (nth p 2))))

(defn- hide-menu! []
  (when-let [el (:menu @ui*)] (.remove el) (swap! ui* dissoc :menu)))

(declare edit-assembly!)

(defn- show-menu!
  "The viewport's context menu for the selected member, at the cursor."
  [^js e id]
  (hide-menu!)
  (let [el (h :div {:class "context-menu"
                    :style (str "left:" (.-clientX e) "px;top:" (.-clientY e) "px")}
              (h :div {:class "muted small"} (mark id))
              (h :button {:on-click #(do (hide-menu!) (edit-assembly! id))} "Edit assembly"))]
    (.append (.-body js/document) el)
    (swap! ui* assoc :menu el)))

(defn on-pointer-down [^js e]
  (when (= 2 (.-button e))
    (swap! ui* assoc :right-down (vp/pointer e)))
  (when (= 0 (.-button e))
    (let [cursor (vp/pointer e)
          {:keys [tool xline-start]} @state*]
      (case tool
        :xline (when-let [s (snap-at cursor #{} (active-level) (:level/elevation (active-level) 0))]
                 (if xline-start
                   (do (create-xline! (:ref xline-start) (:ref s))
                       (swap! state* assoc :xline-start nil))
                   (swap! state* assoc :xline-start s))
                 (update-overlays!))
        :select (if-let [end (grip-under cursor)]
                  (do (.setPointerCapture (.-target e) (.-pointerId e))
                      (swap! state* assoc :command nil
                             :drag {:id (:selected @state*) :end end :from cursor :moved? false}))
                  (let [s (vp/pick cursor)]
                    (select! (some #(when (= (str %) s) %) (keys (:objects @state*))) (.-shiftKey e))))))
    (some-> (:input @ui*) .focus)))

(defn on-pointer-move [^js e]
  (let [cursor (vp/pointer e)
        {:keys [tool drag]} @state*]
    (cond
      drag (let [[fx fy] (:from drag) [cx cy] cursor
                 moved? (or (:moved? drag) (> (+ (js/Math.abs (- cx fx)) (js/Math.abs (- cy fy))) 3))]
             (swap! state* assoc :drag (assoc drag :moved? moved?)
                    :snap (when moved? (drag-snap drag cursor)))
             (update-overlays!))
      (= :xline tool) (do (swap! state* assoc :snap (snap-at cursor #{} (active-level) (:level/elevation (active-level) 0)))
                          (update-overlays!)))))

(defn- right-click!
  "A right click without a drag (a right drag pans) on a member selects it,
  unless it is already selected, and opens the context menu."
  [^js e]
  (let [[fx fy] (:right-down @ui*) [cx cy :as cursor] (vp/pointer e)]
    (swap! ui* dissoc :right-down)
    (when (and fx (< (+ (js/Math.abs (- cx fx)) (js/Math.abs (- cy fy))) 4) (= :select (:tool @state*)))
      (let [s (vp/pick cursor)]
        (when-let [id (some #(when (= (str %) s) %) (keys (:objects @state*)))]
          (when-not (contains? (:selection @state*) id) (select! id))
          (show-menu! e id))))))

(defn on-pointer-up [^js e]
  (when (and (= 2 (.-button e)) (:right-down @ui*)) (right-click! e))
  (when-let [{:keys [id end moved?]} (:drag @state*)]
    (let [s (:snap @state*)]
      (swap! state* assoc :drag nil :snap nil)
      (if (and moved? s)
        (send-edit! {:edit/type :xline/set-node :xline/id id :xline/end end :node (:ref s)})
        (swap! state* assoc :command {:id id :end end}))
      (update-overlays!)
      (some-> (:input @ui*) .focus))))

(defn- typed! [text]
  (let [{:keys [relative? values error]} (snap/parse-entry text)
        {:keys [tool xline-start command view]} @state*]
    (cond
      error (set-status! error)

      command
      (let [{:keys [id end]} command
            k (if (= :start end) :xline/start :xline/end)
            ref (get-in view [:xlines id k])
            point (resolved-end id end)
            node (if relative?
                   (snap/moved-ref (:grid view) ref point values)
                   {:node/at (mapv js/Math.round values)})]
        (swap! state* assoc :command nil)
        (send-edit! {:edit/type :xline/set-node :xline/id id :xline/end end :node node}))

      (and (= :xline tool) xline-start)
      (do (create-xline! (:ref xline-start)
                         (if relative? {:node/rel (mapv js/Math.round values)} {:node/at (mapv js/Math.round values)}))
          (swap! state* assoc :xline-start nil))

      (= :xline tool)
      (if relative?
        (set-status! "The start point needs absolute x,y,z")
        (swap! state* assoc :xline-start (snap/free values)))

      :else (set-status! "Pick a grip or start the XLine tool first"))
    (update-overlays!)))

(defn on-key-down [^js e]
  (when (= "Escape" (.-key e))
    (hide-menu!)
    (cancel!)
    (set-status! "Cancelled")))


;; --- panels: toolbar, navigator, properties, popovers -------------------------------

(defn- number-input [value on-commit & [attrs]]
  (h :input (merge {:type "number" :step "any" :value value :class "num"
                    :on-change (fn [e] (when-let [v (parse-double (.. e -target -value))] (on-commit v)))}
                   attrs)))

(defn- section [title & body] (h :section {} (h :h2 {} title) body))

(defn- ref-text [ref]
  (let [{:keys [view]} @state*
        g (:grid view)
        label-of (fn [axis-id] (some #(when (= axis-id (:id %)) (:label %))
                                     (concat (:grid/x-axes g) (:grid/y-axes g))))]
    (cond
      (:node/grid ref) (str "grid " (str/join "/" (map label-of (:node/grid ref)))
                            (when-let [l (:node/level ref)] (str " @ " l))
                            (when-let [o (:node/offset ref)] (str " + " (pr-str o))))
      (:node/of ref) (str (mark (:node/of ref)) " " (name (:node/end ref)))
      (:node/on ref) (str "on " (mark (:node/on ref)) " at " (:node/station ref)
                          " from " (name (:node/from ref :start)))
      (:node/rel ref) (str "start + " (pr-str (:node/rel ref)))
      (:node/at ref) (str "at " (pr-str (:node/at ref)))
      :else (pr-str ref))))

(defn- levels-section []
  (let [{:keys [view active-level]} @state*]
    (section "Levels"
             (h :table {}
                (h :tr {} (h :th {} "Work") (h :th {} "Name") (h :th {} "Elevation"))
                (for [l (reverse (:levels view))]
                  (h :tr {}
                     (h :td {} (h :input {:type "radio" :name "active-level" :checked (= active-level (:xt/id l))
                                          :on-change (fn [_] (swap! state* assoc :active-level (:xt/id l))
                                                       (draw-grid!) (update-overlays!))}))
                     (h :td {} (:level/name l))
                     (h :td {} (number-input (:level/elevation l)
                                             #(send-edit! {:edit/type :level/set-elevation
                                                           :level/id (:xt/id l) :elevation %}))))))
             (h :p {:class "muted small"} "The grid is drawn, and grid snaps sit, at the working level."))))

(defn- grid-section []
  (let [g (get-in @state* [:view :grid])
        style-select (fn [k current]
                       (h :select {:on-change (fn [e] (send-edit! {:edit/type :grid/relabel
                                                                   k (keyword (.. e -target -value))}))}
                          (for [s [:numbers :letters]]
                            (h :option {:value (name s) :selected (= s current)} (name s)))))
        axes (fn [title k]
               (list (h :tr {} (h :th {:colspan 2} title))
                     (for [{:keys [id label offset]} (get g k)]
                       (h :tr {}
                          (h :td {} label)
                          (h :td {} (number-input offset #(send-edit! {:edit/type :grid/set-axis-offset
                                                                       :axis/id id :offset %})))))))]
    (when g
      (section "Grid"
               (h :label {:class "row"} "x-axes labelled " (style-select :x-style (:grid/x-style g)))
               (h :label {:class "row"} "y-axes labelled " (style-select :y-style (:grid/y-style g)))
               (h :table {}
                  (axes "x-axes (offset along x)" :grid/x-axes)
                  (axes "y-axes (offset along y)" :grid/y-axes))))))

(defn- settings-section
  "The project's detailing settings: what a part's blank field takes
  (gauge, pitch, bolt and hole size, thicknesses...). A change regenerates
  the assemblies that inherit it; a blank box is the default."
  []
  (let [{:keys [stored fields]} (:settings-view @templates/env*)]
    (section "Detailing settings"
             (h :table {:class "settings"}
                (for [[k default label help] fields]
                  (h :tr {}
                     (h :th {:title help} label)
                     (h :td {} (h :input {:type "number" :step "any" :min 0 :class "num" :data-k (str "set-" (name k))
                                          :value (if (contains? stored k) (get stored k) "")
                                          :placeholder (str default)
                                          :on-change (fn [e]
                                                       (let [t (str/trim (.. e -target -value))
                                                             v (when-not (str/blank? t) (parse-double t))]
                                                         (when (or v (str/blank? t))
                                                           (send-edit! {:edit/type :settings/update :settings {k v}}))))}))
                     (h :td {:class "muted small"} help))))
             (h :p {:class "muted small"}
                "For this project. A part's blank field takes these (shown greyed in the connection editor, \"90 from settings\"); "
                "a value typed on the part wins. Blank here is the default shown. Changing one regenerates the members that inherit it."))))

(defn edit-assembly!
  "Open the assembly page for one assembly, in instance mode."
  [id]
  (let [{:keys [view]} @state*
        x (get-in view [:xlines id])]
    (when x
      (templates/open-assembly! id {:start (ref-text (:xline/start x))
                                    :end (ref-text (:xline/end x))
                                    :length (get-in view [:resolved id :length] 0)
                                    :rotation (:xline/rotation x 0)}))))

(defn- apply-section
  "Apply an assembly template at a size to every selected member."
  []
  (let [{:keys [selection selected view apply]} @state*
        xs (keep #(get-in view [:xlines %]) selection)
        role (:xline/role (get-in view [:xlines selected]))
        candidates (sort-by #(if (= role (:template/role %)) 0 1) (templates/templates-of :assembly))
        current (let [ts (set (map #(or (get-in % [:assembly/from-template :template/id]) (:assembly/template %)) xs))]
                  (when (= 1 (count ts)) (first ts)))
        tid (or (:template-id apply) current (:template/id (first candidates)))
        sizes (te/sizes-for-role (:sections @templates/env*)
                                 (get-in @templates/env* [:templates tid :template/role] role))
        size (or (:size apply)
                 (some #(let [s (or (:assembly/size %) (:assembly/profile %))] (when (string? s) s)) xs)
                 (first sizes))]
    (when (seq xs)
      (section "Apply a template"
               (h :p {:class "muted small"} (str (count xs) " selected; Shift + click adds or removes members"))
               (h :label {:class "row"} "Template "
                  (h :select {:on-change (fn [e] (let [v (.. e -target -value)]
                                                   (swap! state* assoc-in [:apply :template-id]
                                                          (some #(when (= v (str (:template/id %))) (:template/id %)) candidates))
                                                   (render-panel!)))}
                     (for [t candidates]
                       (h :option {:value (str (:template/id t)) :selected (= tid (:template/id t))}
                          (:template/name t)))))
               (when-let [src (get-in @templates/env* [:templates tid :template/thumbnail])]
                 (h :img {:class "thumb" :src src :alt ""}))
               (h :label {:class "row"} "Size "
                  (h :select {:on-change (fn [e] (swap! state* assoc-in [:apply :size] (.. e -target -value)))}
                     (for [d sizes] (h :option {:value d :selected (= d size)} d))))
               (h :div {:class "buttons"}
                  (h :button {:class "on" :disabled (nil? tid)
                              :on-click #(send-edit! {:edit/type :assembly/apply-template
                                                      :xline/ids (vec (sort-by str selection))
                                                      :template/id tid :size size})}
                     "Apply"))))))

(defn- messages-section []
  (let [errors (get-in @state* [:view :errors])]
    (when (seq errors)
      (section "Regeneration messages"
               (h :ul {:class "errors"} (for [e errors] (h :li {} (pr-str e))))))))

;; toolbar

(defn- toggle-popover! [k]
  (swap! state* update :popover #(when-not (= % k) k))
  (render-panel!))

(defn- render-toolbar! []
  (when-let [el (:toolbar @ui*)]
    (let [{:keys [tool popover]} @state*
          tool-button (fn [t label title] (h :button {:class (when (= tool t) "on") :title title :on-click #(set-tool! t)} label))]
      (.replaceChildren
       el
       (h :strong {:class "brand"} "ConstruCAD")
       (h :div {:class "tool-group"}
          (tool-button :select "Select" "Click a member; drag a grip to move an end")
          (tool-button :xline "XLine" "Draw a new member: click its start and end")
          (h :label {:title "The role new XLines get"} "New XLines are "
             (h :select {:on-change #(swap! state* assoc :xline-role (keyword (.. % -target -value)))}
                (for [[r label] te/role-names]
                  (h :option {:value (name r) :selected (= r (:xline-role @state*))} label)))))
       (h :div {:class "tool-group"}
          (h :label {} "Display "
             (h :select {:on-change #(vp/set-display! (keyword (.. % -target -value)))}
                (for [[k label] [[:hidden-line "Hidden line"] [:shaded "Shaded"] [:wireframe "Wireframe"]]]
                  (h :option {:value (name k) :selected (= k (:display @vp/vp*))} label))))
          (h :label {}
             (h :input {:type "checkbox" :checked (:show-xlines? @vp/vp*)
                        :on-change #(vp/show-xlines! (.. % -target -checked))})
             " XLines")
          (h :button {:on-click vp/zoom-extents!} "Zoom extents"))
       (h :div {:class "tool-group"}
          (h :button {:class (when (= :levels popover) "on") :on-click #(toggle-popover! :levels)} "Levels & grid")
          (h :button {:class (when (= :templates popover) "on") :on-click #(toggle-popover! :templates)} "Templates")
          (h :button {:class (when (= :settings popover) "on") :title "The project's detailing settings: bolts, gauge, pitch, thicknesses"
                      :on-click #(toggle-popover! :settings)} "Settings"))))))

(defn- render-popover! []
  (when-let [el (:popover @ui*)]
    (let [k (:popover @state*)]
      (set! (.. el -style -display) (if k "block" "none"))
      (dom/replace-keeping-focus!
       el
       (when k
         [(h :div {:class "popover-head"}
             (h :strong {} (case k :levels "Levels & grid" :settings "Detailing settings" "Templates"))
             (h :button {:class "small" :title "Close" :on-click #(toggle-popover! k)} "✕"))
          (case k
            :levels (h :div {} (levels-section) (grid-section))
            :settings (settings-section)
            (templates/library-section))])))))

;; navigator

(declare render-tree! render-props!)

(defn- open-building! [id]
  (hide-menu!)
  (swap! state* assoc :building-id id :filter-level nil :pages {} :collapsed #{}
         :selection #{} :selected nil)
  (connect-socket! id)
  (load-templates!)
  (load-view! :fit? true))

(defn- picker [label value options on-pick]
  (h :label {:class "picker"}
     (h :span {:class "muted small"} label)
     (h :select {:data-k label :on-change #(on-pick (.. % -target -value))}
        (for [[v text] options] (h :option {:value v :selected (= v value)} text)))))

(defn- render-nav-head! []
  (when-let [el (:nav-head @ui*)]
    (let [{:keys [projects project-id building-id view filter-level tree-query]} @state*
          project (some #(when (= project-id (:xt/id %)) %) projects)]
      (dom/replace-keeping-focus!
       el
       [(picker "Project" (str project-id) (map (fn [p] [(str (:xt/id p)) (:project/name p)]) projects)
                (fn [v] (let [p (some #(when (= v (str (:xt/id %))) %) projects)]
                          (swap! state* assoc :project-id (:xt/id p))
                          (if-let [b (first (:buildings p))]
                            (open-building! (:xt/id b))
                            (render-panel!)))))
        (picker "Building" (str building-id)
                (if (seq (:buildings project))
                  (map (fn [b] [(str (:xt/id b)) (:building/name b)]) (:buildings project))
                  [["" "No buildings"]])
                (fn [v] (when-let [b (some #(when (= v (str (:xt/id %))) %) (:buildings project))]
                          (open-building! (:xt/id b)))))
        (picker "Level" (str filter-level)
                (cons ["" "All levels"] (map (fn [l] [(str (:xt/id l)) (:level/name l)])
                                             (reverse (:levels view))))
                (fn [v] (swap! state* assoc :filter-level
                               (some #(when (= v (str (:xt/id %))) (:xt/id %)) (:levels view)))
                  (apply-level-filter!)
                  (render-panel!)))
        (h :input {:type "search" :class "tree-search" :placeholder "Find a mark or size" :data-k "tree-search"
                   :value tree-query
                   :on-input (fn [e] (swap! state* assoc :tree-query (.. e -target -value))
                               (render-tree!))})]))))

(defn- branch [k label n children]
  (let [open? (not (contains? (:collapsed @state*) k))]
    (h :li {:class "tree-branch"}
       (h :div {:class "tree-label" :on-click #(do (swap! state* update :collapsed (if open? conj disj) k)
                                                   (render-tree!))}
          (h :span {:class "twisty"} (if open? "▾" "▸"))
          label (h :span {:class "count"} n))
       (when open? (h :ul {} children)))))

(defn- tree-item [{:keys [id mark size]}]
  (let [{:keys [selection selected]} @state*]
    (h :li {:class (str "tree-item" (when (contains? selection id) " in-selection") (when (= id selected) " selected"))
            :title "Double-click or right-click to edit the assembly"
            :on-click #(select! id (.-shiftKey %))
            :on-dblclick #(edit-assembly! id)
            :on-contextmenu (fn [^js e]
                              (.preventDefault e)
                              (when-not (contains? (:selection @state*) id) (select! id))
                              (show-menu! e id))}
       (h :span {:class "mark"} mark)
       (when size (h :span {:class "size"} size)))))

(defn render-tree! []
  (when-let [el (:tree @ui*)]
    (let [{:keys [view filter-level tree-query]} @state*
          t (nav/tree view :level-id filter-level :query tree-query)
          count-of (fn [groups] (reduce + (map (comp count :items) groups)))]
      (.replaceChildren
       el
       (if-not view
         (h :p {:class "muted"} "Loading…")
         (h :ul {:class "tree"}
            (h :li {:class "tree-root"}
               (h :div {:class "tree-label building"} (get-in view [:building :building/name] "Building"))
               (if (empty? t)
                 (h :p {:class "muted small"} (if (str/blank? tree-query) "No assemblies here." "Nothing matches."))
                 (h :ul {}
                    (for [{lid :level/id lname :level/name groups :groups} t]
                      (branch [lid] lname (count-of groups)
                              (for [{:keys [role label items]} groups]
                                (branch [lid role] label (count items) (map tree-item items))))))))))))))

;; properties

(defn- ensure-page!
  "Fetch the selected assembly's page data (its connections and supports)
  once; the properties re-render when it comes."
  [id]
  (when (and id (not (contains? (:pages @state*) id)))
    (swap! state* assoc-in [:pages id] :loading)
    (-> (load-assembly! id)
        (.then (fn [page]
                 (when (= :loading (get-in @state* [:pages id]))
                   (swap! state* assoc-in [:pages id] (or page :failed))
                   (render-props!)))))))

(defn- fmt-length [v] (if (number? v) (str (js/Math.round v) " mm") "—"))

(def part-labels {:main "Main member" :part "Part" :plate "End plate" :bolts "Bolts"})

(defn- end-row
  "One end in the properties: its own connection by name, where it came
  from, what it sits on, and a link to edit this assembly's copy."
  [end {:keys [connection-id support from] cname :name} page]
  (h :div {:class "end-row"}
     (h :div {} (h :strong {} (if (= :start end) "Start " "End "))
        (if connection-id (or cname "unnamed connection") (h :span {:class "muted"} "plain end")))
     (when from
       (h :div {:class "muted small"} (str "from library: " (:name from))))
     (h :div {:class "muted small"}
        (if support
          (str "on " (:assembly/mark support) (when-let [s (:size support)] (str ", " s)))
          "not on another XLine"))
     (when connection-id
       (h :a {:href "#" :on-click (fn [e] (.preventDefault e) (templates/open-connection-instance! page end))}
          (str "Edit " (name end) " connection…")))))

(defn- properties-body []
  (let [{:keys [selected selection view pages]} @state*
        page (get pages selected)
        p (when selected (nav/properties view selected (when (map? page) page) templates/template-name))]
    (ensure-page! selected)
    (if-not p
      (h :div {:class "empty"}
         (h :p {} "Nothing selected.")
         (h :p {:class "muted small"} "Pick an assembly in the tree or the model to see its properties."))
      (h :div {}
         (when (> (count selection) 1)
           (h :p {:class "muted small"} (str (count selection) " selected; showing " (:mark p) ".")))
         (h :table {:class "props-table"}
            (h :tr {} (h :th {} "Mark") (h :td {} (h :strong {} (:mark p))))
            (h :tr {} (h :th {} "Role") (h :td {} (:role-label p)))
            (h :tr {} (h :th {} "Size") (h :td {} (or (:size p) "—")))
            (h :tr {} (h :th {} "Level") (h :td {} (or (:level p) "—")))
            (h :tr {} (h :th {} "Length") (h :td {} (fmt-length (:length p))))
            (h :tr {} (h :th {} "Template")
               (h :td {} (let [{:keys [kind] tname :name} (:template p)]
                           (case kind
                             :from (list (h :span {:class "muted"} "from library: ") tname)
                             "—")))))
         (h :div {:class "buttons"}
            (h :button {:class "on" :on-click #(edit-assembly! selected) :title "Or double-click it in the tree"}
               "Edit assembly"))
         (h :h3 {} "Connections")
         (if (map? page)
           (list (end-row :start (get-in p [:ends :start]) page)
                 (end-row :end (get-in p [:ends :end]) page))
           (h :p {:class (if (= :failed page) "warn small" "muted small")}
              (if (= :failed page) "Could not load its connections." "Loading…")))
         (h :h3 {} "Parts")
         (h :table {:class "parts"}
            (for [{:keys [kind end size] n :count} (:parts p)]
              (h :tr {}
                 (h :td {} (part-labels kind) (when end (h :span {:class "muted"} (str " (" (name end) ")"))))
                 (h :td {} size)
                 (h :td {:class "tight"} (str "× " n)))))))))

(defn render-props! []
  (when-let [el (:props @ui*)]
    (let [open? (:props-open? @state*)
          toggle! #(do (swap! state* update :props-open? not) (render-props!))]
      (.toggle (.-classList (:app @ui*)) "props-collapsed" (not open?))
      (dom/replace-keeping-focus!
       el
       (if-not open?
         [(h :button {:class "props-tab" :title "Show the properties" :on-click toggle!} "‹ Properties")]
         [(h :div {:class "props-head"}
             (h :h2 {} "Properties")
             (h :button {:class "small" :title "Hide the properties" :on-click toggle!} "›"))
          (properties-body)
          (apply-section)
          (messages-section)])))))

(defn render-panel! []
  (render-toolbar!)
  (render-nav-head!)
  (render-tree!)
  (render-props!)
  (render-popover!))

;; --- start-up --------------------------------------------------------------------

(defn- build-shell! []
  (let [app (.getElementById js/document "app")
        toolbar (h :header {:class "toolbar"})
        nav-head (h :div {:class "nav-head"})
        tree (h :div {:class "tree-wrap"})
        nav (h :aside {:class "panel nav"} nav-head tree)
        props (h :aside {:class "panel props"})
        popover (h :div {:class "panel popover"})
        viewport (h :main {:class "viewport"})
        prompt (h :div {:class "prompt"})
        input (h :input {:type "text" :class "command-input" :placeholder "x,y,z or @dx,dy,dz then Enter"
                         :autocomplete "off" :spellcheck "false"
                         :on-keydown (fn [e]
                                       (when (= "Enter" (.-key e))
                                         (typed! (.. e -target -value))
                                         (set! (.. e -target -value) "")))})
        status (h :div {:class "status"})
        command-bar (h :div {:class "command-bar"} prompt input status)]
    (.replaceChildren app toolbar nav viewport props)
    (.append viewport popover command-bar)
    (swap! ui* assoc :app app :toolbar toolbar :nav-head nav-head :tree tree :props props :popover popover
           :viewport viewport :prompt prompt :input input :status status)
    viewport))

(defn init! []
  (let [viewport (build-shell!)
        canvas (vp/init! viewport {})]
    (.insertBefore viewport canvas (.-firstChild viewport))
    (.addEventListener canvas "pointerdown" on-pointer-down)
    (.addEventListener canvas "pointermove" on-pointer-move)
    (.addEventListener canvas "pointerup" on-pointer-up)
    ;; A release outside the canvas, or a lost capture, still ends a drag.
    (.addEventListener js/window "pointerup" on-pointer-up)
    (.addEventListener canvas "lostpointercapture" on-pointer-up)
    (.addEventListener js/document "keydown" on-key-down)
    ;; Any press outside the context menu closes it.
    (.addEventListener js/document "pointerdown"
                       (fn [e] (when-not (some-> ^js (:menu @ui*) (.contains (.-target e))) (hide-menu!)))
                       true)
    (render-panel!)
    (-> (fetch-edn "/api/projects" {})
        (.then (fn [[_ projects]]
                 (let [p (or (some #(when (seq (:buildings %)) %) projects) (first projects))]
                   (swap! state* assoc :projects projects :project-id (:xt/id p))
                   (if-let [b (first (:buildings p))]
                     (open-building! (:xt/id b))
                     (render-panel!))))))))

(defn reload! []
  (render-panel!)
  (update-overlays!))
