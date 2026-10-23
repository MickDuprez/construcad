(ns construcad.app.main
  "The grid and XLine editor. The browser holds the instructions it was sent
  and the resolved XLine ends (for snapping); every change goes to the server
  as a source edit (construcad.edits) and comes back as an upsert patch,
  which every open viewer of the building also receives."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [construcad.app.dom :refer [h]]
            [construcad.app.grid-view :as grid-view]
            [construcad.app.reader :as instr-reader]
            [construcad.app.templates :as templates]
            [construcad.app.viewport :as vp]
            [construcad.snap :as snap]
            [construcad.template-edit :as te]))

(defonce state*
  (atom {:buildings [] :building-id nil
         :view nil          ; {:grid :levels :resolved :xlines :errors :building}
         :objects {}        ; assembly id -> three.js root
         :filter-level nil  ; level id shown, nil = every level
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

(defn- level-shown? [level-id]
  (let [f (:filter-level @state*)] (or (nil? f) (= f level-id))))

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
     (update-overlays!)
     (render-panel!))))

(defn- draw-grid! []
  (grid-view/draw! (get-in @state* [:view :grid]) (:level/elevation (active-level) 0)))

(defn apply-patch!
  "Upsert the affected assemblies this viewer shows (and new ones on its
  level), and take the grid and levels if the edit changed them."
  [{:keys [upsert resolved xlines created grid levels errors] :as patch}]
  (let [{:keys [objects]} @state*
        shown? #(or (contains? objects %)
                    (and (some #{%} created) (level-shown? (get-in xlines [% :assembly/level]))))
        ids (filter shown? (keys upsert))]
    (doseq [id ids] (show-assembly! id (upsert id)))
    (swap! state* update :view
           (fn [v] (cond-> (-> v
                               (update :resolved merge (select-keys resolved ids))
                               (update :xlines merge (select-keys xlines ids))
                               (assoc :errors errors))
                     grid (assoc :grid grid)
                     levels (assoc :levels levels))))
    (when-let [t (:templates patch)] (templates/templates-changed! t))
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
      (.then (fn [[status {:keys [templates sections]}]]
               (when (= 200 status)
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

(defn load-view! [& {:keys [fit?]}]
  (let [{:keys [building-id filter-level]} @state*]
    (-> (fetch-edn (str "/api/buildings/" building-id "/view"
                        (when filter-level (str "?level=" filter-level))) {})
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
                 (vp/refresh-display!)
                 (draw-grid!)
                 (doseq [id (:selection @state*)] (highlight! id true))
                 (when fit? (vp/look! [1 -1 1]))
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

;; --- side panel ------------------------------------------------------------------

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
  (let [{:keys [view active-level filter-level]} @state*]
    (section "Levels"
             (h :label {:class "row"} "Show "
                (h :select {:on-change (fn [e]
                                         (let [v (.. e -target -value)]
                                           (swap! state* assoc :filter-level
                                                  (some #(when (= v (str (:xt/id %))) (:xt/id %)) (:levels view)))
                                           (load-view!)))}
                   (h :option {:value "" :selected (nil? filter-level)} "All levels")
                   (for [l (:levels view)]
                     (h :option {:value (str (:xt/id l)) :selected (= filter-level (:xt/id l))} (:level/name l)))))
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
                                                           :level/id (:xt/id l) :elevation %})))))))))

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

(defn- selection-section []
  (let [{:keys [selected view]} @state*
        x (get-in view [:xlines selected])]
    (section "Selection"
             (if-not x
               (h :p {:class "muted"} "Nothing selected.")
               (list
                (h :table {}
                   (h :tr {} (h :th {} "Mark") (h :td {} (:assembly/mark x)))
                   (h :tr {} (h :th {} "Role") (h :td {} (name (:xline/role x :xline))))
                   (h :tr {} (h :th {} "Size") (h :td {} (let [sz (:assembly/size x)] (if (string? sz) sz "—"))))
                   (h :tr {} (h :th {} "Template")
                      (h :td {} (cond
                                  (:assembly/template x) (or (templates/template-name (:assembly/template x)) "(unknown)")
                                  (:assembly/from-template x) (str "edited, from "
                                                                   (or (templates/template-name (:assembly/from-template x)) "(unknown)"))
                                  :else "—"))))
                (h :div {:class "buttons"}
                   (h :button {:on-click #(edit-assembly! selected) :title "Or right-click the member"} "Edit…")))))))

(defn- apply-section
  "Apply an assembly template at a size to every selected member."
  []
  (let [{:keys [selection selected view apply]} @state*
        xs (keep #(get-in view [:xlines %]) selection)
        role (:xline/role (get-in view [:xlines selected]))
        candidates (sort-by #(if (= role (:template/role %)) 0 1) (templates/templates-of :assembly))
        current (let [ts (set (map :assembly/template xs))] (when (= 1 (count ts)) (first ts)))
        tid (or (:template-id apply) current (:template/id (first candidates)))
        sizes (te/sizes-for-role (:sections @templates/env*)
                                 (get-in @templates/env* [:templates tid :template/role] role))
        size (or (:size apply) (some :assembly/size xs) (first sizes))]
    (when (seq xs)
      (section "Assembly template"
               (h :p {:class "muted"} (str (count xs) " selected; Shift + click adds or removes members"))
               (h :label {:class "row"} "Template "
                  (h :select {:on-change (fn [e] (let [v (.. e -target -value)]
                                                   (swap! state* assoc-in [:apply :template-id]
                                                          (some #(when (= v (str (:template/id %))) (:template/id %)) candidates))
                                                   (render-panel!)))}
                     (for [t candidates]
                       (h :option {:value (str (:template/id t)) :selected (= tid (:template/id t))}
                          (:template/name t)))))
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

(defn render-panel! []
  (when-let [panel (:panel @ui*)]
    (let [{:keys [tool view]} @state*
          tool-button (fn [t label] (h :button {:class (when (= tool t) "on") :on-click #(set-tool! t)} label))]
      (.apply (.-replaceChildren panel) panel
       (into-array
        (remove nil?
         [(h :header {} (h :h1 {} "ConstruCAD")
          (h :div {:class "muted"} (get-in view [:building :building/name] "")))
       (section "Tools"
                (h :div {:class "buttons"}
                   (tool-button :select "Select")
                   (tool-button :xline "XLine")
                   (h :button {:on-click vp/zoom-extents!} "Zoom extents"))
                (h :label {:class "row" :title "The role new XLines get. A beam drawn at one grid intersection is a column."}
                   "New XLines are "
                   (h :select {:on-change #(swap! state* assoc :xline-role (keyword (.. % -target -value)))}
                      (for [[r label] te/role-names]
                        (h :option {:value (name r) :selected (= r (:xline-role @state*))} label))))
                (h :label {:class "row"} "Display "
                   (h :select {:on-change #(vp/set-display! (keyword (.. % -target -value)))}
                      (for [[k label] [[:hidden-line "Hidden line"] [:shaded "Shaded"] [:wireframe "Wireframe"]]]
                        (h :option {:value (name k) :selected (= k (:display @vp/vp*))} label))))
                (h :label {:class "row"}
                   (h :input {:type "checkbox" :checked (:show-xlines? @vp/vp*)
                              :on-change #(vp/show-xlines! (.. % -target -checked))})
                   " Show XLines"))
       (selection-section)
       (apply-section)
       (templates/library-section)
       (levels-section)
       (grid-section)
       (messages-section)]))))))

;; --- start-up --------------------------------------------------------------------

(defn- build-shell! []
  (let [app (.getElementById js/document "app")
        panel (h :aside {:class "panel"})
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
    (.replaceChildren app panel viewport)
    (.append viewport command-bar)
    (swap! ui* assoc :panel panel :viewport viewport :prompt prompt :input input :status status)
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
    (-> (fetch-edn "/api/buildings" {})
        (.then (fn [[_ buildings]]
                 (let [id (:xt/id (first buildings))]
                   (swap! state* assoc :buildings buildings :building-id id)
                   (connect-socket! id)
                   (load-templates!)
                   (load-view! :fit? true)))))))

(defn reload! []
  (render-panel!)
  (update-overlays!))
