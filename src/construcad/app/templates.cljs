(ns construcad.app.templates
  "The template library panel and the editors, shown as a dialog over the
  editor:

  - the assembly page: the main member on its XLine, end-on with its nine
    insertion points, offset, rotation and extensions, a side elevation of
    the member alone, and a Start and an End card that pick a connection
    template (or none) with per-end overrides. It edits an assembly template,
    or (instance mode) one assembly in the model, read from the server with
    modeller.template/assembly-state, with its mark, role and size; Save
    sends :assembly/update and Apply template... re-applies a template;
  - the connection template editor: ordered actions, parameters as numbers
    or formulas, and a 3D preview of an owner framing into a support (a
    vertical column or a horizontal beam), sized from the library. This is
    where a connection is seen: a connection template knows nothing of member
    roles, only the two ECSs it joins and how.

  Previews regenerate in the browser with the real core and the in-browser
  section library (construcad.template-edit); only Save goes to the server.
  Editors stack: an end's connection template opens over the assembly page
  and returns to it."
  (:require [clojure.string :as str]
            [construcad.app.dom :as dom :refer [h s]]
            [construcad.app.mini-view :as mini-view]
            [construcad.template-edit :as te]
            [modeller.library :as library]
            [modeller.sections-au :as au]))

(defonce lib (delay (library/library {} au/rows)))

;; :templates {id t} and :sections [{:section/designation ..}] come from the
;; server (GET /templates, and patches). :send! sends one edit and returns a
;; promise of true when the server took it; :load-assembly! returns a promise
;; of an assembly's page data (construcad.edits/assembly-page), or nil.
(defonce env* (atom {:templates {} :sections [] :send! nil :load-assembly! nil}))

;; The open editors, innermost last: {:kind :draft :size :support-size :error ...}.
(defonce dlg* (atom {:stack []}))

(defonce ui* (atom {}))

(declare render!)

(defn- top [] (peek (:stack @dlg*)))
(defn- update-top! [f & args]
  (swap! dlg* update :stack #(conj (pop %) (apply f (peek %) args)))
  (render!))
(defn- update-draft! [f & args] (update-top! #(apply update % :draft f args)))

(defn templates-of [kind]
  (sort-by :template/name (filter #(= kind (:template/kind %)) (vals (:templates @env*)))))

(defn template-name [id] (get-in @env* [:templates id :template/name]))

(defn- designations []
  (let [from-server (map :section/designation (:sections @env*))]
    (if (seq from-server) from-server (sort (keys (:sections @lib))))))

(defn- sizes-for [role]
  (if (seq (:sections @env*))
    (te/sizes-for-role (:sections @env*) role)
    (designations)))

;; Where each kind keeps the main member and the two end conditions.
(defn- paths [kind]
  (if (= :instance kind)
    {:main :main :start :start :end :end}
    {:main :template/main :start :template/start :end :template/end}))

;; --- opening and closing ------------------------------------------------------------

(defn- default-sizes [kind draft]
  (if (and (= :assembly kind) (= :column (:template/role draft)))
    {:size "200UC46.2" :support-size "200UC46.2"}
    {:size "310UB40.4" :support-size "200UC46.2"}))

(defn open!
  "Open an editor for a template map (an existing one, a duplicate or a blank)."
  [draft]
  (let [kind (:template/kind draft)]
    (swap! dlg* update :stack conj (merge {:kind kind :draft draft :arrangement :column}
                                          (default-sizes kind draft)))
    (render!)))

(defn- instance-dlg [{:keys [state supports context]} info]
  {:kind :instance :draft state :supports supports :context context :info info
   :rotation (:rotation info) :rotation-was (:rotation info)})

(defn open-assembly!
  "Open the assembly page for one assembly in the model. `info` is what the
  editor already knows about its XLine: {:start :end (text) :length :rotation}."
  [id info]
  (-> ((:load-assembly! @env*) id)
      (.then (fn [page]
               (when page
                 (swap! dlg* update :stack conj (instance-dlg page info))
                 (render!))))))

(defn- close! []
  (swap! dlg* update :stack pop)
  (render!))

(defn- send-all!
  "Send edits one after another, stopping at the first the server refuses.
  A promise of true when every one was taken."
  [edits]
  (reduce (fn [p e] (.then p #(if % ((:send! @env*) e) false)))
          (js/Promise.resolve true)
          edits))

(defn- refused! [] (update-top! assoc :saving? false :error "The server refused the change; see the status bar."))

(defn- save! []
  (let [{:keys [kind draft] :as dlg} (top)
        problems (when-not (= :instance kind) (te/draft-errors draft))]
    (if (seq problems)
      (update-top! assoc :error (str/join " " problems))
      (do (update-top! assoc :error nil :saving? true)
          (-> (if (= :instance kind)
                (send-all! (te/save-edits draft dlg))
                ((:send! @env*) {:edit/type :template/save :template draft}))
              (.then #(if % (close!) (refused!))))))))

(defn- apply-template! []
  (let [{:keys [draft apply info]} (top)
        id (:xline/id draft)
        tid (:template-id apply)]
    (update-top! assoc :error nil :saving? true)
    (-> ((:send! @env*) {:edit/type :assembly/apply-template :xline/ids [id]
                         :template/id tid :size (:size apply (:assembly/size draft))})
        (.then (fn [ok?] (if ok? ((:load-assembly! @env*) id) (do (refused!) nil))))
        (.then (fn [page]
                 (when page
                   (swap! dlg* update :stack #(conj (pop %) (instance-dlg page info)))
                   (render!)))))))

;; --- shared bits ----------------------------------------------------------------------

(defn- fmt [v]
  (cond
    (nil? v) "—"
    (number? v) (let [r (/ (js/Math.round (* 10 v)) 10)] (str r))
    :else (str v)))

(defn- select-of [k value options on-pick & [attrs]]
  (h :select (merge {:data-k k :on-change #(on-pick (.. % -target -value))} attrs)
     (for [[v label] options]
       (h :option {:value v :selected (= v value)} label))))

(defn- size-select [k value on-pick & [sizes]]
  (select-of k value (map (fn [d] [d d]) (or sizes (designations))) on-pick))

(defn- role-select [k value on-pick]
  (select-of k (name (or value :beam)) (map (fn [[r l]] [(name r) l]) te/role-names) #(on-pick (keyword %))))

(defn- num-field [k value on-commit & [attrs]]
  (h :input (merge {:type "number" :step "any" :class "num" :data-k k :value (if (nil? value) "" value)
                    :on-change (fn [e] (on-commit (parse-double (.. e -target -value))))}
                   attrs)))

(defn- text-field [k value on-commit & [attrs]]
  (h :input (merge {:type "text" :data-k k :value (or value "") :spellcheck "false"
                    :on-change (fn [e] (on-commit (.. e -target -value)))}
                   attrs)))

(defn- error-text [e]
  (case (:error/type e)
    :param/formula-failed (str (:message e))
    :param/not-a-number "does not give a number"
    (pr-str e)))

(defn- regen-error-text
  "A regeneration error from a preview, in words where we know it."
  [{:keys [action] :as e}]
  (let [what (some-> action :action/type name)]
    (case (:error/type e)
      :action/no-plate "Bolt group: there is no end plate before it in this template to bolt through."
      :action/missing-input (str what ": the member or the supporting face isn't there.")
      :action/unknown (str "Unknown action " what ".")
      :part/unknown-profile (str "Unknown section " (pr-str (:profile e)) ".")
      (pr-str e))))

(defn- preview-host!
  "The canvas host for the open editor's 3D preview, created once per editor
  and reused across re-renders so the WebGL context survives typing."
  [dir]
  (let [depth (count (:stack @dlg*))]
    (when-not (= depth (get-in @ui* [:preview :depth]))
      (some-> (get-in @ui* [:preview :view]) mini-view/dispose!)
      (let [el (h :div {:class "mini-view"})]
        (swap! ui* assoc :preview {:depth depth :el el :view (mini-view/create! el dir)})))
    (get-in @ui* [:preview :el])))

(defn- drop-preview! []
  (some-> (get-in @ui* [:preview :view]) mini-view/dispose!)
  (swap! ui* dissoc :preview))

(defn- show-preview! [instructions & opts]
  (when-let [view (get-in @ui* [:preview :view])]
    (apply mini-view/show! view instructions opts)))

(defn- view-buttons [views]
  (h :div {:class "buttons small"}
     (for [[label dir] views]
       (h :button {:on-click #(some-> (get-in @ui* [:preview :view]) (mini-view/look! dir))} label))))

(defn- messages [items]
  (when (seq items) (h :ul {:class "errors"} (for [i items] (h :li {} i)))))

;; --- connection template editor -------------------------------------------------------

(def action-types
  [[:cut-to-face "Cut to face" "Cut the member square to the support face, leaving :gap."]
   [:end-plate "End plate" "A plate on the support face (:plate-t :plate-w :plate-h :plate-dx :plate-dy, :gap); the member is cut back behind it."]
   [:bolt-group "Bolt group" "Holes through this connection's end plate and bolts in them (:rows :cols :gauge :pitch :bolt-dia :hole-dia :bolt-length)."]])

(def faces [[:auto "facing face"] [:top "top"] [:bottom "bottom"] [:left "left"] [:right "right"]])

(defn- action-row [i n {:action/keys [type face] :as action}]
  (let [[_ label help] (some #(when (= type (first %)) %) action-types)]
    (h :li {:class "action"}
       (h :div {:class "action-head"}
          (h :strong {} (or label (name type)))
          (when (#{:cut-to-face :end-plate} type)
            (select-of (str "face-" i) (name (or face :auto)) (map (fn [[k l]] [(name k) l]) faces)
                       #(update-draft! update :template/actions assoc i (assoc action :action/face (keyword %)))))
          (h :span {:class "grow"})
          (h :button {:title "Move up" :disabled (zero? i)
                      :on-click #(update-draft! update :template/actions te/move i -1)} "↑")
          (h :button {:title "Move down" :disabled (= i (dec n))
                      :on-click #(update-draft! update :template/actions te/move i 1)} "↓")
          (h :button {:title "Remove" :on-click #(update-draft! update :template/actions te/remove-at i)} "✕"))
       (h :div {:class "muted small"} help))))

(defn- params-table [params {resolved :params errors :errors} {:keys [on-rename on-set on-remove]}]
  (let [error-of (into {} (map (juxt :param identity)) errors)]
    (h :table {:class "params"}
       (h :tr {} (h :th {} "Parameter") (h :th {} "Value or formula") (h :th {} "Preview") (h :th {}))
       (for [[k v] params]
         (h :tr {:class (when (error-of k) "bad")}
            (h :td {} (text-field (str "pk-" (name k)) (name k)
                                  #(if-let [k' (te/param-key %)] (on-rename k k') (render!))
                                  {:class "pname"}))
            (h :td {} (text-field (str "pv-" (name k)) (te/param-text v) #(on-set k (te/parse-param %))
                                  {:class (if (string? v) "formula" "number")}))
            (h :td {:class "resolved"} (if-let [e (error-of k)] (h :span {:class "warn"} (error-text e)) (fmt (get resolved k))))
            (h :td {} (h :button {:title "Remove" :on-click #(on-remove k)} "✕"))))
       (h :tr {}
          (h :td {} (text-field "pk-new" "" (fn [t]
                                              (when-let [k (te/param-key t)]
                                                (when-not (contains? params k) (on-set k 0))))
                                {:class "pname" :placeholder "new parameter"}))
          (h :td {:colspan 3 :class "muted small"} "Type a name and press Enter to add a row.")))))

(defn- connection-editor [{:keys [draft size support-size arrangement error saving?]}]
  (let [{:keys [instructions errors]} (te/connection-preview @lib (:templates @env*) draft
                                                             {:size size :support-size support-size
                                                              :arrangement arrangement})
        resolved (te/resolve-for @lib draft {} size support-size)
        n (count (:template/actions draft))
        host (preview-host! [1 -1.4 0.9])
        other-errors (remove :param errors)]
    (show-preview! instructions)
    (h :div {:class "dialog connection-editor"}
       (h :header {}
          (h :h2 {} (if (:template/id draft) "Connection template" "New connection template"))
          (text-field "name" (:template/name draft) #(update-draft! assoc :template/name %) {:class "name"})
          (h :span {:class "grow"})
          (h :button {:on-click close!} "Cancel")
          (h :button {:class "on" :disabled saving? :on-click save!} "Save"))
       (when error (h :div {:class "dialog-error"} error))
       (h :div {:class "dialog-body two-col"}
          (h :div {:class "col"}
             (h :p {:class "muted small"}
                "A connection joins the end of one member (the owner) to another (the support), whatever their roles: "
                "a beam to a column, a joist to a beam, a brace to a gusset.")
             (h :h3 {} "Actions, in order")
             (h :ol {:class "actions"}
                (map-indexed (fn [i a] (action-row i n a)) (:template/actions draft)))
             (h :div {:class "row"}
                (select-of "add-action" "" (cons ["" "Add an action…"] (map (fn [[k l]] [(name k) l]) action-types))
                           #(when (seq %)
                              (update-draft! update :template/actions (fnil conj []) {:action/type (keyword %)}))))
             (h :h3 {} "Parameters")
             (params-table (:template/params draft) resolved
                           {:on-rename #(update-draft! update :template/params te/rename-param %1 %2)
                            :on-set #(update-draft! update :template/params te/set-param %1 %2)
                            :on-remove #(update-draft! update :template/params te/remove-param %)})
             (h :p {:class "muted small"}
                "A value is a number or a formula, e.g. " (h :code {} "(+ d 20)") ". Formulas see the owner's section data ("
                (h :code {} "d bf tf tw r1") "…), " (h :code {} "support") " (the supporting member's, e.g. "
                (h :code {} "(:tf support)") ") and the other numeric parameters."))
          (h :div {:class "col"}
             (h :h3 {} "Preview")
             (h :table {:class "placement"}
                (h :tr {} (h :th {} "Owner") (h :td {} (size-select "size" size #(update-top! assoc :size %))))
                (h :tr {} (h :th {} "Support")
                   (h :td {} (size-select "support-size" support-size #(update-top! assoc :support-size %))))
                (h :tr {} (h :th {} "Support as")
                   (h :td {} (select-of "arrangement" (name arrangement)
                                        (map (fn [[k l]] [(name k) l]) te/arrangements)
                                        #(do (update-top! assoc :arrangement (keyword %))
                                             (show-preview! (:instructions (te/connection-preview
                                                                            @lib (:templates @env*) draft
                                                                            {:size size :support-size support-size
                                                                             :arrangement (keyword %)}))
                                                            :fit? true))))))
             (view-buttons [["Iso" [1 -1.4 0.9]] ["Plan" [0 0 1]] ["Side" [0 -1 0]] ["End" [-1 0 0]]])
             host
             (messages (map regen-error-text other-errors)))))))

;; --- assembly page ------------------------------------------------------------------------

(defn- end-on-view
  "SVG of the main member end-on: XLine at the origin (cross), section
  outline, and the nine insertion points (click to choose)."
  [size main on-pick]
  (if-let [{:keys [loops points b d]} (te/section-view @lib size main)]
    (let [xy (concat (apply concat loops) (vals points) [[0 0]])
          xs (map first xy) ys (map second xy)
          [x0 x1 y0 y1] [(apply min xs) (apply max xs) (apply min ys) (apply max ys)]
          span (max (- x1 x0) (- y1 y0) 50)
          pad (* 0.18 span)
          r (* 0.025 span)
          cx (/ (+ x0 x1) 2) cy (/ (+ y0 y1) 2)
          half (+ (/ span 2) pad)
          ;; SVG y runs down: draw at (x, -y)
          pts (fn [loop] (str/join " " (map (fn [[x y]] (str x "," (- y))) loop)))
          chosen (:part/insert main :centre)]
      (s :svg {:viewBox (str/join " " [(- cx half) (- (- cy) half) (* 2 half) (* 2 half)])
               :class "end-on"}
         (s :line {:x1 (- cx half) :y1 0 :x2 (+ cx half) :y2 0 :class "axis"})
         (s :line {:x1 0 :y1 (- (- cy) half) :x2 0 :y2 (+ (- cy) half) :class "axis"})
         (for [loop loops] (s :polygon {:points (pts loop) :class "section"}))
         (s :circle {:cx 0 :cy 0 :r (* 1.6 r) :class "xline"})
         (for [[k [x y]] points]
           (s :circle {:cx x :cy (- y) :r r :class (str "ipoint" (when (= k chosen) " chosen"))
                       :on-click #(on-pick k)}
              (s :title {} (str (name k) (when (= k chosen) " (on the XLine)")))))
         (s :text {:x (- cx half (- (* 0.3 pad))) :y (+ (- cy) half (- (* 0.3 pad))) :class "dim"}
            (str (fmt b) " × " (fmt d)))))
    (h :p {:class "warn"} (if (map? size) "A custom section: no end-on view." (str "Unknown section " size)))))

(defn- side-view
  "SVG side elevation of the member alone on its XLine (start on the left):
  the XLine and its two nodes, the member with its extensions, and the
  section's flange faces as lines along it."
  [size main length]
  (when-let [{[l0 l1] :xline [z0 z1] :member [y0 y1] :y edges :edges} (te/side-elevation @lib size main length)]
    (let [x0 (min l0 z0) x1 (max l1 z1)
          ylo (min y0 0) yhi (max y1 0)
          pad (* 0.06 (- x1 x0))
          w (+ (- x1 x0) (* 2 pad))
          hgt (max (+ (- yhi ylo) (* 2 pad)) (* 0.18 w))
          cy (/ (+ ylo yhi) 2)
          r (* 0.008 w)]
      (s :svg {:viewBox (str/join " " [(- x0 pad) (- (- cy) (/ hgt 2)) w hgt]) :class "side-view"}
         (s :rect {:x z0 :y (- y1) :width (- z1 z0) :height (- y1 y0) :class "member"})
         (for [y edges] (s :line {:x1 z0 :y1 (- y) :x2 z1 :y2 (- y) :class "edge"}))
         (s :line {:x1 l0 :y1 0 :x2 l1 :y2 0 :class "xline"})
         (for [[x label] [[l0 "start"] [l1 "end"]]]
           (s :circle {:cx x :cy 0 :r r :class "node"} (s :title {} (str label " node"))))))))

(defn- main-member-section
  "The main member on its XLine: end-on view, placement and side elevation."
  [{:keys [kind draft] :as dlg} size length size-control]
  (let [k (:main (paths kind))
        main (merge te/default-main (get draft k))
        set-main! (fn [mk v] (update-draft! assoc-in [k mk] v))
        [ox oy] (:part/offset main [0 0])]
    (list
     (h :section {:class "main-member"}
        (h :h3 {} "Main member, end-on")
        size-control
        (end-on-view size main #(set-main! :part/insert %))
        (h :p {:class "muted small"} "Top flange up, across the view is the XLine's local X. Click one of the nine points to put it on the XLine (the cross)."))
     (h :section {:class "placement-col"}
        (h :h3 {} "On its XLine")
        (h :table {:class "placement"}
           (h :tr {} (h :th {} "On the XLine") (h :td {} (str/replace (name (:part/insert main)) "-" " ")))
           (h :tr {} (h :th {} "Offset X") (h :td {} (num-field "ox" ox #(set-main! :part/offset [(or % 0) oy]))))
           (h :tr {} (h :th {} "Offset Y") (h :td {} (num-field "oy" oy #(set-main! :part/offset [ox (or % 0)]))))
           (h :tr {} (h :th {} "Rotation")
              (h :td {}
                 (h :div {:class "buttons small"}
                    (for [a [0 90 180 270]]
                      (h :button {:class (when (== a (:part/rotation main)) "on")
                                  :on-click #(set-main! :part/rotation a)} (str a "°"))))
                 (num-field "rot" (:part/rotation main) #(set-main! :part/rotation (or % 0)))))
           (h :tr {} (h :th {} "Start extension") (h :td {} (num-field "sx" (:part/start-ext main) #(set-main! :part/start-ext (or % 0)))))
           (h :tr {} (h :th {} "End extension") (h :td {} (num-field "ex" (:part/end-ext main) #(set-main! :part/end-ext (or % 0))))))
        (h :p {:class "muted small"} "An extension lengthens the member past its node; a negative one sets it back."))
     (h :section {:class "elevation"}
        (h :h3 {} "Side elevation")
        (side-view size main length)
        (h :p {:class "muted small"}
           (str "The member alone on " (if (= :instance kind) "its" "a") " " (fmt length)
                " mm XLine, start on the left. What it meets at each end is the connection's business."))))))

(defn- end-card
  "One end: the connection template (or none), its parameters for this
  member's size and the per-end overrides. A template's card resolves
  without a support; an assembly's against the member its end sits on."
  [end {:keys [kind draft supports]} size]
  (let [k (end (paths kind))
        instance? (= :instance kind)
        support (get supports end)
        support-size (:size support)
        condition (get draft k)
        summary (te/end-summary @lib (:templates @env*) condition size support-size)
        set-condition! (fn [c] (update-draft! #(if c (assoc % k c) (dissoc % k))))
        error-of (into {} (map (juxt :param identity)) (:errors summary))
        options (cons ["" "None (plain end)"]
                      (map (fn [t] [(str (:template/id t)) (:template/name t)]) (templates-of :connection)))]
    (h :div {:class "end-card"}
       (h :h3 {} (if (= :start end) "Start" "End"))
       (when instance?
         (h :p {:class "muted small"}
            (if support
              (str "Sits on " (:assembly/mark support) (when support-size (str ", " support-size)) ".")
              "Not on another XLine: no connection is made here.")))
       (h :div {:class "row"}
          "Connection "
          (select-of (str (name end) "-tpl") (str (:connection/template condition ""))
                     options
                     (fn [v] (set-condition! (when-let [t (some #(when (= v (str (:template/id %))) %)
                                                              (templates-of :connection))]
                                               {:connection/template (:template/id t)})))))
       (if-not summary
         (h :p {:class "muted"} (if condition "Unknown connection template." "No connection: the member end is left as it is."))
         (list
          (h :div {:class "row"}
             (h :span {:class "muted small"}
                (str/join " → " (map #(name (:action/type %)) (:template/actions (:template summary)))))
             (h :span {:class "grow"})
             (h :a {:href "#" :on-click (fn [e] (.preventDefault e) (open! (:template summary)))}
                "Open in the connection editor"))
          (h :table {:class "params"}
             (h :tr {} (h :th {} "Parameter") (h :th {} (str "At " (if (map? size) "this size" size))) (h :th {} "Override"))
             (for [{:keys [param value template-value override overridden? needs-support?]} (:rows summary)]
               (h :tr {:class (str (when overridden? "overridden") (when (error-of param) " bad"))}
                  (h :td {} (name param))
                  (h :td {:class "resolved"}
                     (cond
                       (error-of param) (h :span {:class "warn"} (error-text (error-of param)))
                       needs-support? (h :span {:class "muted" :title "A formula over the support: it is worked out where the member meets one"}
                                         "from the support")
                       :else (fmt value)))
                  (h :td {} (text-field (str (name end) "-ov-" (name param)) (when overridden? (te/param-text override))
                                        (fn [t]
                                          (let [v (te/parse-param t)]
                                            (update-draft! update-in [k :connection/params]
                                                           #(if (nil? v) (dissoc % param) (assoc (or % {}) param v)))))
                                        {:placeholder (te/param-text template-value)}))))))))))

(defn- regen-messages [errors]
  (messages (map regen-error-text (remove :param errors))))

(defn- member-preview
  "The 3D preview of the member alone: main part, plates, bolts, holes and
  cuts as its connections made them, its XLine as a thin line with a ball at
  each node. Nothing it connects to is drawn."
  [{:keys [instructions nodes]} note]
  (let [views (te/preview-views nodes)
        host (preview-host! (second (first views)))]
    (show-preview! instructions :nodes nodes)
    (h :section {:class "member-preview"}
       (h :div {:class "row"}
          (h :h3 {} "3D preview")
          (h :span {:class "grow"})
          (view-buttons views))
       host
       (h :p {:class "muted small"} note))))

(defn- assembly-builder [{:keys [draft size error saving?] :as dlg}]
  (let [{:keys [errors] :as preview} (te/assembly-preview @lib (:templates @env*) draft
                                                          {:size size :support-size (:support-size dlg)})]
    (h :div {:class "dialog assembly-builder"}
       (h :header {}
          (h :h2 {} (if (:template/id draft) "Assembly template" "New assembly template"))
          (text-field "name" (:template/name draft) #(update-draft! assoc :template/name %) {:class "name"})
          (h :label {:title "Only a hint: size pickers offer this role's usual sections first. Applying a template never changes a member's role."}
             "Usually for "
             (role-select "role" (:template/role draft)
                          #(update-top! (fn [d] (merge (assoc-in d [:draft :template/role] %)
                                                       (default-sizes :assembly {:template/role %}))))))
          (h :span {:class "grow"})
          (h :button {:on-click close!} "Cancel")
          (h :button {:class "on" :disabled saving? :on-click save!} "Save"))
       (when error (h :div {:class "dialog-error"} error))
       (h :div {:class "dialog-body assembly"}
          (main-member-section dlg size te/preview-length
                               (h :div {:class "row"} "Preview size "
                                  (size-select "size" size #(update-top! assoc :size %)
                                               (sizes-for (:template/role draft)))))
          (member-preview preview
                          (str "The member alone, on a " (fmt te/preview-length) " mm XLine (the thin line, a ball at each node). "
                               "An end with a connection is made against a " (:support-size dlg)
                               " column, as in the connection editor, which isn't drawn."))
          (h :div {:class "ends"}
             (end-card :start dlg size)
             (end-card :end dlg size))
          (h :div {:class "messages"} (regen-messages errors))))))

(defn- template-note [{:assembly/keys [template from-template]}]
  (cond
    template (list "From template " (h :strong {} (or (template-name template) "(unknown)"))
                   ": editing the template updates this assembly.")
    from-template (list "Edited by hand, from template " (h :strong {} (or (template-name from-template) "(unknown)"))
                        ": template edits no longer change it.")
    :else "Not from a template."))

(defn- apply-row [{:keys [draft apply saving?]}]
  (let [candidates (sort-by #(if (= (:xline/role draft) (:template/role %)) 0 1) (templates-of :assembly))
        tid (or (:template-id apply) (:template/id (first candidates)))
        sizes (sizes-for (:xline/role draft))
        size (or (:size apply) (when (string? (:assembly/size draft)) (:assembly/size draft)) (first sizes))]
    (h :div {:class "apply-row"}
       (h :strong {} "Apply a template instead ")
       (select-of "apply-tpl" (str tid) (map (fn [t] [(str (:template/id t)) (:template/name t)]) candidates)
                  (fn [v] (update-top! assoc-in [:apply :template-id]
                                       (some #(when (= v (str (:template/id %))) (:template/id %)) candidates))))
       " at "
       (size-select "apply-size" size #(update-top! assoc-in [:apply :size] %) sizes)
       (h :button {:class "on" :disabled (or saving? (nil? tid))
                   :on-click (fn [] (update-top! update :apply assoc :template-id tid :size size) (apply-template!))}
          "Apply")
       (h :button {:on-click #(update-top! dissoc :apply)} "Close")
       (h :span {:class "muted small"} "Replaces the placement and both ends with the template's; unsaved changes here are lost."))))

(defn- assembly-instance [{:keys [draft info rotation error saving? apply context] :as dlg}]
  (let [size (:assembly/size draft)
        role (:xline/role draft :beam)
        preview (when context (te/instance-preview @lib (:templates @env*) context draft))]
    (h :div {:class "dialog assembly-builder"}
       (h :header {}
          (h :h2 {} "Assembly")
          (h :label {} "Mark "
             (text-field "mark" (:assembly/mark draft) #(update-draft! assoc :assembly/mark %) {:class "mark"}))
          (h :label {} "Role " (role-select "role" role #(update-draft! assoc :xline/role %)))
          (h :span {:class "grow"})
          (h :button {:class (when apply "on") :disabled saving?
                      :on-click #(update-top! (fn [d] (if (:apply d) (dissoc d :apply) (assoc d :apply {}))))}
             "Apply template…")
          (h :button {:on-click close!} "Cancel")
          (h :button {:class "on" :disabled saving? :on-click save!} "Save"))
       (when error (h :div {:class "dialog-error"} error))
       (when apply (apply-row dlg))
       (h :div {:class "instance-info"}
          (h :span {} (template-note draft))
          (h :span {} (h :span {:class "muted"} "Start ") (:start info))
          (h :span {} (h :span {:class "muted"} "End ") (:end info))
          (h :span {} (h :span {:class "muted"} "Length ") (str (fmt (:length info)) " mm"))
          (h :label {} (h :span {:class "muted"} "XLine rotation ")
             (num-field "xrot" rotation #(update-top! assoc :rotation (or % 0)))))
       (h :div {:class "dialog-body assembly"}
          (main-member-section dlg size (:length info)
                               (h :div {:class "row"} "Size "
                                  (if (map? size)
                                    (h :span {:class "muted"} "custom section")
                                    (size-select "size" size #(update-draft! assoc :assembly/size %)
                                                 (sizes-for role)))))
          (when preview
            (member-preview preview
                            (str "This assembly alone, as Save would make it, on its XLine (the thin line, a ball at each node). "
                                 "What its ends sit on isn't drawn.")))
          (h :div {:class "ends"}
             (end-card :start dlg size)
             (end-card :end dlg size))
          (h :div {:class "messages"} (regen-messages (:errors preview)))))))

;; --- dialog and library panel -----------------------------------------------------------

(defn render! []
  (let [root (or (:root @ui*)
                 (let [el (h :div {:class "dialog-backdrop"})]
                   (.append (.-body js/document) el)
                   (swap! ui* assoc :root el)
                   el))
        dlg (top)]
    (when-not dlg (drop-preview!))
    (if-not dlg
      (do (set! (.. root -style -display) "none")
          (.replaceChildren root))
      (do (set! (.. root -style -display) "flex")
          (dom/replace-keeping-focus!
           root [(case (:kind dlg)
                   :connection (connection-editor dlg)
                   :assembly (assembly-builder dlg)
                   :instance (assembly-instance dlg))])))))

(defn library-section
  "The side panel's template library: both kinds, with new, copy and edit."
  []
  (let [rows (fn [kind]
               (for [t (templates-of kind)]
                 (h :tr {}
                    (h :td {} (:template/name t))
                    (h :td {:class "tight"}
                       (h :button {:class "small" :on-click #(open! t)} "Edit")
                       (h :button {:class "small" :on-click #(open! (te/duplicate t))} "Copy")))))]
    (h :section {}
       (h :h2 {} "Templates")
       (h :table {:class "library"}
          (h :tr {} (h :th {:colspan 2} "Assembly"))
          (rows :assembly)
          (h :tr {} (h :th {:colspan 2} "Connection"))
          (rows :connection))
       (h :div {:class "buttons"}
          (h :button {:on-click #(open! (te/blank :assembly))} "New assembly")
          (h :button {:on-click #(open! (te/blank :connection))} "New connection")))))

(defn templates-changed!
  "New templates from the server: keep open editors' references fresh."
  [templates]
  (swap! env* assoc :templates templates)
  (when (top) (render!)))
