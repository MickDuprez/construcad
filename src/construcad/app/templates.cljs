(ns construcad.app.templates
  "The template library panel and the two template editors, shown as a
  dialog over the editor:

  - the assembly template builder: the main member end-on with its nine
    insertion points, offset, rotation and extensions, a 3D preview of the
    member on its XLine between two columns, and a start and an end
    condition card (connection template, resolved parameters, overrides);
  - the connection template editor: ordered actions, parameters as numbers
    or formulas, and a 3D preview of a beam meeting a column.

  Previews regenerate in the browser with the real core and the in-browser
  section library (construcad.template-edit); only Save goes to the server,
  as a :template/save edit. Editors stack: an end condition's connection
  template opens over the assembly builder and returns to it."
  (:require [clojure.string :as str]
            [construcad.app.dom :as dom :refer [h s]]
            [construcad.app.mini-view :as mini-view]
            [construcad.template-edit :as te]
            [modeller.library :as library]
            [modeller.sections-au :as au]))

(defonce lib (delay (library/library {} au/rows)))

;; :templates {id t} and :sections [{:section/designation ..}] come from the
;; server (GET /templates, and patches); :save! sends a :template/save edit
;; and returns a promise of true when the server took it.
(defonce env* (atom {:templates {} :sections [] :save! nil}))

;; The open editors, innermost last: {:kind :draft :size :support-size :error}.
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

;; --- opening and closing ------------------------------------------------------------

(defn- default-sizes [kind draft]
  (if (and (= :assembly kind) (= :column (:template/role draft)))
    {:size "200UC46.2" :support-size "200UC46.2"}
    {:size "310UB40.4" :support-size "200UC46.2"}))

(defn open!
  "Open an editor for a template map (an existing one, a duplicate or a blank)."
  [draft]
  (let [kind (:template/kind draft)]
    (swap! dlg* update :stack conj (merge {:kind kind :draft draft} (default-sizes kind draft)))
    (render!)))

(defn- close! []
  (swap! dlg* update :stack pop)
  (render!))

(defn- save! []
  (let [{:keys [draft]} (top)
        problems (te/draft-errors draft)]
    (if (seq problems)
      (update-top! assoc :error (str/join " " problems))
      (do (update-top! assoc :error nil :saving? true)
          (-> ((:save! @env*) draft)
              (.then (fn [ok?]
                       (if ok?
                         (close!)
                         (update-top! assoc :saving? false :error "The server refused the template; see the status bar.")))))))))

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

(defn- size-select [k value on-pick]
  (select-of k value (map (fn [d] [d d]) (designations)) on-pick))

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

(defn- connection-editor [{:keys [draft size support-size error saving?]}]
  (let [{:keys [instructions errors]} (te/connection-preview @lib (:templates @env*) draft
                                                             {:size size :support-size support-size})
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
                "A value is a number or a formula, e.g. " (h :code {} "(+ d 20)") ". Formulas see the member's section data ("
                (h :code {} "d bf tf tw r1") "…), " (h :code {} "support") " (the supporting member's, e.g. "
                (h :code {} "(:tf support)") ") and the other numeric parameters."))
          (h :div {:class "col"}
             (h :h3 {} "Preview")
             (h :div {:class "row"}
                "Beam " (size-select "size" size #(update-top! assoc :size %))
                " into column " (size-select "support-size" support-size #(update-top! assoc :support-size %)))
             (view-buttons [["Iso" [1 -1.4 0.9]] ["Plan" [0 0 1]] ["Side" [0 -1 0]] ["End" [-1 0 0]]])
             host
             (messages (map pr-str other-errors)))))))

;; --- assembly template builder ---------------------------------------------------------

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
    (h :p {:class "warn"} "Unknown section " size)))

(defn- end-card [end {:keys [draft size support-size]}]
  (let [k (if (= :start end) :template/start :template/end)
        condition (get draft k)
        summary (te/end-summary @lib (:templates @env*) condition size support-size)
        set-condition! (fn [c] (update-draft! #(if c (assoc % k c) (dissoc % k))))
        error-of (into {} (map (juxt :param identity)) (:errors summary))
        options (cons ["" "None (plain end)"]
                      (map (fn [t] [(str (:template/id t)) (:template/name t)]) (templates-of :connection)))]
    (h :div {:class "end-card"}
       (h :h3 {} (if (= :start end) "Start" "End"))
       (select-of (str (name end) "-tpl") (str (:connection/template condition ""))
                  options
                  (fn [v] (set-condition! (when-let [t (some #(when (= v (str (:template/id %))) %)
                                                           (templates-of :connection))]
                                            {:connection/template (:template/id t)}))))
       (if-not summary
         (h :p {:class "muted"} (if condition "Unknown connection template." "No connection: the member end is left as it is."))
         (list
          (h :div {:class "row"}
             (h :strong {} (:template/name (:template summary)))
             (h :span {:class "grow"})
             (h :a {:href "#" :on-click (fn [e] (.preventDefault e) (open! (:template summary)))}
                "Edit connection template"))
          (h :div {:class "muted small"}
             (str/join " → " (map #(name (:action/type %)) (:template/actions (:template summary)))))
          (h :table {:class "params"}
             (h :tr {} (h :th {} "Parameter") (h :th {} (str "At " size)) (h :th {} "Override"))
             (for [{:keys [param value template-value override overridden?]} (:rows summary)]
               (h :tr {:class (str (when overridden? "overridden") (when (error-of param) " bad"))}
                  (h :td {} (name param))
                  (h :td {:class "resolved"} (if-let [e (error-of param)] (h :span {:class "warn"} (error-text e)) (fmt value)))
                  (h :td {} (text-field (str (name end) "-ov-" (name param)) (when overridden? (te/param-text override))
                                        (fn [t]
                                          (let [v (te/parse-param t)]
                                            (update-draft! update-in [k :connection/params]
                                                           #(if (nil? v) (dissoc % param) (assoc (or % {}) param v)))))
                                        {:placeholder (te/param-text template-value)}))))))))))

(defn- assembly-builder [{:keys [draft size support-size error saving?] :as dlg}]
  (let [main (merge te/default-main (:template/main draft))
        set-main! (fn [k v] (update-draft! assoc-in [:template/main k] v))
        {:keys [instructions errors warnings]} (te/assembly-preview @lib (:templates @env*) draft
                                                                   {:size size :support-size support-size})
        column? (= :column (:template/role draft))
        side (if column? [0 -1 0.15] [0 -1 0])
        host (preview-host! side)
        [ox oy] (:part/offset main [0 0])]
    (show-preview! instructions)
    (h :div {:class "dialog assembly-builder"}
       (h :header {}
          (h :h2 {} (if (:template/id draft) "Assembly template" "New assembly template"))
          (text-field "name" (:template/name draft) #(update-draft! assoc :template/name %) {:class "name"})
          (h :label {} "Role "
             (select-of "role" (name (:template/role draft :beam)) [["beam" "Beam"] ["column" "Column"]]
                        #(update-top! (fn [d] (merge (assoc-in d [:draft :template/role] (keyword %))
                                                     (default-sizes :assembly {:template/role (keyword %)}))))))
          (h :span {:class "grow"})
          (h :button {:on-click close!} "Cancel")
          (h :button {:class "on" :disabled saving? :on-click save!} "Save"))
       (when error (h :div {:class "dialog-error"} error))
       (h :div {:class "dialog-body assembly"}
          (h :section {:class "main-member"}
             (h :h3 {} "Main member, end-on")
             (h :div {:class "row"} "Preview size " (size-select "size" size #(update-top! assoc :size %)))
             (end-on-view size main #(set-main! :part/insert %))
             (h :p {:class "muted small"} "Top flange up, across the view is the XLine's local X. Click one of the nine points to put it on the XLine (the cross).")
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
                (h :tr {} (h :th {} "End extension") (h :td {} (num-field "ex" (:part/end-ext main) #(set-main! :part/end-ext (or % 0)))))))
          (h :section {:class "elevation"}
             (h :h3 {} (if column? "Elevation" "Side elevation, on its XLine"))
             (h :div {:class "row"}
                (if column? "Column alone (its ends sit on nothing)"
                    (list "Between columns " (size-select "support-size" support-size #(update-top! assoc :support-size %)))))
             (view-buttons [["Side" side] ["Iso" [1 -1.4 0.9]] ["Plan" [0 0 1]]])
             host
             (messages (concat (map #(str "The " (name (:end %)) " is not on another XLine: no connection there.") warnings)
                               (map pr-str errors))))
          (h :div {:class "ends"}
             (end-card :start dlg)
             (end-card :end dlg))))))

;; --- dialog and library panel -----------------------------------------------------------

(defn render! []
  (let [root (or (:root @ui*)
                 (let [el (h :div {:class "dialog-backdrop"})]
                   (.append (.-body js/document) el)
                   (swap! ui* assoc :root el)
                   el))
        dlg (top)]
    (if-not dlg
      (do (some-> (get-in @ui* [:preview :view]) mini-view/dispose!)
          (swap! ui* dissoc :preview)
          (set! (.. root -style -display) "none")
          (.replaceChildren root))
      (do (set! (.. root -style -display) "flex")
          (dom/replace-keeping-focus!
           root [(case (:kind dlg)
                   :connection (connection-editor dlg)
                   :assembly (assembly-builder dlg))])))))

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
