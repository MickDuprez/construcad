(ns construcad.app.templates
  "The template library panel and the editors, shown as a dialog over the
  editor. Templates are starting points: applying one copies it into the
  assembly and its connections, so editing the library never changes an
  assembly, and editing an assembly's connection never changes the library.

  - the assembly page, split in two: on the left the insertion panel (the
    member end-on with its nine insertion points, offset, rotation and
    extensions); on the right one 3D view of the member on its XLine,
    opening side-on (start on the left), with the Start end's card on its
    left edge and the End's on its right. A card shows the end's connection
    by name (with the library template's thumbnail) and Edit connection….
    It edits an assembly template (a sample member; a card picks a library
    connection template), or (instance mode) one assembly in the model, read
    from the server with modeller.template/assembly-state, with its mark,
    role and size; Save sends :assembly/update and Re-apply from library…
    copies a template again;
  - the connection editor: ordered actions, parameters as numbers or
    formulas, and a 3D preview. In library mode it edits a library template,
    previewed on a sample owner framing into a sample support (a vertical
    column or a horizontal beam); Save sends :template/save with a small
    thumbnail of the preview. In instance mode it edits one assembly's own
    copy of a connection, previewed on the real member and what it sits on;
    Save sends :connection/update, and Save as new library template… puts
    a copy in the library.

  Previews regenerate in the browser with the real core and the in-browser
  section library (construcad.template-edit); only Save goes to the server.
  Editors stack: an end's connection opens over the assembly page and
  returns to it."
  (:require [clojure.string :as str]
            [construcad.app.dom :as dom :refer [h s]]
            [construcad.app.mini-view :as mini-view]
            [construcad.template-edit :as te]
            [modeller.library :as library]
            [modeller.sections-au :as au]
            [modeller.template :as template]))

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

(defn open-connection-instance!
  "Open the connection editor in instance mode on one end of an assembly:
  `page` is its page data (construcad.edits/assembly-page, or the assembly
  page's own draft state with the same context), `end` :start or :end.
  From the assembly page (`from-page?`), Save also puts the copy into the
  page's draft."
  [{:keys [state context supports]} end & {:keys [from-page?]}]
  (when-let [copy (get state end)]
    (swap! dlg* update :stack conj
           {:kind :conn-instance :draft (template/copy->template copy)
            :from (:connection/from-template copy) :connection-id (:connection/id copy)
            :end end :state state :context context :supports supports :from-page? from-page?})
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

(def thumbnail-size
  "A template's thumbnail, in pixels: small, it is stored with the template."
  [240 160])

(defn- thumbnail
  "The open editor's preview as a thumbnail (a PNG data URL), or nil."
  []
  (some-> (get-in @ui* [:preview :view]) (mini-view/snapshot (thumbnail-size 0) (thumbnail-size 1))))

(defn- with-thumbnail [t]
  (if-let [png (thumbnail)] (assoc t :template/thumbnail png) t))

(defn- copy-into-page!
  "After an end's connection was edited over the assembly page, put the
  copy into the page's draft (the dialog now on top)."
  [end copy]
  (when (= :instance (:kind (top)))
    (update-draft! assoc end copy)))

(defn- save-connection-instance!
  "Save one assembly's own connection: :connection/update when it is
  stored, else (an end picked on the assembly page and not saved yet) only
  into the page's draft, which saves it with the assembly."
  [{:keys [draft from connection-id end state from-page?]}]
  (let [copy (cond-> (template/template->copy draft from)
               connection-id (assoc :connection/id connection-id))
        done! (fn [] (close!) (when from-page? (copy-into-page! end copy)))]
    (if-not connection-id
      (done!)
      (-> ((:send! @env*) {:edit/type :connection/update :connection/id connection-id
                           :xline/id (:xline/id state)
                           :connection (dissoc copy :connection/id)})
          (.then #(if % (done!) (refused!)))))))

(defn- save! []
  (let [{:keys [kind draft] :as dlg} (top)
        problems (when-not (= :instance kind) (te/draft-errors draft))]
    (if (seq problems)
      (update-top! assoc :error (str/join " " problems))
      (do (update-top! assoc :error nil :saving? true)
          (case kind
            :instance (-> (send-all! (te/save-edits draft dlg)) (.then #(if % (close!) (refused!))))
            :conn-instance (save-connection-instance! dlg)
            (-> ((:send! @env*) {:edit/type :template/save :template (with-thumbnail draft)})
                (.then #(if % (close!) (refused!)))))))))

(defn- save-to-library!
  "Put the instance connection editor's draft in the library as a new
  connection template, with a thumbnail of the real connection. The copy
  then notes it came from there (recorded when the connection is saved)."
  []
  (let [{:keys [draft]} (top)
        problems (te/draft-errors draft)
        before (set (keys (:templates @env*)))]
    (if (seq problems)
      (update-top! assoc :error (str/join " " problems))
      (do (update-top! assoc :error nil :saving? true)
          (-> ((:send! @env*) {:edit/type :template/save
                               :template (with-thumbnail (-> draft (dissoc :template/id) (assoc :template/kind :connection)))})
              (.then (fn [ok?]
                       (if-not ok?
                         (refused!)
                         (let [id (first (remove before (keys (:templates @env*))))]
                           (update-top! assoc :saving? false
                                        :from (template/provenance (get-in @env* [:templates id]))
                                        :note "Saved to the library. Save to keep the note of where this connection came from."))))))))))

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

(defn- size-of
  "The designation of an XLine's main part in a page context, or nil."
  [context id]
  (let [p (get-in context [:xlines id :assembly/parts 0 :part/profile])]
    (when (string? p) p)))

(defn- connection-editor
  "The connection editor. Library mode (:connection) edits a library
  template against a sample owner and support; instance mode
  (:conn-instance) edits one assembly's own copy against the real member
  and what that end sits on."
  [{:keys [kind draft arrangement error note saving? from end state context supports] :as dlg}]
  (let [instance? (= :conn-instance kind)
        support-id (get-in state [:supports end])
        size (if instance? (:assembly/size state) (:size dlg))
        support-size (if instance? (or (get-in supports [end :size]) (size-of context support-id)) (:support-size dlg))
        {:keys [instructions errors nodes]}
        (if instance?
          (te/connection-instance-preview @lib (:templates @env*) context state end draft)
          (te/connection-preview @lib (:templates @env*) draft
                                 {:size size :support-size support-size :arrangement arrangement}))
        resolved (te/resolve-for @lib draft {} (when (string? size) size) support-size)
        n (count (:template/actions draft))
        iso (if instance? (second (first (te/preview-views nodes))) [1 -1.4 0.9])
        host (preview-host! iso)
        other-errors (remove :param errors)]
    (show-preview! instructions)
    (h :div {:class "dialog connection-editor"}
       (h :header {}
          (h :h2 {} (cond
                      instance? (str (:assembly/mark state) ", " (if (= :start end) "start" "end") " connection")
                      (:template/id draft) "Connection template"
                      :else "New connection template"))
          (text-field "name" (:template/name draft) #(update-draft! assoc :template/name %) {:class "name"})
          (h :span {:class "grow"})
          (when instance?
            (h :button {:disabled saving? :on-click save-to-library!
                        :title "Put a copy of this connection in the template library"}
               "Save as new library template"))
          (h :button {:on-click close!} "Cancel")
          (h :button {:class "on" :disabled saving? :on-click save!} "Save"))
       (when error (h :div {:class "dialog-error"} error))
       (when instance?
         (h :div {:class "instance-info"}
            (h :span {} (if from
                          (list (h :span {:class "muted"} "from library: ") (h :strong {} (or (:template/name from) "(unnamed)")))
                          (h :span {:class "muted"} "not from a library template")))
            (h :span {:class "muted small"}
               "This assembly's own copy: saving changes this end only, never the library.")
            (when note (h :span {:class "small"} note))))
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
             (if instance?
               (h :p {:class "muted small"}
                  (str (:assembly/mark state) (when (string? size) (str ", " size)) " into "
                       (get-in supports [end :assembly/mark] "its support")
                       (when support-size (str ", " support-size))
                       ", where they are in the model, as Save would make them."))
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
                                                              :fit? true)))))))
             (when-not instance?
               (h :p {:class "muted small"}
                  "A sample owner and support: saving changes the library template only. "
                  "Assemblies already made from it keep their own copies."))
             (view-buttons (if instance?
                             (te/preview-views nodes)
                             [["Iso" [1 -1.4 0.9]] ["Plan" [0 0 1]] ["Side" [0 -1 0]] ["End" [-1 0 0]]]))
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

(defn- main-member-section
  "The main member on its XLine: end-on view and placement."
  [{:keys [kind draft]} size size-control]
  (let [k (:main (paths kind))
        main (merge te/default-main (get draft k))
        set-main! (fn [mk v] (update-draft! assoc-in [k mk] v))
        [ox oy] (:part/offset main [0 0])]
    (h :div {:class "member-setup"}
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
        (h :p {:class "muted small"} "An extension lengthens the member past its node; a negative one sets it back.")))))

(defn- thumb
  "A library template's thumbnail, or nothing."
  [src & [title]]
  (when src (h :img {:class "thumb" :src src :alt "" :title (or title "")})))

(defn- connection-options
  "The library's connection templates for a picker, after `first-option`."
  [first-option]
  (cons first-option (map (fn [t] [(str (:template/id t)) (:template/name t)]) (templates-of :connection))))

(defn- library-connection [v]
  (some #(when (= v (str (:template/id %))) %) (templates-of :connection)))

(defn- end-card
  "One end, on the edge of the Assembly view it belongs to: the
  connection's name (with its library template's thumbnail) and Edit
  connection…. An assembly template's end picks a library connection
  template, and Edit opens it in the library; an assembly's end is its own
  copy, which Edit opens in instance mode against the real member and what
  it sits on, and Replace swaps for a fresh copy of a library template."
  [end {:keys [kind draft supports context] :as dlg}]
  (let [k (end (paths kind))
        instance? (= :instance kind)
        support (get supports end)
        condition (get draft k)
        info (te/end-info (:templates @env*) condition)
        set-condition! (fn [c] (update-draft! #(if c (assoc % k c) (dissoc % k))))
        edit! (fn [e]
                (.preventDefault e)
                (if instance?
                  (open-connection-instance! {:state draft :context context :supports supports} end :from-page? true)
                  (when-let [t (get-in @env* [:templates (:connection/template condition)])] (open! t))))]
    (h :div {:class (str "end-card " (name end))}
       (h :h3 {} (if (= :start end) "Start" "End"))
       (when instance?
         (h :p {:class "muted small"}
            (if support
              (str "On " (:assembly/mark support) (when-let [s (:size support)] (str ", " s)))
              "Not on another XLine: no connection here.")))
       (cond
         (nil? info) (h :p {:class "muted"} "Plain end")
         (not (:known? info)) (h :p {:class "warn small"} "Unknown connection template.")
         :else (list (thumb (:thumbnail info) (get-in info [:from :template/name]))
                     (h :div {:class "conn-name"} (h :strong {} (:name info)))
                     (when (and instance? (:from info))
                       (h :div {:class "muted small"} "from library: " (or (get-in info [:from :template/name]) "(unnamed)")))
                     (h :div {:class "muted small"} (str/join " → " (map name (:actions info))))
                     (when (or (not instance?) support)
                       (h :a {:href "#" :class "edit-link" :on-click edit!} "Edit connection…"))))
       (when (or (not instance?) support)
         (h :div {:class "row"}
            (select-of (str (name end) "-tpl")
                       (if instance? "" (str (:connection/template condition "")))
                       (if instance?
                         (concat [["" (if condition "Replace with…" "Add a connection…")]]
                                 (when condition [["none" "None (plain end)"]])
                                 (rest (connection-options nil)))
                         (connection-options ["" "None (plain end)"]))
                       (fn [v]
                         (let [t (library-connection v)]
                           (cond
                             ;; an assembly's end gets its own copy, on the connection it had
                             instance? (cond
                                         (= "none" v) (set-condition! nil)
                                         t (set-condition! (cond-> (template/connection-copy
                                                                    (:templates @env*) {:connection/template (:template/id t)})
                                                             (:connection/id condition)
                                                             (assoc :connection/id (:connection/id condition)))))
                             :else (set-condition! (when t {:connection/template (:template/id t)}))))))))
       (when (and (not instance?) (seq (get-in condition [:connection/params])))
         (h :p {:class "muted small"}
            "This end overrides " (str/join ", " (map name (keys (:connection/params condition))))
            " (an older template); applying it copies them into the connection.")))))

(defn- regen-messages [errors]
  (messages (map regen-error-text (remove :param errors))))

(defn- assembly-view
  "The page's one view of the member alone, in member-space: main part,
  plates, bolts, holes and cuts as its connections made them, its XLine as a
  thin line with a ball at each node. It opens side-on (the web, start on
  the left) and orbits, pans and zooms; the buttons snap back. Nothing it
  connects to is drawn."
  [preview note & {:keys [length]}]
  (let [{:keys [instructions nodes]} (te/in-member-space preview)
        views (te/preview-views nodes)
        host (preview-host! (some #(when (= "Side" (first %)) (second %)) views))
        refit? (and length (not= length (get-in @ui* [:preview :length])))]
    ;; a new sample length moves the end node: frame the member again
    (when refit? (swap! ui* assoc-in [:preview :length] length))
    (show-preview! instructions :nodes nodes :fit? refit?)
    (h :section {:class "assembly-view"}
       (h :div {:class "row"}
          (h :h3 {:class "grow"} "Assembly")
          (view-buttons views))
       host
       (h :p {:class "muted small"} note " Left drag orbits, right drag pans, the wheel zooms."))))

(defn- assembly-panes
  "The page split in two: the insertion panel on the left; on the right the
  Assembly view, the Start card on its left edge and the End card on its
  right."
  [dlg size size-control preview note errors & view-opts]
  (h :div {:class "dialog-body assembly"}
     (h :div {:class "pane insertion"}
        (main-member-section dlg size size-control))
     (h :div {:class "pane model"}
        (h :div {:class "ends-and-view"}
           (end-card :start dlg)
           (apply assembly-view preview note view-opts)
           (end-card :end dlg))
        (h :div {:class "messages"} (regen-messages errors)))))

(defn- assembly-builder [{:keys [draft size error saving?] :as dlg}]
  (let [length (te/preview-length @lib (:templates @env*) draft size (:support-size dlg))
        {:keys [errors] :as preview} (te/assembly-preview @lib (:templates @env*) draft
                                                          {:size size :support-size (:support-size dlg) :length length})]
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
       (assembly-panes dlg size
                       (h :div {:class "row"} "Preview size "
                          (size-select "size" size #(update-top! assoc :size %)
                                       (sizes-for (:template/role draft))))
                       preview
                       (str "A sample member, on a " (fmt length) " mm XLine (the thin line, a ball at each node), "
                            "just long enough to keep its two end details apart. "
                            "An end with a connection is made against a " (:support-size dlg)
                            " column, which isn't drawn. Saving changes the library only: "
                            "assemblies made from this template keep their own copies.")
                       errors
                       :length length))))

(defn- template-note [{from :assembly/from-template}]
  (if from
    (list "From library template " (h :strong {} (or (:template/name from) (template-name (:template/id from)) "(unnamed)"))
          ": a copy, so library edits don't change it.")
    "Not from a template."))

(defn- apply-row [{:keys [draft apply saving?]}]
  (let [from (get-in draft [:assembly/from-template :template/id])
        candidates (sort-by #(cond (= from (:template/id %)) 0 (= (:xline/role draft) (:template/role %)) 1 :else 2)
                            (templates-of :assembly))
        tid (or (:template-id apply) (:template/id (first candidates)))
        sizes (sizes-for (:xline/role draft))
        size (or (:size apply) (when (string? (:assembly/size draft)) (:assembly/size draft)) (first sizes))]
    (h :div {:class "apply-row"}
       (h :strong {} "Re-apply from library ")
       (select-of "apply-tpl" (str tid) (map (fn [t] [(str (:template/id t)) (:template/name t)]) candidates)
                  (fn [v] (update-top! assoc-in [:apply :template-id]
                                       (some #(when (= v (str (:template/id %))) (:template/id %)) candidates))))
       (thumb (get-in @env* [:templates tid :template/thumbnail]))
       " at "
       (size-select "apply-size" size #(update-top! assoc-in [:apply :size] %) sizes)
       (h :button {:class "on" :disabled (or saving? (nil? tid))
                   :on-click (fn [] (update-top! update :apply assoc :template-id tid :size size) (apply-template!))}
          "Apply")
       (h :button {:on-click #(update-top! dissoc :apply)} "Close")
       (h :span {:class "muted small"}
          "Copies the template as the library has it now: placement and both ends' connections. Unsaved changes here are lost."))))

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
             "Re-apply from library…")
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
       (assembly-panes dlg size
                       (h :div {:class "row"} "Size "
                          (if (map? size)
                            (h :span {:class "muted"} "custom section")
                            (size-select "size" size #(update-draft! assoc :assembly/size %)
                                         (sizes-for role))))
                       (or preview {:instructions {} :nodes []})
                       (str "This assembly alone, as Save would make it, on its " (fmt (:length info))
                            " mm XLine (the thin line, a ball at each node), laid along the view whichever way it runs. "
                            "What its ends sit on isn't drawn.")
                       (:errors preview)))))

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
                   (:connection :conn-instance) (connection-editor dlg)
                   :assembly (assembly-builder dlg)
                   :instance (assembly-instance dlg))])))))

(defn library-section
  "The side panel's template library: both kinds, each with its thumbnail,
  with new, copy and edit."
  []
  (let [rows (fn [kind]
               (for [t (templates-of kind)]
                 (h :tr {}
                    (h :td {:class "thumb-cell"} (thumb (:template/thumbnail t)))
                    (h :td {} (:template/name t))
                    (h :td {:class "tight"}
                       (h :button {:class "small" :on-click #(open! t)} "Edit")
                       (h :button {:class "small" :on-click #(open! (te/duplicate t))} "Copy")))))]
    (h :section {}
       (h :h2 {} "Templates")
       (h :table {:class "library"}
          (h :tr {} (h :th {:colspan 3} "Assembly"))
          (rows :assembly)
          (h :tr {} (h :th {:colspan 3} "Connection"))
          (rows :connection))
       (h :p {:class "muted small"}
          "Templates are starting points: applying one copies it, so editing a template here changes no assembly.")
       (h :div {:class "buttons"}
          (h :button {:on-click #(open! (te/blank :assembly))} "New assembly")
          (h :button {:on-click #(open! (te/blank :connection))} "New connection")))))

(defn templates-changed!
  "New templates from the server: keep open editors' references fresh."
  [templates]
  (swap! env* assoc :templates templates)
  (when (top) (render!)))
