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
  - the connection editor: the connection's parameters (numbers or
    formulas) and its tree of parts (cuts, plates, gussets, hole groups,
    modeller.prim), each edited with the placement they all share and its
    type's own fields, and a 3D preview picking out the selected part and
    its ECS. In library mode it edits a library template,
    previewed on a sample owner framing into a sample support (a vertical
    column or a horizontal beam); Save sends :template/save with a small
    thumbnail of the preview. In instance mode it edits one assembly's own
    copy of a connection, previewed on the real member and what it sits on;
    Save sends :connection/update, Save as new library template… puts a
    copy in the library, and Change connection… swaps the draft for a copy
    of another library template;
  - the library picker, with thumbnails, behind Change connection… on the
    assembly page's end cards and in the instance connection editor.

  Previews regenerate in the browser with the real core and the in-browser
  section library (construcad.template-edit); only Save goes to the server.
  Editors stack: an end's connection opens over the assembly page and
  returns to it."
  (:require [clojure.string :as str]
            [construcad.app.dom :as dom :refer [h s]]
            [construcad.app.mini-view :as mini-view]
            [construcad.template-edit :as te]
            [modeller.connection :as connection]
            [modeller.library :as library]
            [modeller.prim :as prim]
            [modeller.sections-au :as au]
            [modeller.settings :as settings]
            [modeller.template :as template]))

(defonce lib (delay (library/library {} au/rows)))

;; :templates {id t} and :sections [{:section/designation ..}] come from the
;; server (GET /templates, and patches). :send! sends one edit and returns a
;; promise of true when the server took it; :load-assembly! returns a promise
;; of an assembly's page data (construcad.edits/assembly-page), or nil.
(defonce env* (atom {:templates {} :sections [] :send! nil :load-assembly! nil
                     ;; the project's detailing settings (construcad.edits/settings-view)
                     :settings-view {:settings settings/defaults :stored {} :fields settings/fields}}))

(defn effective-settings [] (get-in @env* [:settings-view :settings] settings/defaults))
(defn stored-settings [] (get-in @env* [:settings-view :stored] {}))

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
  "Open an editor for a template map (an existing one, a duplicate or a
  blank). A connection template saved as an action list opens as its tree."
  [draft]
  (let [draft (te/as-draft draft)
        kind (:template/kind draft)]
    (swap! dlg* update :stack conj (merge {:kind kind :draft draft :arrangement :column
                                           :selected (:prim/id (first (:template/parts draft)))}
                                          (default-sizes kind draft)))
    (render!)))

(defn open-connection-instance!
  "Open the connection editor in instance mode on one end of an assembly:
  `page` is its page data (construcad.edits/assembly-page, or the assembly
  page's own draft state with the same context), `end` :start or :end.
  From the assembly page (`from-page?`), Save also puts the copy into the
  page's draft. `copy` (a connection copy) opens in place of the end's
  own, to replace it (Change connection…), with `note` saying so."
  [{:keys [state context supports]} end & {:keys [from-page? copy note]}]
  (when-let [copy (or copy (get state end))]
    (swap! dlg* update :stack conj
           {:kind :conn-instance :draft (template/copy->template copy)
            :from (:connection/from-template copy) :connection-id (:connection/id copy)
            :end end :state state :context context :supports supports :from-page? from-page?
            :selected (:prim/id (first (prim/connection-parts copy))) :note note})
    (render!)))

(defn open-picker!
  "Open the library picker over the open editor: the connection templates
  with their thumbnails (and None, with `none?`). `on-pick` gets the chosen
  template, or nil for None, once the picker has closed."
  [{:keys [title on-pick none? current]}]
  (swap! dlg* update :stack conj {:kind :picker :title title :on-pick on-pick :none? none? :current current})
  (render!))

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
            ;; the thumbnail without the selected part picked out
            (-> (do (update-top! assoc :selected nil)
                    ((:send! @env*) {:edit/type :template/save :template (with-thumbnail draft)}))
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
      (do (update-top! assoc :error nil :saving? true :selected nil)
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
  "A regeneration error from a preview, in words where we know it, named
  by the part of the connection (in `parts`) it came from."
  [parts e]
  (let [p (when (:prim/id e) (prim/find-prim parts (:prim/id e)))
        who (when (:prim/id e) (str (or (not-empty (:prim/name p)) (name (:prim/id e))) ": "))]
    (str who
         (case (:error/type e)
           :prim/missing-input (case (:missing e)
                                 :target "there is no such part to cut (a cut of its parent needs a plate or gusset above it)."
                                 :through "nothing to drill through: a parent plate or gusset, the support or the member."
                                 "something it needs isn't there.")
           :prim/no-anchor (str "its anchor (" (some-> (:at e) name) ") isn't there"
                                (when (= :parent (:at e)) ": a top-level part has no parent") ".")
           :prim/hole-along-face "its holes run along a face they should go through; turn or flip it."
           :prim/unknown-type (str "unknown part type " (some-> (:prim/type e) name) ".")
           :prim/unknown-param (str "no parameter " (some-> (:param e) name) ".")
           :prim/bad-value (str "can't read " (pr-str (:value e)) " as a value.")
           :param/formula-failed (str (:formula e) ": " (:message e))
           :param/not-a-number (str (:formula e) " does not give a number.")
           :part/unknown-profile (str "Unknown section " (pr-str (:profile e)) ".")
           (pr-str e)))))

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

;; --- connection editor: the parts tree --------------------------------------------------

(def type-labels (into {} (map (juxt first second)) connection/types))
(def type-glyphs {:cut "⟋" :plate "▭" :gusset "◺" :hole-group "⁘"})

(defn- type-label [t] (get type-labels t (prim/type-label t)))

(defn- update-parts! [f & args] (update-draft! #(apply update % :template/parts f args)))
(defn- update-prim! [id f & args] (apply update-parts! prim/update-prim id f args))
(defn- select-prim! [id] (update-top! assoc :selected id))

(defn- add-part! [parent-id type]
  (let [{:keys [draft id]} (te/add-part (:draft (top)) parent-id type)]
    (update-top! assoc :draft draft :selected id)))

(defn- remove-part! [id]
  (let [parent (prim/parent-of (:template/parts (:draft (top))) id)]
    (update-top! #(-> % (update-in [:draft :template/parts] prim/remove-prim id) (assoc :selected parent)))))

(defn- parts-tree
  "The connection's parts as a tree: click one to edit it (and to pick it
  out in the preview), move it among its siblings or remove it, and add new
  parts at the top level or under the selected one."
  [parts selected bad]
  (let [sel (prim/find-prim parts selected)
        add-buttons (fn [parent-id]
                      (for [[t label] connection/types]
                        (h :button {:class "small" :title (str "Add a " (str/lower-case label))
                                    :on-click #(add-part! parent-id t)}
                           (str "+ " label))))]
    (list
     (h :ul {:class "parts-tree"}
        (if (empty? parts)
          (h :li {:class "muted"} "No parts yet.")
          (for [[p depth] (prim/depth-list parts)
                :let [id (:prim/id p)]]
            (h :li {:class (str (when (= id selected) "selected") (when (bad id) " bad"))
                    :style (str "padding-left:" (+ 6 (* 16 depth)) "px")
                    :title (str (type-label (:prim/type p)) " (" (name id) ")")
                    :on-click #(select-prim! id)}
               (h :span {:class "prim-icon"} (get type-glyphs (:prim/type p) "•"))
               (h :span {:class "prim-name"} (or (not-empty (:prim/name p)) (name id)))
               (h :span {:class "muted small"} (type-label (:prim/type p)))))))
     (when sel
       (h :div {:class "buttons small"}
          (h :button {:title "Move up among its siblings" :on-click #(update-parts! prim/move-prim selected -1)} "↑")
          (h :button {:title "Move down among its siblings" :on-click #(update-parts! prim/move-prim selected 1)} "↓")
          (h :button {:title "Remove it and its children" :on-click #(remove-part! selected)} "✕ Remove")))
     (h :div {:class "add-parts"}
        (h :span {:class "label"} "Add to the connection")
        (add-buttons nil))
     (when sel
       (h :div {:class "add-parts"}
          (h :span {:class "label"} (str "Add to " (or (not-empty (:prim/name sel)) (name selected))))
          (add-buttons selected))))))

;; --- connection editor: one part --------------------------------------------------------

(defn- value-field
  "A prim value: a number, a parameter name or a formula."
  [k v on-commit & [attrs]]
  (text-field k (te/value-text v) #(on-commit (te/parse-param %))
              (merge {:class (str "val " (cond (number? v) "number" (keyword? v) "name" :else "formula"))
                      :placeholder "number, name or formula"}
                     attrs)))

(defn- check [k label checked? on-change]
  (h :label {:class "check"}
     (h :input {:type "checkbox" :data-k k :checked (boolean checked?)
                :on-change #(on-change (.. % -target -checked))})
     label))

(def anchor-options
  [[:node "Node (the member's XLine end)"] [:support-face "Support face"]
   [:owner-face "Member's own face"] [:parent "Parent part"]])

(def anchor-help
  {:node "Z runs along the member's XLine into the member, Y is the XLine's Y (up for a beam)."
   :support-face "On the member's centre line, at the support's face; Z out of the support, towards the member; Y up."
   :owner-face "On a face of the member's own section, at the node; Z out of that face."
   :parent "The parent part's ECS: a plate's or gusset's own plane, Z through its thickness."})

(def face-labels
  {:auto "the face it runs into" :top "top" :bottom "bottom" :left "left" :right "right"
   :web-left "web, left" :web-right "web, right" :flange-top-inner "top flange, inside"
   :flange-bottom-inner "bottom flange, inside"})

(defn- placement-section
  "Where a part starts, shared by every type: its anchor, an offset in the
  anchor's ECS, a rotation about its Z and a flip."
  [{id :prim/id :as p} child?]
  (let [place (merge {:at (if child? :parent :support-face)} (:prim/place p))
        set-place! (fn [k v] (update-prim! id #(if (nil? v)
                                                 (update % :prim/place dissoc k)
                                                 (assoc-in % [:prim/place k] v))))
        [dx dy dz] (:offset place [0 0 0])
        set-offset! (fn [i v] (set-place! :offset (assoc (vec (:offset place [0 0 0])) i (or v 0))))
        rot (:rotation place 0)
        at (:at place)
        faces (case at
                :support-face connection/face-names
                :owner-face (remove #{:auto} connection/face-names)
                nil)]
    (h :section {}
       (h :h3 {} "Placement")
       (h :table {:class "placement"}
          (h :tr {} (h :th {} "Anchor")
             (h :td {}
                (select-of (str "at-" (name id)) (name at)
                           (map (fn [[k l]] [(name k) l]) (if child? anchor-options (butlast anchor-options)))
                           ;; a face name belongs to one anchor
                           #(update-prim! id update :prim/place (fn [pl] (-> pl (dissoc :face) (assoc :at (keyword %))))))))
          (when faces
            (h :tr {} (h :th {} "Face")
               (h :td {}
                  (select-of (str "face-" (name id)) (name (or (:face place) (if (= :owner-face at) :web-right :auto)))
                             (map (fn [k] [(name k) (face-labels k (name k))]) faces)
                             #(set-place! :face (keyword %))))))
          (h :tr {} (h :th {} "Offset X, Y, Z")
             (h :td {:class "triple"}
                (value-field (str "dx-" (name id)) dx #(set-offset! 0 %))
                (value-field (str "dy-" (name id)) dy #(set-offset! 1 %))
                (value-field (str "dz-" (name id)) dz #(set-offset! 2 %))))
          (h :tr {} (h :th {} "Rotation about Z")
             (h :td {}
                (h :div {:class "buttons small"}
                   (for [a [0 90 180 270]]
                     (h :button {:class (when (and (number? rot) (== a rot)) "on")
                                 :on-click #(set-place! :rotation a)} (str a "°"))))
                (value-field (str "rot-" (name id)) rot #(set-place! :rotation %))))
          (h :tr {} (h :th {} "Flip")
             (h :td {} (check (str "flip-" (name id)) "a half turn about X (Z the other way)" (:flip? place)
                              #(set-place! :flip? (when % true))))))
       (h :p {:class "muted small"} (anchor-help at) " Then the offset (in that ECS), the rotation and the flip, in that order."))))

(defn- param-row
  "One of a prim's own values, in a table row. Blank, it shows what it
  takes from the project's detailing settings (\"90 from settings\"), else
  `placeholder`, the built-in default."
  [{id :prim/id :as p} k label & [{:keys [placeholder help]}]]
  (let [inherited (settings/placeholder (effective-settings) p k)]
    (h :tr {}
       (h :th {} label)
       (h :td {}
          (value-field (str (name k) "-" (name id)) (get-in p [:prim/params k])
                       #(update-prim! id (fn [q] (if (nil? %) (update q :prim/params dissoc k) (assoc-in q [:prim/params k] %))))
                       (when-let [ph (or inherited placeholder)]
                         {:placeholder ph :class (str "val " (when inherited "inherits ")
                                                      (let [v (get-in p [:prim/params k])]
                                                        (cond (number? v) "number" (keyword? v) "name" :else "formula")))}))
          (when help (h :span {:class "muted small"} " " help))))))

(defn- set-param! [id k v]
  (update-prim! id #(if (nil? v) (update % :prim/params dissoc k) (assoc-in % [:prim/params k] v))))

(defmulti type-fields "The fields of one prim type." (fn [p _child?] (:prim/type p)))

(defmethod type-fields :default [p _]
  (h :p {:class "muted small"} (str "No editor for " (name (:prim/type p)) " parts yet.")))

(defmethod type-fields :cut [{id :prim/id :as p} child?]
  (let [{:keys [target plane]} (:prim/params p)]
    (h :table {:class "placement"}
       (h :tr {} (h :th {} "Cuts")
          (h :td {} (select-of (str "target-" (name id)) (name (or target :owner-main))
                               (cond-> [["owner-main" "the member"]] child? (conj ["parent" "its parent part"]))
                               #(set-param! id :target (when (not= "owner-main" %) (keyword %))))))
       (h :tr {} (h :th {} "With the plane")
          (h :td {} (select-of (str "plane-" (name id)) (name (or plane :xy))
                               [["xy" "XY (square to Z)"] ["xz" "XZ (square to Y)"] ["yz" "YZ (square to X)"]]
                               #(set-param! id :plane (when (not= "xy" %) (keyword %))))))
       (param-row p :gap "Gap" {:help "along the plane's normal"})
       (h :tr {} (h :td {:colspan 2 :class "muted small"} "Keeps the side the plane's normal points to: for XY at the node or the support face, the member's side.")))))

(defn- shape-fields [{id :prim/id :as p} default]
  (let [shape (get-in p [:prim/params :shape] default)
        polygon? (contains? shape :polygon)
        [sw sh] (:rect shape)
        set-shape! #(set-param! id :shape %)]
    (list
     (h :tr {} (h :th {} "Shape")
        (h :td {} (select-of (str "shape-" (name id)) (if polygon? "polygon" "rect")
                             [["rect" "Rectangle, centred"] ["polygon" "Polygon"]]
                             #(set-shape! (if (= "polygon" %)
                                            {:polygon (or (:polygon shape) [[0 0] [100 0] [0 100]])}
                                            {:rect (or (:rect shape) ["bf" "d"])})))))
     (if polygon?
       (h :tr {} (h :th {} "Points x,y")
          (h :td {} (text-field (str "poly-" (name id)) (te/polygon-text (:polygon shape))
                                #(if-let [pts (te/parse-polygon %)] (set-shape! {:polygon pts}) (render!))
                                {:class "val formula wide" :placeholder "0,0  100,0  0,100"})))
       (list
        (h :tr {} (h :th {} "Width (X)")
           (h :td {} (value-field (str "w-" (name id)) sw #(set-shape! {:rect [% sh]}) {:placeholder "bf"})))
        (h :tr {} (h :th {} "Height (Y)")
           (h :td {} (value-field (str "h-" (name id)) sh #(set-shape! {:rect [sw %]}) {:placeholder "d"}))))))))

(defmethod type-fields :plate [p _]
  (h :table {:class "placement"}
     (shape-fields p nil)
     (param-row p :t "Thickness" {:help "along +Z"})))

(defmethod type-fields :gusset [p _]
  (h :table {:class "placement"}
     (param-row p :w "Leg along X" {:placeholder "100"})
     (param-row p :h "Leg along Z" {:placeholder "100"})
     (param-row p :t "Thickness" {:help "centred on the XZ plane"})
     (h :tr {} (h :td {:colspan 2 :class "muted small"}
                  "A triangle standing on its ECS's XY plane, its legs along X and Z: on a plate's face, turn it with the rotation."))))

(defmethod type-fields :hole-group [{id :prim/id :as p} child?]
  (let [through (set (get-in p [:prim/params :through] [:parent]))
        set-through! (fn [k on?] (set-param! id :through (vec (filter (if on? (conj through k) (disj through k))
                                                                      [:parent :support :owner-main]))))
        bolts? (get-in p [:prim/params :bolts?])]
    (h :table {:class "placement"}
       (param-row p :rows "Rows (up Y)" {:placeholder "2"})
       (param-row p :cols "Columns (across X)" {:placeholder "2"})
       (param-row p :pitch "Pitch")
       (param-row p :gauge "Gauge")
       (param-row p :bolt-dia "Bolt diameter")
       (param-row p :dia "Hole diameter")
       (param-row p :overrun "Cutter overrun" {:help "past each face"})
       (h :tr {} (h :th {} "Through")
          (h :td {}
             (when child? (check (str "thr-parent-" (name id)) "its parent" (through :parent) #(set-through! :parent %)))
             (check (str "thr-support-" (name id)) "the support" (through :support) #(set-through! :support %))
             (check (str "thr-owner-" (name id)) "the member" (through :owner-main) #(set-through! :owner-main %))))
       (h :tr {} (h :th {} "Bolts")
          (h :td {} (check (str "bolts-" (name id)) "bolts in the holes" bolts? #(set-param! id :bolts? (when % true)))))
       (when bolts? (param-row p :bolt-length "Bolt length"))
       (h :tr {} (h :td {:colspan 2 :class "muted small"}
                    "Holes run along Z, centred on the ECS. The support's holes are drilled in its own assembly.")))))

(defn- prim-editor
  "The selected part: its name, the placement every type shares, then its
  type's own fields, and what went wrong with it."
  [parts selected messages]
  (if-let [{id :prim/id :as p} (prim/find-prim parts selected)]
    (let [child? (some? (prim/parent-of parts id))]
      (h :div {:class "prim-editor"}
         (h :div {:class "row"}
            (h :strong {} (type-label (:prim/type p)))
            (h :span {:class "muted small"} (str "(" (name id) ")")))
         (text-field (str "pname-" (name id)) (:prim/name p) #(update-prim! id assoc :prim/name %)
                     {:class "name-field" :placeholder "name"})
         (when (seq messages) (h :ul {:class "errors"} (for [m messages] (h :li {} m))))
         (placement-section p child?)
         (h :section {}
            (h :h3 {} (type-label (:prim/type p)))
            (type-fields p child?))))
    (h :p {:class "muted"} "Select a part in the tree to edit it, or add one.")))

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

(defn- change-connection!
  "Replace the instance connection editor's draft with a copy of library
  template `t`, which Save then makes this end's own."
  [t]
  (when t
    (let [copy (template/connection-copy (:templates @env*) {:connection/template (:template/id t)})]
      (update-top! assoc :draft (template/copy->template copy) :from (:connection/from-template copy)
                   :selected nil :error nil
                   :note (str "Replaced with a copy of " (:template/name t) ": Save to keep it.")))))

(defn- params-flyout
  "The connection's shared parameters, behind Parameters… in the header:
  named values two or more parts use (a plate's thickness that the cut
  behind it also needs), each a number or a formula."
  [draft resolved]
  (h :div {:class "params-flyout"}
     (h :div {:class "row"}
        (h :strong {} "Shared parameters")
        (h :span {:class "grow"})
        (h :button {:class "small" :title "Close" :on-click #(update-top! assoc :params-open? false)} "✕"))
     (params-table (:template/params draft) resolved
                   {:on-rename #(update-draft! update :template/params te/rename-param %1 %2)
                    :on-set #(update-draft! update :template/params te/set-param %1 %2)
                    :on-remove #(update-draft! update :template/params te/remove-param %)})
     (h :p {:class "muted small"}
        "Only for values several parts share: put the rest on the part, and leave a part's field blank to take the "
        "project's detailing settings (toolbar, Settings). A part uses one by its name, e.g. " (h :code {} "plate-t") ".")
     (h :p {:class "muted small"}
        "Formulas see the member's section data (" (h :code {} "d bf tf tw r1") "…), "
        (h :code {} "support") " (the supporting member's, e.g. " (h :code {} "(:tf support)") "), "
        (h :code {} "settings") " (e.g. " (h :code {} "(+ (:gauge settings) 10)") ") and the numeric parameters; "
        "a part's values also see the formula parameters.")))

(defn- connection-editor
  "The connection editor, fixed to the window: the parts tree on the left
  and the selected part's editor (its placement, then its type's own
  fields) beside it, each scrolling on its own, and the preview on the
  right (above them on a narrower window), the selected part picked out in
  it with its ECS. The connection's shared parameters sit behind
  Parameters… in the header. Library mode (:connection) edits a library
  template against a sample owner and support; instance mode
  (:conn-instance) edits one assembly's own copy against the real member
  and what that end sits on."
  [{:keys [kind draft arrangement error note saving? from end state context supports selected params-open?] :as dlg}]
  (let [instance? (= :conn-instance kind)
        parts (:template/parts draft)
        support-id (get-in state [:supports end])
        size (if instance? (:assembly/size state) (:size dlg))
        support-size (if instance? (or (get-in supports [end :size]) (size-of context support-id)) (:support-size dlg))
        context (when context (assoc context :settings (stored-settings)))
        ;; the owner and support as short stubs about the connection
        preview-of (fn [arr]
                     (if instance?
                       (te/stub-preview (te/connection-instance-preview @lib (:templates @env*) context state end draft)
                                        (:xline/id state) end support-id)
                       (te/stub-preview (te/connection-preview @lib (:templates @env*) draft
                                                               {:size size :support-size support-size :arrangement arr
                                                                :settings (stored-settings)})
                                        :owner :end :support)))
        {:keys [instructions errors nodes detail stubs]} (preview-of arrangement)
        resolved (te/resolve-for @lib draft {} (when (string? size) size) support-size (stored-settings))
        iso (if instance? (second (first (te/preview-views nodes))) [1 -1.4 0.9])
        host (preview-host! iso)
        owner-id (if instance? (:xline/id state) :owner)
        show! (fn [instructions detail & opts]
                (apply show-preview! instructions
                       :highlight (when-let [p (get-in detail [:outputs selected])] {:assembly owner-id :part p})
                       :axes (when-let [f (get-in detail [:frames selected])] [f])
                       :cut (when (= :cut (:prim/type (prim/find-prim parts selected))) {:assembly owner-id :prim selected})
                       opts))
        prim-errors (filter :prim/id errors)
        other-errors (remove #(or (:param %) (:prim/id %)) errors)
        bad (set (map :prim/id prim-errors))
        n-params (count (:template/params draft))]
    ;; framed again when the stubs change length (another size, a longer gusset)
    (let [refit? (and stubs (not= stubs (get-in @ui* [:preview :stubs])))]
      (when refit? (swap! ui* assoc-in [:preview :stubs] stubs))
      (show! instructions detail :fit? refit?))
    (h :div {:class "dialog connection-editor"}
       (h :header {}
          (h :h2 {} (cond
                      instance? (str (:assembly/mark state) ", " (if (= :start end) "start" "end") " connection")
                      (:template/id draft) "Connection template"
                      :else "New connection template"))
          (text-field "name" (:template/name draft) #(update-draft! assoc :template/name %) {:class "name"})
          (h :div {:class "flyout-anchor"}
             (h :button {:class (str (when params-open? "on") (when (seq (:errors resolved)) " bad"))
                         :title "The connection's shared parameters: values several parts use"
                         :on-click #(update-top! update :params-open? not)}
                "Parameters… " (h :span {:class "badge"} (str n-params)))
             (when params-open? (params-flyout draft resolved)))
          (h :span {:class "grow"})
          (when instance?
            (h :button {:disabled saving? :title "Replace this end's connection with a copy of a library template"
                        :on-click #(open-picker! {:title "Change connection" :on-pick change-connection!
                                                  :current (:template/id from)})}
               "Change connection…"))
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
       (h :div {:class "dialog-body conn-layout"}
          ;; the tree keeps its scroll across re-renders; the part's editor
          ;; is keyed by the part, so picking another starts at its top
          (h :div {:class "tree-area pane" :data-scroll "tree"}
             (h :h3 {} "Parts")
             (parts-tree parts selected bad)
             (h :p {:class "muted small"}
                "A connection is built from simple parts: cuts, plates, gussets and hole groups, each placed from an anchor. "
                "A child is placed from its parent and works on it (holes through a plate, a snipe off it)."))
          (h :div {:class "editor-area pane" :data-scroll (str "part-" (some-> selected name))}
             (prim-editor parts selected
                          (map #(regen-error-text parts %) (filter #(= selected (:prim/id %)) prim-errors))))
          (h :div {:class "preview-area"}
             (if instance?
               (h :p {:class "muted small"}
                  (str (:assembly/mark state) (when (string? size) (str ", " size)) " into "
                       (get-in supports [end :assembly/mark] "its support")
                       (when support-size (str ", " support-size))
                       ", where they are in the model, as Save would make them, cut short about the connection."))
               (h :div {:class "preview-setup"}
                  (h :label {} (h :span {:class "muted small"} "Owner ")
                     (size-select "size" size #(update-top! assoc :size %)))
                  (h :label {} (h :span {:class "muted small"} "into ")
                     (size-select "support-size" support-size #(update-top! assoc :support-size %)))
                  (h :label {} (h :span {:class "muted small"} "as ")
                     (select-of "arrangement" (name arrangement)
                                (map (fn [[k l]] [(name k) l]) te/arrangements)
                                #(let [arr (keyword %)
                                       {:keys [instructions detail stubs]} (preview-of arr)]
                                   (swap! ui* assoc-in [:preview :stubs] stubs)
                                   (update-top! assoc :arrangement arr)
                                   (show! instructions detail :fit? true))))))
             (h :div {:class "row"}
                (view-buttons (if instance?
                                (te/preview-views nodes)
                                [["Iso" [1 -1.4 0.9]] ["Plan" [0 0 1]] ["Side" [0 -1 0]] ["End" [-1 0 0]]]))
                (h :span {:class "muted small"}
                   (if instance?
                     "Selected part orange; ECS X red, Y green, Z blue."
                     "A sample owner and support; saving changes the library only. Selected part orange; ECS X red, Y green, Z blue.")))
             host
             (messages (concat (map #(regen-error-text parts %) other-errors)
                               (map #(regen-error-text parts %) (remove #(= selected (:prim/id %)) prim-errors)))))))))

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

(defn- picker
  "The library picker: each connection template with its thumbnail and its
  parts, and None (a plain end) where an end may have no connection."
  [{:keys [title on-pick none? current]}]
  (let [pick! (fn [t] (close!) (on-pick t))]
    (h :div {:class "dialog tpl-picker"}
       (h :header {}
          (h :h2 {} title)
          (h :span {:class "grow"})
          (h :button {:on-click close!} "Cancel"))
       (h :div {:class "dialog-body"}
          (h :p {:class "muted small"} "The end gets its own copy of the template you pick: later library edits don't change it.")
          (h :div {:class "pick-grid"}
             (when none?
               (h :button {:class (str "pick-card" (when (and none? (nil? current)) " current")) :on-click #(pick! nil)}
                  (h :div {:class "thumb blank"} "—")
                  (h :strong {} "None")
                  (h :span {:class "muted small"} "A plain end, no connection")))
             (for [t (templates-of :connection)]
               (h :button {:class (str "pick-card" (when (= current (:template/id t)) " current")) :on-click #(pick! t)}
                  (or (thumb (:template/thumbnail t)) (h :div {:class "thumb blank"} "no preview yet"))
                  (h :strong {} (:template/name t))
                  (h :span {:class "muted small"}
                     (str/join " · " (map type-label (prim/summary (prim/template-parts t))))))))))))

(defn- end-card
  "One end, on the edge of the Assembly view it belongs to: the
  connection's name (with its library template's thumbnail), Edit
  connection… and Change connection…. An assembly template's end names a
  library connection template: Edit opens it in the library, Change picks
  another (or none). An assembly's end is its own copy: Edit opens it in
  instance mode against the real member and what it sits on, and Change
  opens the same editor on a fresh copy of the template picked, which Save
  makes this end's own; None leaves the end plain when the page is saved."
  [end {:keys [kind draft supports context] :as dlg}]
  (let [k (end (paths kind))
        instance? (= :instance kind)
        support (get supports end)
        condition (get draft k)
        info (te/end-info (:templates @env*) condition)
        set-condition! (fn [c] (update-draft! #(if c (assoc % k c) (dissoc % k))))
        page {:state draft :context context :supports supports}
        edit! (fn [e]
                (.preventDefault e)
                (if instance?
                  (open-connection-instance! page end :from-page? true)
                  (when-let [t (get-in @env* [:templates (:connection/template condition)])] (open! t))))
        change! (fn []
                  (open-picker!
                   {:title (str (if (= :start end) "Start" "End") " connection")
                    :none? (some? condition)
                    :current (if instance? (get-in info [:from :template/id]) (:connection/template condition))
                    :on-pick (fn [t]
                               (cond
                                 (nil? t) (set-condition! nil)
                                 (not instance?) (set-condition! {:connection/template (:template/id t)})
                                 :else (open-connection-instance!
                                        page end :from-page? true
                                        :copy (cond-> (template/connection-copy (:templates @env*)
                                                                                {:connection/template (:template/id t)})
                                                ;; it replaces the connection this end has
                                                (:connection/id condition) (assoc :connection/id (:connection/id condition)))
                                        :note (str "A fresh copy of " (:template/name t) ": Save to make it this end's connection."))))}))]
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
                     (h :div {:class "muted small"} (str/join " · " (map type-label (:parts info))))
                     (when (or (not instance?) support)
                       (h :a {:href "#" :class "edit-link" :on-click edit!} "Edit connection…"))))
       (when (or (not instance?) support)
         (h :div {:class "row"}
            (h :button {:class "small" :on-click change!
                        :title "Pick a library connection template for this end (or none)"}
               (if condition "Change connection…" "Add a connection…"))))
       (when (and (not instance?) (seq (get-in condition [:connection/params])))
         (h :p {:class "muted small"}
            "This end overrides " (str/join ", " (map name (keys (:connection/params condition))))
            " (an older template); applying it copies them into the connection.")))))


(defn- regen-messages [errors]
  (messages (map #(regen-error-text nil %) (remove :param errors))))

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
  (let [length (te/preview-length @lib (:templates @env*) draft size (:support-size dlg) (stored-settings))
        {:keys [errors] :as preview} (te/assembly-preview @lib (:templates @env*) draft
                                                          {:size size :support-size (:support-size dlg) :length length
                                                           :settings (stored-settings)})]
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
        preview (when context (te/instance-preview @lib (:templates @env*) (assoc context :settings (stored-settings)) draft))]
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
      (let [scrolled (dom/scroll-positions root)]
        (set! (.. root -style -display) "flex")
        (dom/replace-keeping-focus!
         root [(case (:kind dlg)
                 (:connection :conn-instance) (connection-editor dlg)
                 :picker (picker dlg)
                 :assembly (assembly-builder dlg)
                 :instance (assembly-instance dlg))])
        ;; panes keep their place while you type or pick in them
        (dom/restore-scroll! root scrolled)))))

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

(defn settings-changed!
  "The project's detailing settings from the server (construcad.edits/
  settings-view): open editors show and preview with them."
  [view]
  (swap! env* assoc :settings-view view)
  (when (top) (render!)))

(defn templates-changed!
  "New templates from the server: keep open editors' references fresh."
  [templates]
  (swap! env* assoc :templates templates)
  (when (top) (render!)))
