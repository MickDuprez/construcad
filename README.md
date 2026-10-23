# construcad

An XLine-based, semi-parametric steel and construction modeller: a shared
`.cljc` core (`src/modeller`), a Biff web app (`src/construcad*.clj`) and a
three.js editor in the browser (`src/construcad/app`). The design is in the
"Steel modeller core spec" doc.

## Running the editor

Needs the Clojure CLI, Java and Node.js.

    clojure -M:dev dev

On first run this writes `config.env` (dev secrets), runs `npm install`,
starts `shadow-cljs watch app` and the Biff server. Wait for `System
started`, then open http://localhost:8080. nREPL listens on port 7888. To
run shadow-cljs yourself, start the server with `clojure -M:dev dev --no-cljs`
and run `npx shadow-cljs watch app` in another terminal.

XTDB 1 keeps its data in `storage/xtdb`. An empty database is seeded with
the demo bay (four columns, two beams with square-cut connections, a tilt
panel). Stop the server and delete `storage/` to start again from the seed.

In the editor:

- Middle or right drag pans, Shift + middle/right drag orbits, the wheel
  zooms at the cursor. The view cube sets standard views and orbits when
  dragged.
- **Select**, then click a member. Drag a yellow grip to move that end; it
  snaps to XLine ends, grid intersections (at the level the end sits on) and
  stations along XLines, and the snap decides the node reference written.
  Click a grip without dragging to type `@dx,dy,dz` (or absolute `x,y,z`) in
  the command bar. A grid node keeps its grid reference and takes the move as
  an offset.
- **XLine** draws a new member: pick its role under **New XLines are**
  (column, beam, rafter, joist, purlin, girt, brace, panel; beam by default),
  then click a start and an end (same snaps), or type coordinates;
  `@dx,dy,dz` for the end is relative to the start. A beam drawn at one grid
  intersection (both ends on it) is a column. The grid is drawn, and grid
  snaps sit, at the working level ticked in the Levels panel. Esc cancels.
- **Right-click** a member (or **Edit…** under Selection) for **Edit
  assembly**: the assembly page for that member (see Templates). The side
  panel keeps a short summary: mark, role, size, template.
- Level elevations, grid axis offsets and grid label styles are edited in
  the side panel.

## Templates

An **assembly template** is a recipe for an XLine-rooted member: how the main
member sits on its XLine (which of the nine insertion points, an X/Y offset,
a rotation, start and end extensions) and a connection template for each end.
The section size is chosen when the template is applied. A **connection
template** is an ordered list of actions (`:cut-to-face`, `:end-plate`,
`:bolt-group`) and parameters; a parameter is a number or a formula over the
member's section data (`d bf tf tw r1` ...), `support` (the supporting
member's data, e.g. `(:tf support)`) and the other numeric parameters, e.g.
`:plate-h "(+ d 20)"`. A new project starts with a small library (square cut,
end plate with four bolts, and beams and a column built from them).

- The side panel's **Templates** section lists both kinds: **Edit**, **Copy**
  (saves as a new template), **New assembly**, **New connection**.
- The **assembly page** edits an assembly template. It shows the main
  member on its XLine and nothing else: end-on at a preview size (top flange
  up, across the view is the XLine's local X), where you click one of the
  nine points to put it on the XLine (the cross), type the offsets, pick a
  rotation (0/90/180/270 or typed) and the extensions, beside a side
  elevation of the member alone. The **Start** and **End** cards pick a
  connection template (or none) and take per-end overrides of its
  parameters, with a link to the connection editor, which opens on top and
  returns to the page. A card can't know what its end will sit on, so a
  parameter worked out from `support` shows as "from the support".
  `:template/role` ("Usually for") only orders the size pickers: applying a
  template never changes a member's role, which is chosen when it is drawn.
- The **connection template editor** is where a connection is seen. A
  connection template knows nothing of member roles, only the two ECSs it
  joins and how (cut, plate, bolts...), so one template serves a beam into a
  column and a joist into a beam. It orders the actions (add, ↑ ↓, remove),
  edits the parameters (a formula's error shows on its row) and previews an
  owner of one size framing into a support of another, the support either a
  vertical column (the default) or a horizontal beam the owner meets at 90
  degrees, tops flush, both sized from the library.
- Previews regenerate in the browser with `modeller.core` and the in-browser
  section library (`construcad.template-edit`), so nothing goes to the server
  until **Save**, which sends a `:template/save` edit. Every member built from
  the template regenerates, and the patch reaches every open viewer.
- To apply one to many members, select them (Shift + click adds or
  removes), choose the template and the size under **Assembly template**,
  and **Apply** (`:assembly/apply-template`). The supporting member of each
  end is read from the end node; an end that isn't on another XLine gets no
  connection, and the status bar says so.
- **Edit assembly** opens the same page in instance mode for one member,
  loaded from the server (`GET /api/buildings/:id/assemblies/:xid`, which
  runs `modeller.template/assembly-state`). It adds the mark, role and size,
  the XLine's ends, length and rotation, says which template the member came
  from, and each card says what that end sits on and resolves the parameters
  against it. **Save** sends `:assembly/update` (and `:xline/set-rotation`
  if that changed); a hand-edited assembly detaches from its template
  (template edits no longer change it; it remembers where it came from).
  **Apply template…** applies a template at a size to this member instead.

Holes are cut: the reader realises `:subtract` (and `:union`, `:intersect`)
with three-bvh-csg; bolts are cylinders.

Every change goes to the server as a source edit (`construcad.edits`); the
server validates it, saves it in one XTDB transaction, regenerates only the
affected assemblies and sends the patch back, and to every other open
viewer over a websocket. The browser never sends geometry.

| Namespace | What it does |
|---|---|
| `construcad.edits` | source edits: validate, apply to the model map, upsert patch (pure) |
| `construcad.records` | one XTDB document per record, to and from the model map |
| `construcad.seed` | the demo project |
| `construcad.snap` | snap candidates and the node references they write (pure) |
| `construcad.store` | XTDB reads and the edit transaction |
| `construcad.template-edit` | template drafts, typed parameters, end-on and side views of the member, in-browser preview models, assembly page edits (pure) |
| `construcad.api` | EDN endpoints (buildings, view, templates, one assembly, edits) and the patch websocket |
| `construcad.app.reader` | instructions to three.js; `construcad.app.slice` cuts and caps meshes |
| `construcad.app.viewport` | camera, navigation, view cube, hidden-line display, picking |
| `construcad.app.templates` | template library panel, assembly page (templates and single assemblies), connection editor |
| `construcad.app.mini-view` | the small 3D preview view in the template editors |
| `construcad.app.main` | panel, XLine tool, grips, selection, context menu, applying templates, server round trip |

## Core

| Namespace | What it does |
|---|---|
| `modeller.vec` | vectors and the tolerance policy |
| `modeller.frame` | rigid frames (ECS), XLine frame rule, compose/invert |
| `modeller.grid` | grids, levels, grid points, reading a point back against the grid |
| `modeller.node` | node references (absolute, grid, coincident, on-line, relative) |
| `modeller.model` | dependency-ordered XLine resolution, affected-set calculation |
| `modeller.profile` | outline loops with arc bulges, round-corners, tessellation, area, nine insertion points, face planes |
| `modeller.profile-builder` | builds outlines; SCI-sandboxed scripts for custom extrusions |
| `modeller.library` | compiled builders + section records with stored outlines |
| `modeller.shapes` | standard shape builders: rect, I, channel, angle, RHS/SHS, CHS |
| `modeller.sections-au` | 120 AU hot rolled sections from the ASI 8th edition tables (UB, UC, UBP, PFC, EA, UA), plus placeholder hollow sections |
| `modeller.instr` | the reader instruction algebra |
| `modeller.assembly` | parts to instructions, connection actions (`:cut-to-face`, `:end-plate`, `:bolt-group`) |
| `modeller.params` | template parameters: numbers or SCI formulas over section data |
| `modeller.template` | assembly templates: apply to an XLine at a size, re-apply where used |
| `modeller.core` | `regenerate` and `edit` entry points |

## Tests

    clojure -M:test            # JVM: core, edits, snapping
    npm i && npm run test:cljs # node via shadow-cljs: the same, plus mesh slicing and booleans
