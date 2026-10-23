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

In the editor: a toolbar across the top, the navigator on the left, the
model in the middle and the properties of the selected assembly on the right.

- **Navigator** (left): **Project** and **Building** pickers load a building
  (`GET /api/projects` lists every project with its buildings; there are no
  accounts yet, so every project is listed), a **Level** filter ("All
  levels" by default) and a search box (mark or size). Below them the tree:
  building > level > role (Columns, Beams, Rafters...) > assembly mark, its
  size shown faintly. An assembly is listed under the level its start node
  is at or nearest below (`construcad.navigator`). The Level filter filters
  the tree and hides the other levels' assemblies in the model (hidden ones
  can't be picked). Click an assembly to select it (Shift + click adds or
  removes); selecting one in the model opens the tree down to it and
  highlights it. **Double-click** it, or right-click > **Edit assembly**,
  to open its assembly page.
- **Properties** (right, collapsible with **›**): mark, role, size, level,
  length, the library template it came from ("from library: ..."), **Edit
  assembly**, each end's connection by name, the library template it was
  copied from and what it sits on, with **Edit start/end connection…**
  (the connection editor in instance mode, on this assembly's own copy,
  previewed on the real member and its support), and the parts list (main
  member, each end's plate and bolts with sizes and counts). The ends come
  from the assembly's page data, fetched when it is selected. **Apply a
  template** for the selection sits below.
- **Toolbar**: Select and XLine tools, the role new XLines get, display mode,
  XLines on/off, zoom extents, and **Levels & grid** and **Templates**, which
  open over the model.
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
  `@dx,dy,dz` for the end is relative to the start. The member always gets
  the picked role, whichever way it runs. The grid is drawn, and grid
  snaps sit, at the working level ticked under **Levels & grid**. Esc cancels.
- **Right-click** a member in the model for **Edit assembly**: the assembly
  page for that member (see Templates).
- Level elevations, grid axis offsets and grid label styles are edited under
  **Levels & grid**.

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

**Templates are starting points, copied and never linked.** Applying an
assembly template copies its placement into the assembly, and each end's
connection template into that end's connection: its own name, actions and
parameters (`:connection/name`, `:connection/actions`, `:connection/params`).
Both note where they came from (`:assembly/from-template`,
`:connection/from-template`: the template's id and name), and nothing more.
So saving a library template changes no assembly, and editing an assembly's
connection changes neither the library nor any other assembly. To take a
library change into an assembly, use **Re-apply from library…** on its page,
which copies the template as it is now.

- The toolbar's **Templates** lists both kinds, each with a thumbnail of its
  preview: **Edit**, **Copy** (saves as a new template), **New assembly**,
  **New connection**.
- The **assembly page** is split in two. On the left, the insertion panel:
  the member end-on at the page's size (top flange up, across the view is the
  XLine's local X), where you click one of the nine points to put it on the
  XLine (the cross), the offsets, a rotation (0/90/180/270 or typed) and the
  extensions. On the right, the 3D **Assembly** view of the member and its
  own parts (plates, bolts, holes, end cuts), its XLine as a thin line with a
  ball at each node, laid along the view whichever way the member runs and
  opening side-on (looking at the web, start on the left); drag to orbit,
  pan and zoom, and **Iso**, **Plan**, **Side** and **End** snap back. The
  **Start** card sits on the view's left edge and the **End** card on its
  right, each showing just the end's connection by name (with the library
  template's thumbnail) and **Edit connection…**. Everything stacks on a
  narrow window.
- **Template mode** (from the Templates panel) edits an assembly template on
  a virtual sample member, its ends made against sample columns (not drawn).
  An end card picks a library connection template (or none); **Edit
  connection…** opens that template in the library. `:template/role`
  ("Usually for") only orders the size pickers: applying a template never
  changes a member's role, which is chosen when it is drawn.
- **Instance mode** (**Edit assembly**, double-click in the tree or
  right-click > Edit assembly) edits one assembly, loaded from the server
  (`GET /api/buildings/:id/assemblies/:xid`, which runs
  `modeller.template/assembly-state`). It adds the mark, role and size, the
  XLine's ends, length and rotation, and the template it came from. Its
  Assembly view is the member as Save would make it, regenerated in the
  browser against what its ends really sit on (the page data carries that
  context: the XLine, the XLines its nodes hang off, the grid and levels, its
  connections, and the connections other members bolt into it, which drill
  its holes), still drawn alone. Each end card says what the end sits on;
  **Edit connection…** opens the connection editor in instance mode, and
  **Replace with…** swaps the end for a fresh copy of a library connection
  template. **Save** sends `:assembly/update` (and `:xline/set-rotation` if
  that changed). **Re-apply from library…** applies a template (the one it
  came from first) at a size to this member again.
- The **connection editor** orders the actions (add, ↑ ↓, remove), edits the
  parameters (a formula's error shows on its row) and previews the
  connection. A connection template knows nothing of member roles, only the
  two ECSs it joins and how (cut, plate, bolts...), so one template serves a
  beam into a column and a joist into a beam.
  - **Library mode** (from the Templates panel or an assembly template's end)
    edits the library template, previewed on a sample owner of one size
    framing into a sample support of another, the support either a vertical
    column (the default) or a horizontal beam the owner meets at 90 degrees,
    tops flush, both sized from the library. **Save** sends `:template/save`
    with a 240 x 160 PNG thumbnail of the preview (`:template/thumbnail`),
    and changes the library only.
  - **Instance mode** (from an assembly's end card, or **Edit start/end
    connection…** in the properties) edits that connection's own copy,
    previewed on the real member and the real member that end sits on, where
    they are in the model. It says which library template the copy came
    from. **Save** sends `:connection/update`, which regenerates the member
    and what it bolts into; **Save as new library template** puts a copy in
    the library (with a thumbnail of the real connection), and the
    connection then notes it came from there once it is saved.
- Previews regenerate in the browser with `modeller.core` and the in-browser
  section library (`construcad.template-edit`), so nothing goes to the server
  until **Save**.
- To apply one to many members, select them (Shift + click adds or
  removes), choose the template and the size under **Apply a template**,
  and **Apply** (`:assembly/apply-template`). The supporting member of each
  end is read from the end node; an end that isn't on another XLine gets no
  connection, and the status bar says so.

Records from before templates were copied still load: a connection with a
`:connection/template` id and no actions of its own is built through that
library template with its per-end overrides (`modeller.assembly/own-recipe`)
and becomes a copy the next time it, or its assembly, is saved; an
assembly's `:assembly/template` (the old live link) or bare
`:assembly/from-template` id reads as where it came from.

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
| `construcad.navigator` | the navigator tree (level of an assembly, grouping, search) and the properties summary (pure) |
| `construcad.snap` | snap candidates and the node references they write (pure) |
| `construcad.store` | XTDB reads and the edit transaction |
| `construcad.template-edit` | template drafts, typed parameters, the end-on view and member-space of the member, in-browser preview models, assembly page edits (pure) |
| `construcad.api` | EDN endpoints (projects, buildings, view, templates, one assembly, edits) and the patch websocket |
| `construcad.app.reader` | instructions to three.js; `construcad.app.slice` cuts and caps meshes |
| `construcad.app.viewport` | camera, navigation, view cube, hidden-line display, picking |
| `construcad.app.templates` | template library panel (thumbnails), assembly page (templates and single assemblies), connection editor (library and instance modes) |
| `construcad.app.mini-view` | the small 3D views in the template editors (the assembly page's Assembly view, the connection preview) |
| `construcad.app.main` | toolbar, navigator, properties, XLine tool, grips, selection, context menu, applying templates, server round trip |

## Core

| Namespace | What it does |
|---|---|
| `modeller.vec` | vectors and the tolerance policy |
| `modeller.frame` | rigid frames (ECS), XLine frame rule, compose/invert |
| `modeller.grid` | grids, levels, grid points, reading a point back against the grid, the level a point is on or above |
| `modeller.node` | node references (absolute, grid, coincident, on-line, relative) |
| `modeller.model` | dependency-ordered XLine resolution, affected-set calculation |
| `modeller.profile` | outline loops with arc bulges, round-corners, tessellation, area, nine insertion points, face planes |
| `modeller.profile-builder` | builds outlines; SCI-sandboxed scripts for custom extrusions |
| `modeller.library` | compiled builders + section records with stored outlines |
| `modeller.shapes` | standard shape builders: rect, I, channel, angle, RHS/SHS, CHS |
| `modeller.sections-au` | 120 AU hot rolled sections from the ASI 8th edition tables (UB, UC, UBP, PFC, EA, UA), plus placeholder hollow sections |
| `modeller.instr` | the reader instruction algebra |
| `modeller.assembly` | parts to instructions, connection actions (`:cut-to-face`, `:end-plate`, `:bolt-group`) from each connection's own recipe |
| `modeller.params` | template parameters: numbers or SCI formulas over section data |
| `modeller.template` | templates: copy an assembly template onto an XLine at a size, connection copies and provenance, one connection's update |
| `modeller.core` | `regenerate` and `edit` entry points |

## Tests

    clojure -M:test            # JVM: core, edits, snapping
    npm i && npm run test:cljs # node via shadow-cljs: the same, plus mesh slicing and booleans
