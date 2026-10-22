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
- **XLine** draws a new member: click a start and an end (same snaps), or type
  coordinates; `@dx,dy,dz` for the end is relative to the start. The same
  grid intersection at both ends makes a column, anything else a beam. The
  grid is drawn, and grid snaps sit, at the working level ticked in the
  Levels panel. Esc cancels.
- Level elevations, grid axis offsets, grid label styles and a member's
  rotation are edited in the side panel.

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
| `construcad.api` | EDN endpoints and the patch websocket |
| `construcad.app.reader` | instructions to three.js; `construcad.app.slice` cuts and caps meshes |
| `construcad.app.viewport` | camera, navigation, view cube, hidden-line display, picking |
| `construcad.app.main` | panel, XLine tool, grips, typed entry, server round trip |

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
| `modeller.assembly` | parts to instructions, connection actions (`:cut-to-face`) |
| `modeller.core` | `regenerate` and `edit` entry points |

## Tests

    clojure -M:test            # JVM: core, edits, snapping
    npm i && npm run test:cljs # node via shadow-cljs: the same, plus mesh slicing
