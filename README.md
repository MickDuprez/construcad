# construcad core

Shared `.cljc` kernel for the XLine-based steel and construction modeller.
No three.js, DOM or database code lives here: model data in, reader
instructions out. The design is in the "Steel modeller core spec" doc.

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

Run the tests:

    clojure -M:test            # JVM
    npm i && npm run test:cljs # node, via shadow-cljs
