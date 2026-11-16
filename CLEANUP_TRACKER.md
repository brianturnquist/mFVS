# mFVS cleanup audit and tracker

Created: 13 November 2026.

Audit baseline: main repository `776491e8850b447e638f7b267b8bcaab641584f4`,
with NVEL submodule `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.

This checklist stores the redundancy and dormant-code findings before cleanup.
P01–P03, D01–D15, C01–C19, B01–B03 and the unbuilt-directory removal (R01–R08, R10–R10d)
are complete. Six stub tasks, the metric audits M01–M21 (including M08b/M08c)
and the implementation tasks M01a–M07a, M08a, M08d/M08e, M09a–M21a
and M17b/M17d are also complete (M06a is included in M04a). The checklist
now records 162 completed and 10 pending items, including R09. L01–L03
are complete with the explicit AK MORTS retention decision below.
BC/ON simulation parity remains blocked by the existing bugs
in [FOUND_BUGS.md](FOUND_BUGS.md).

Run the necessary comparisons against original F77 and the immediate baseline,
then discard generated results and temporary builds. Keep this checklist and
bug descriptions; do not retain test-result archives, logs, receipts, compiler
dumps or separate completion reports.

The findings combine source-list inventory, source comparisons, call/ENTRY
inspection and historical notes. They are not blanket permission to delete
scientific alternatives. The unbuilt directories listed in section R were
removed on 14 November 2026 by the maintainer's decision; they remain in Git
and in upstream FVS.

## Using this tracker

- Work on one bounded task at a time. Do not combine whole routine families,
  metric conversion, extension retirement, and bug fixes into one change.
- Keep existing IDs stable. Add a new ID when a task must be split further.
- Leave a checkbox unchecked while work is pending, in progress, blocked, or
  awaiting verification. State the status next to the item when needed.
- A completed audit or retention decision does not mean implementation or
  deletion is complete. The task type states what its checkbox covers.
- For implementation, deletion, and build changes, check the item only after
  the applicable verification below passes; update the checkbox and note any
  unresolved coverage briefly.
- Match the existing coding style. Preserve useful scientific commentary and
  explain variant-specific choices where future debugging will require it.
- Keep bug fixes that change model results separate from modernization.
  Record newly discovered defects in [FOUND_BUGS.md](FOUND_BUGS.md).
- Recheck build membership and callers against the current checkout before
  acting on an old finding. Counts and locations below describe the audit
  baseline, not every future revision.

Task types used below are **audit**, **implementation**, **deletion review**,
**decision**, and **build change**. A deletion review includes the preservation
decision and, only when that removal is authorized, the verified deletion.
Record a retained/deferred outcome explicitly instead of silently deleting an
item from the tracker.

- [x] **A00 — Audit findings stored.** The read-only findings, upstream parity
  requirements, centralized destinations, and task list are recorded in this
  document. This checks off documentation only; no cleanup is complete.

## Original upstream parity is required

The fixed-form upstream reference is:

| Component | Required reference revision |
|---|---|
| Main FVS repository | `c210536536d81fd9b46dccb9a571f0de1e55e944` |
| NVEL submodule at that revision | `38548071d5aa652bb90c7f111f86b427f798a1c9` |

The current converted NVEL fork differs from the original reference's
submodule. Pin both revisions explicitly; do not silently substitute the
current library into the original reference.

Check every change that touches a compiled file with the whole regression
suite:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

It compares the original upstream reference, with its original NVEL revision,
against the working tree. It runs every `tests/FVSxx` makefile for each and
requires every generated file to match the original, restart files included
(apart from two undefined original values; see bugs 11 and 12 in
[FOUND_BUGS.md](FOUND_BUGS.md)). It first checks that every source-list
entry exists and that every new source file is in a source list. A task is
done when the final line says `PASS` for all variants the change can affect;
put that line in the commit message. If a difference already exists, record
it; do not redefine the baseline to conceal it.

Follow [Contributing.md](Contributing.md), including its documented linkage-only
accommodations for building the original BC/ON reference. Keep scientific
changes separate. The pinned revisions above identify the original sources;
generated patches and build records are temporary.

For each implementation task:

- Derive all affected variants from the source lists, including consumers that
  use another variant's implementation.
- Use the same compiler versions, relevant flags, platform, and inputs for
  reference and candidate comparisons. Check both submodule revisions.
- Compare listings, summaries, tree lists, database schemas and contents, and
  other generated model outputs. Normalize only identified run metadata such as
  UUIDs, dates, times, and version strings. Do not discard model values, use a
  numerical tolerance, or broadly ignore formatting differences without a
  separately justified comparison contract.
- Require the comparison to fail on a real mismatch. Existing test makefiles
  ignore some program and `diff` exit codes; a successful `make` exit is not
  sufficient evidence.
- Add or use targeted cases that reach the differing branches. Compare those
  cases with the original too. Preserve arithmetic order, precision, species
  indexing, defaults, common-block layout, initialization order, and observable
  diagnostics.
- Check source-list selection and filename collisions, then run
  `git diff --check`. Record checks and limits before checking off the task.

For an unbuilt-file deletion, establish that no current source list, selected
include dependency, generated source, or supported auxiliary target needs the
file. Verify that the compiled input set remains unchanged and check the
applicable builds and outputs. A missing direct caller alone is insufficient.

The README reports original-reference agreement for 22 variants. That result
was not rerun in this audit. BC and ON currently stop during keyword input in
both versions; matching those failures does not prove simulation parity. Their
recorded defects must be handled separately. A deliberately corrected reference
can help diagnose those defects, but it must remain distinct from the original
upstream reference and cannot be reported as pristine-original parity.

- [x] **P01 — Prepare the original reference.** Completed 13 November 2026.
  Pinned the original main/NVEL revisions and checked IE/BC/ON builds with
  identical settings; Canadian linkage-only additions are in Contributing.md.
- [x] **P02 — Establish exact comparison.** Completed 13 November 2026.
  IE `iet01` matched original/current/candidate; controls rejected changed
  model values, database rows, identities and output inventory.
- [x] **P03 — Establish the BC/ON boundary.** Completed 13 November 2026.
  Both versions stop during keyword input; bugs 1, 2 and 5 remain unfixed.
  Matching these failures does not establish Canadian simulation parity.
  The user accepted direct routine/SQLite and statement comparisons for
  C01/C06, C07/C10 and C13/C14 while leaving this limitation in place.

## Centralized definitions

Use existing shared areas for canonical routines. Avoid adding another set of
per-variant implementation copies. Keep genuine variant differences explicit
through parameters, tables, or narrow branches within the shared implementation.

| Shared area | Responsibility |
|---|---|
| `base/` | General utilities and core infrastructure |
| `vbase/` | Shared model logic that varies geographically |
| `estb/` | Establishment and regeneration algorithms |
| `fire/base/`, `fire/vbase/` | Shared fire algorithms and reporting |
| `dbsqlite/`, `vdbsqlite/` | Shared database operations |
| `common/` | Shared declarations and parameter definitions |
| `organon/` | Shared ORGANON implementation and mappings |
| `volume/NVEL/` | Volume-library implementation within its submodule |

The build copies selected inputs into one flat directory. Each variant's
source set must select one implementation per basename and routine. Update all
affected source lists and include classification when moving a definition.
The shared destinations below are proposed homes; creating this tracker does
not move any files.

Use `VARIANTCD` when behavior can run before `GRINIT`, `VARACD` for appropriate
runtime branches, and `LMETRICVAR` for the existing metric distinction, as
described in `Contributing.md`. Preserve when those values become available.

## Audit inventory

These counts exclude files inside the NVEL submodule and include tracked
Fortran include files with Fortran extensions.

| Classification | Files |
|---|---:|
| Tracked Fortran files in the main repository | 2,081 |
| Compilation sources selected by current builds | 1,396 |
| Selected Fortran include files | 117 |
| Fortran files outside all 24 current source lists | 568 |

All 24 current manifests had existing input paths and no duplicate basenames.
No identical whole-file active routine copies were found after removing
comments and whitespace while preserving literals. Close copies still have
real statement differences, including species tables, defaults, diagnostics,
and control flow. The 568 unlisted files are dormant inventory, not 568 proven
safe deletions.

After the section R, D14 and D15 removals on 14 November 2026 there are
1,528 tracked Fortran files, of which 28 are outside all 24 source lists:
`acd/` (27, R09) and `organon/orgfert.f` (kept on purpose).

The historical source lists under `canada/bin/` were removed with section R.
Trace the selected implementation, not just the routine name: many old files
define names that remain live through a different file.

## C — Bounded consolidation tasks

Each item is an implementation task after its comparison and dependencies are
understood. Retain existing behavior, including known defects, during these
structural changes. Build and compare every consumer identified from manifests;
the named variants below are starting points, not a substitute for that check.

- [x] **C01 — ESTIME.** Completed 14 November 2026 in `estb/estime.f90`:
  removed the unused ESCOM2 declaration and STRP twin. All 23 consumers build;
  direct timing/state comparisons match F77 and the immediate baseline.
  BC/ON runtime objects also match. The user accepted that evidence for this
  task's completion; Canadian integrated simulation parity remains blocked.
- [x] **C02 — CI/IE ESPSUB.** Completed 14 November 2026 in
  `estb/espsub.f90`, including existing KT consumption. Preserved first-ten
  equations and exactly the old tails using `PSUB(11:MAXSP)`: CI 19, IE 23,
  KT 11 species. Direct bounds-checked and integrated comparisons pass.
- [x] **C03 — CI/IE ESPXCS.** Completed independently 14 November 2026 in
  `estb/espxcs.f90`. Preserved all equations and variant tails using
  `PXCS(11:MAXSP)`; CI/IE/KT direct and integrated comparisons pass.
- [x] **C04 — KT/NC FMCROW.** Completed 14 November 2026 in
  `fire/base/fmcrow.f90`. Preserved both full mapping tables and their 11/12
  lengths. Every mapped species is exercised with actual crown equations.
  The old NC comment/table discrepancy is logged without changing mapping 19.
- [x] **C05 — OP/PN/WC FMCFMD.** Completed 14 November 2026 in
  `fire/base/fmcfmd.f90`. OP alone includes non-WC species 24 in WOCT; PN/WC
  grouping and arithmetic order are preserved. Direct and integrated tests
  pass, and targeted group outputs distinguish the variants as expected.
- [x] **C06 — CI/IE FMSFALL.** Completed 14 November 2026 in
  `fire/base/fmsfall.f90` for CI plus BC/CA/IE/KT/NC/OC/WS. Only CI retains
  its species-2/8 adjustment at DBH >= 18. All eight direct fixtures and
  seven available integrated simulations pass; BC runtime objects match.
  The user accepted that evidence for this task's completion; BC integrated
  simulation parity remains blocked.
- [x] **C07 — CS/SN FMSFALL.** Completed 14 November 2026 in
  `fire/base/fmsfall.f90`. Preserved CS `KSP > 2` versus SN `KSP /= 2`,
  the 12-inch breakpoint and capped rate. All ten shared consumers match
  direct original/baseline outputs; the former shared executable body is
  unchanged after compile-time selection. The user accepted that alternative
  evidence for completion; no runtime-object equality or BC simulation result
  is claimed.
- [x] **C08 — PN/WC GRINIT.** Completed 14 November 2026 in
  `vbase/grinit.f90`. Preserved variant code, elevation 7/35, forest 2/6 and
  longitude 123/122 at the original positions. All 210 assigned model/helper
  variables match before growth; integrated comparisons pass.
- [x] **C09 — CS/NE CRATET.** Completed 14 November 2026 in
  `vbase/cratet.f90`. Preserved numeric calculations and CS slope-only versus
  NE intercept-plus-slope diagnostics. Real-engine debug decks exercise and
  exactly compare both layouts against original F77 and the baseline.
- [x] **C10 — ESADDT.** Completed 14 November 2026 in `estb/esaddt.f90`
  for all 24 variants. Compile-time IDF preserves former ESTB index 5 versus
  STRP index 3, including bug 3. All direct export files and executable bodies
  match original/baseline after parameter resolution. New data-flow concerns
  are recorded unfixed. The user accepted that alternative evidence for
  completion; BC/ON integrated simulation parity remains blocked.
- [x] **C11 — PN/WC FMVINIT.** Completed 14 November 2026 in
  `fire/base/fmvinit.f90`. Preserved species-6 initial V2T 20.6 versus 28.1,
  all other defaults, and the existing final unit conversion. All 37 assigned
  variables/species and integrated outputs match original/baseline.
- [x] **C12 — OC/OP ORGSPC.** Completed 14 November 2026 in
  `organon/orgspc.f90`. Preserved every value and index of the 50-entry OC and
  39-entry OP tables. All actual mapping calls and integrated outputs match
  original/baseline; old species-order commentary and recovery refs remain.
- [x] **C13 — First FIA database binding helper.** Completed 14 November
  2026 in `dbsqlite/dbs_fiavbc_bindtree.f90`, called from both TRLS binding
  sites and ATRTLS. The nine DBH-through-CarbFrac fields preserve column
  order, types and REAL-to-REAL*8 calculations. IDs, selection and report
  construction stay in callers. All 24 variants' actual SQLite schemas/rows
  and expanded executable statements match F77/baseline. The user accepted
  this evidence for completion while Canadian simulation parity stays blocked.
- [x] **C14 — FIA cut-list caller migration.** Completed and verified
  separately after C13 on 14 November 2026. CUTLST uses the same binding
  helper while retaining cut selection, probability, IDs and reports. All 24
  variants' actual SQLite schemas/rows and executable statements match both
  baselines. The user accepted this evidence under the same limited P03
  exception; Canadian simulation parity remains blocked.
- [x] **C15 — First mortality-allocation helper.** Completed 14 November
  2026 in `vbase/mortalloc.f90` for CA VARMRT and NC NWCMRT, including OC's
  CA caller. Variant efficiencies and initialization remain in callers.
  Arithmetic order, stopping rules, array updates and debug output are
  preserved. Background, adjustment, saturation and redistribution cases
  match original F77 and the immediate baseline exactly.
- [x] **C16 — TT/UT mortality caller migration.** Completed separately after
  C15 on 14 November 2026. TTMRT and UTMRT use MORTALLOC; their distinct
  efficiency/crown-ratio calculations stay in place. Both variants' direct
  saturation/redistribution cases and integrated outputs match both baselines.
- [x] **C17 — SO mortality caller migration.** Completed 14 November 2026
  as a separate SCOMRT migration. SO's efficiencies remain in its caller;
  direct allocation boundaries and integrated outputs match both baselines.
- [x] **C18 — WS mortality caller migration.** Completed 14 November 2026
  as a separate WS VARMRT migration. WS's efficiency scale/crown correction
  remains in place; direct and integrated outputs match both baselines.

- [x] **C19 — Shared helper extractions.** Completed 14 November 2026:
  INTRDEAD/INTRFIN/INTRSCRN, CI/IE ESFIXH, FMMODSEL, CS/SN FMSNFT and CVMAPSP.
  All eight sources are selected by every consumer (112 manifest entries).
  FMMODSEL includes BC/ON by explicit maintainer direction; metric twins,
  common-block merges, the newer FIA binding helper and MORTALLOC are untouched.
  `PASS 24/24 variants`, subject to P03. Focused original-F77 checks also match:
  14 fuel-selection cases and 29,772 CS/SN forest-type cases. Bug 14 is unfixed.

The mortality opportunity crosses routine names: `VARMRT`, `NWCMRT`, `SCOMRT`,
`TTMRT`, and `UTMRT` repeat allocation logic. The three FIA writers likewise
share long binding sequences despite having distinct names. Whole-file or
same-name-only duplicate searches miss these opportunities.

## M — Active metric copies: bounded audits first

There were 21 selected metric copies; 14 remain after M01a–M07a. All tasks in
this section are initially **audits**, not authority to merge whole files. Each
should produce a difference map for one record group, keyword handler, or
calculation block, identify state and interface dependencies, and propose the
next implementation-sized task. Split further if a block is still too large.
BC/ON parity remains subject to P03.

- [x] **M01 — `metric/base/evldx.f90`.** Audit completed 14 November 2026 at
  `3b15e43a`; no model code changed. The saved variables (codes 101–450) were
  checked index by index against `base/algkey.f90` and their setters; the
  differences elsewhere were few enough to classify the whole file. Apart
  from layout, the copies differ in three ways:
  - **Unit layer (the only intended difference).** DBH and height limits are
    converted on input in SPMCDBH, SNAGS, SALVVOL and TREEBIO. Results are
    converted for saved-variable groups 1–4 (four computed-GOTO tables),
    SPMCDBH, DBHDIST, SUMSTAT, FUELLOAD, SNAGS, POTFLEN, POTFMORT, SALVVOL,
    STRSTAT, POTSRATE, POTREINT, TREEBIO, HTDIST, HERBSHRB, DWDVAL and
    ACORNS. BCCFSP/ACCFSP, FUELMODS, POINTID, POTFTYPE, CARBSTAT, user
    variables and constants are the same in both copies.
  - **Stale code.** The metric copy missed two upstream changes. It rejects
    constant codes of 7000 and above although `MAXPRM` is 7999 (`86463e16`),
    so no expression with a number in it can be evaluated in BC/ON (bug 20,
    reproduced). It lacks the 2025 sawlog additions (`08e833b2`): SPMCDBH
    attribute 15, DBHDIST attributes 12–13 and SUMSTAT columns 18–22 are
    rejected, and SPMCDBH attribute 9 sums `WK1` where base sums `MCFV`.
    Saved variables numbered beyond the tables (141–160, 226–238, 426–450)
    are returned unconverted.
  - **Wrong factors in the unit layer** (bug 21): latitude, BHTWTBA, BSCUFT,
    AMERBIO, CROWNIDX, the DBHDIST totals, SNAGS trees and basal area, and
    the ACORNS count; before-thinning density variables are converted where
    their after-thinning partners are not.

  Dependencies: both copies call the same routines with the same arguments,
  and every setter stores imperial values in BC/ON as elsewhere (including
  `metric/fire/vbase/fmpofl.f90`), so the conversion belongs in EVLDX alone. `METRIC.f90` is already selected by all 24 variants
  and `LMETRICVAR` is true only for BC and ON. Two of the stale limits follow
  real BC/ON state and cannot simply be lifted: the Canadian `VOLS` routines
  store merchantable cubic volume in `WK1` and never fill `MCFV` or `SCFV`,
  and `metric/vbase/disply.f90` does not fill IOSUM columns 21–22.
- [x] **M01a — Merge EVLDX (implementation).** Completed 14 November 2026.
  All 24 variants build `base/evldx.f90`; `metric/base/evldx.f90` is removed
  (1,321 lines out, 124 in). Each conversion is behind `LMETRICVAR`. The four
  computed-GOTO tables became unit-code tables applied in one block, which
  keeps the original divide or multiply. BC/ON behaviour is unchanged by
  design: the `WK1` read for SPMCDBH attribute 9 and the constant-code,
  attribute and column limits stay behind the same switch, so bugs 20 and 21
  are carried over unfixed. `PASS 24/24 variants; working tree
  3b15e43a+changes vs original c2105365`, subject to P03. Because the BC/ON
  decks stop at keyword input, old and new EVLDX were also compared directly
  for BC, ON, IE and SN: a driver with each variant's common blocks called
  both over every instruction code and a grid of arguments (86.5 million
  calls per variant, callees replaced by stubs that record their arguments).
  Return codes, result bits, arguments passed on and debug output were
  identical. As a control, 210 single-site changes to the merged routine
  (every unit-code table entry, switch use, conversion factor and limit)
  were each detected by that comparison for both BC and ON.
M02–M07 were audited together on 15 November 2026 at `a442d9af`; no model
code changed. Each pair was small enough to compare whole, statement by
statement with string literals kept. Upstream last touched these six metric
copies in July–August 2022, so later changes to the imperial files are
missing from them ("stale code" below). The follow-up tasks M02a–M07a keep
BC/ON behaviour, defects included, behind `LMETRICVAR`, as M01a did. The BC/ON
decks cannot reach any of these routines (bug 5), so each merge needs a direct
old-against-new driver like the one used for M01a.

- [x] **M02 — `metric/base/intree.f90`.** Audit completed.
  - **Unit layer (the only intended difference).** After the crown-ratio
    step, `DBH` and `DG` are converted from centimetres and `HT`, `THT` and
    `HTG` from metres; site-tree DBH and height are converted when stored in
    `SITETR`.
  - **Stale code.** `DBSTREESIN` is called with 30 of 33 arguments (bug 2).
    The callee returns before touching the missing three when trees come
    from a tree file, so only database input is undefined. There is no
    decay-code correction and no woodland stem count, and `CULL`, `DECAYCD`
    and `WDLDSTEM` are neither set nor moved with dead trees. A blank species
    code is not replaced by `OT` (`d03923d6`, 2025). With one inventory plot,
    every record is given plot 1, where base keeps the plot ID read (change
    of 2022-02-04). Formats 895, 35 and 75 differ by one blank each.
  - **Same code in another form.** The species/DBH screen, the dead-tree
    move and the end-of-file summary are the in-line originals of `INTRSCRN`,
    `INTRDEAD` and `INTRFIN` (C19), statement for statement.
- [x] **M02a — Merge INTREE (implementation).** Completed 15 November 2026.
  All variants use `base/intree.f90` and its three shared helpers; removed
  the 603-line metric copy. `LMETRICVAR` retains both conversion blocks,
  the older plot/species/decay/dead-tree behaviour and diagnostic spacing.
  The shared call passes all 33 `DBSTREESIN` arguments, completing the
  interface half of bug 2. Real SQLite input in BC and ON verifies cull,
  decay and stem outputs and metric diameter/height conversion. Tree-file
  behaviour is unchanged; the missing metric decay correction remains.
- [x] **M03 — `metric/base/oplist.f90`.** Audit completed. Nothing in the
  imperial routine is missing from the metric copy.
  - **Unit layer.** Before printing, each activity's parameters are
    multiplied by the factors from `MCNVRT(IACTK,2,…)`, in the three WRITE
    statements of the activity schedule and summary. `ECOPLS` receives
    unconverted values in both copies. The routine is `RECURSIVE` because
    `MCNVRT` calls its `OPKEY` entry.
  - **Other differences.** The translation table has one more row (149):
    activity 490 as establishment keyword `NATURAL`. No built code schedules
    activity 490; it matters only if an activity file read by `OPRDAT`
    contains that code, which BC/ON would list as `NATURAL` and convert,
    and other variants list as `*UNKNOWN`. Heading formats 10, 11 and 12
    start with a blank.

  Dependencies: `MCNVRT`, `OPFIND` and `OPRDAT` are already single shared
  files for all 24 variants, and `base/oplist.f90` already has `OPKEY`.
- [x] **M03a — Merge OPLIST (implementation).** Completed 15 November 2026.
  All variants use recursive `base/oplist.f90`; removed the 361-line metric
  copy. `LMETRICVAR` selects parameter conversion, activity 490/NATURAL and
  the three metric headings. Imperial values are printed without arithmetic.
  Direct comparisons cover 66 schedule/summary calls and OPKEY codes
  −5 through 3100 per variant, including unknown and completed activities.
- [x] **M04 — `metric/base/stats.f90`.** Audit completed. The copies share
  the initialization, the accumulation loop and one statistics calculation
  repeated once per attribute (six times in base, four in the metric copy).
  Everything printed differs.
  - **Unit layer.** Species table per hectare, columns in the order net
    merchantable volume, gross volume, trees, basal area (base: trees, basal
    area, cubic feet, board feet). The `BFV` total is printed as cubic metres:
    ON stores merchantable cubic volume there and BC stores zero. Sample-point
    rows in the order trees, cubic metres, board feet, basal area (base:
    trees, basal area, cubic, board), with every value in the board-foot row
    multiplied by zero. Labels are 16 characters (base 19).
  - **Stale code.** No above-ground biomass or carbon (`08e833b2`, 2025):
    two species columns, two sample-point rows and two `DBSSTATS` arguments.
    Heading 9006 starts with a blank.
  - **Defects (bug 23).** The unit factor is applied to the sampling error
    in percent and not to the sampling error in units. The database gets
    zero board-foot volume where the printed table shows a value.

  Dependencies: the 17-argument `DBSSTATS` call matches only the metric
  copy (M06), so the two files must change together.
- [x] **M06 — `metric/dbsqlite/dbsstats.f90`.** Audit completed. Sample-point
  rows go to the same `FVS_Stats_Stand` table with the same columns and
  binding types as base, holding metric values; the label is 16 characters.
  Species rows go to `FVS_Stats_Species_Metric` with four REAL columns
  (`BoardM`, `CubicM`, `TreesPerHa`, `BasalArea`), where base writes
  `FVS_Stats_Species` with six (`TreesPerAcre`, `BasalArea`, `CubicFeet`,
  `BoardFeet`, `FIA_AbvGrdBio`, `FIA_AbvGrdCarb`). The interface has 17
  arguments (base 19). No conversion is done here; `STATS` passes converted
  values.
- [x] **M04a — Merge STATS and DBSSTATS (implementation, includes M06a).**
  Completed 15 November 2026. Removed both metric copies (652 lines).
  `base/stats.f90` uses one internal statistics helper, preserving arithmetic
  order, metric row/column order, labels, zero-volume branches and bug 23.
  `dbsqlite/dbsstats.f90` takes 19 arguments for every variant and selects
  the original metric schema, binding order and four value columns behind
  `LMETRICVAR`. Direct comparisons cover 252 statistics cases and 144 SQL/
  binding cases per variant, including disabled output and database errors.
- [x] **M05 — `metric/dbsqlite/dbsin.f90`.** Audit completed. The copy has
  no unit conversion at all; every difference is stale code or a defect.
  - `KARD(7)` where `KEYRDR` fills 12 (bug 5). Upstream widened `KEYRDR` in
    `08e833b2` (2025-08-28) without the metric copies.
  - Keywords 40–43 (`VBCSUMDB`, `VBCTRLDB`, `VBCCUTDB`, `VBCATRDB`) are
    absent, so BC/ON reject them as invalid. The four switches they set stay
    at the zero that `DBSINIT` gives them.
  - `DSNOUT` does not call `DBSCASE(1)` after opening the database
    (`62a9de10`, 2025-02-06), so BC/ON create the case row only when the
    first table is written.
  - Formats 1110 and 1210 lack a comma (same output). Formats 1120, 2710
    and 2810 print a stray apostrophe since the free-form conversion
    (bug 22).
- [x] **M05a — Retire the metric DBSIN (build change).** Completed
  15 November 2026. BC/ON select `dbsqlite/dbsin.f90`; removed the 970-line
  copy. `KARD(12)` resolves this routine's part of bug 5, and three FORMATs
  regain their F77 text (bug 22). `LMETRICVAR` retains rejection of keywords
  40–43 and deferred case creation after DSNOUT. Direct BC/ON checks use
  the real twelve-field KEYRDR over 34 keyword names and three parameter
  settings, including those rejections and case timing. The comparison
  widens only KARD in the old/F77 DBSIN test copies to avoid their overflow;
  it does not claim equivalence for undefined writes.
- [x] **M07 — `metric/fire/base/fmdout.f90`.** Audit completed.
  - **Unit layer.** All-fuels values are multiplied by `TItoTM/ACRtoHA`
    for both the printed report and `DBSFUELS`; down-wood volumes by
    `FT3pACRtoM3pHA` for the report and `DBSFMDWVOL`. Headings name tonnes
    per hectare, cubic metres per hectare and centimetre size classes. Cover
    is a percentage and only its heading differs. The three database
    routines are the shared ones, so metric values go into the same tables.
  - **`FMSVL2` (bug 1).** The copy makes the 8-argument call. That matches
    BC's own `canada/fire/bc/fmsvol.f90` and is wrong for ON, which links
    the 10-argument `fire/base/fmsvol.f90`. The imperial 10-argument call
    would be right for ON and wrong for BC, as 26 shared call sites already
    are.
  - **Stale code.** The report window: base prints when the year lies
    between the begin and end years; the copy prints only when the begin
    value is zero or negative. That suits the −1 the metric `FMIN` sets for
    the down-wood reports, but `FUELOUT` sets the inventory year, so BC/ON
    produce no all-fuels report once an inventory year is set (bug 24). Two debug WRITEs around
    `DBSFUELS` are absent. Heading 715 is spaced differently.
  - Formats 702, 802 and 902 print a stray apostrophe since the free-form
    conversion (bug 22).
- [x] **M07a — Merge FMDOUT (implementation).** Completed 15 November 2026.
  The prerequisite interface repair is isolated in `canada/fire/bc/fmsvol.f90`:
  FMSVL2 accepts all ten shared arguments, ignoring crown ratio and live/dead
  flag as before. Its eight/ten-argument results match F77 and HEAD across
  all BC species and a grid of heights, merchantability and debug flags.
  All variants select `fire/base/fmdout.f90`; removed the 564-line copy.
  `LMETRICVAR` retains metric factors, headings, absent database debug lines
  and the old report-window rule (bug 24; M08 audits its input contract).
  The shared ten-argument call also resolves ON's side of bug 1. Three
  FORMATs regain F77 text (bug 22).
  Direct comparisons cover 240 cases per variant, report windows, database
  suppression, live/dead fuels, headings and common-block state.

Verification for M02a–M07a: `PASS 24/24 variants; working tree
a442d9af+changes vs original c2105365`, rerun 15 November 2026 with the pinned
original NVEL revision. The source lists and `git diff --check` pass. BC/ON
still stop at keyword input (P03/bug 5), so this is not Canadian simulation
parity. Direct drivers for BC, ON, IE and SN compare the five merged routines
above against both F77 and the immediate baseline, including exact output,
argument bits and state. Comparisons of INTREE database input use only the
defined old argument prefix; the repaired tail is checked separately with
real SQLite. FMDOUT compares the old eight-argument call with a matching stub,
not ON's undefined original call. Only the documented FORMAT apostrophes differ
from the immediate baseline; their text matches F77. Controls changing one
conversion or schema in each of the five routines are all detected. Generated
drivers, builds and outputs are temporary and are not kept in the repository.

M08–M14 were audited on 15 November 2026 at `ed5e92fb`, against the pinned
F77 reference as well as the immediate baseline. No compiled files or source
lists changed. The four metric fire routines and ESUCKR serve BC/ON; the two
`metric/newmist` routines serve BC only. The imperial fire routines serve the
other 22 variants. MISIN/MISPRT serve 18 imperial variants; CS/LS/NE/SN/ON
select the `base/exmist.f90` stubs. The findings below cover the named blocks,
not whole-routine equivalence or authority to retire these seven copies.

- [x] **M08 — FMIN: DWDVLOUT handler (audit).** Compared option 51,
  labels 5100–5115, with `fire/base/fmin.f90`. Landscape calls reject the
  request before changing state. Stand calls allocate `IDDWRP` only when
  it is zero and optionally echo the same message. The sole statement
  difference is the report window: metric sets `IDWRPB/IDWRPE = -1/-1`;
  imperial sets `IY(1)/IY(1)+999`. No numeric keyword field or unit factor
  is used. Shared FMDOUT's metric branch tests only `IDWRPB > 0`, whereas
  its imperial branch tests the current year against both bounds. These
  contracts must remain paired; changing the handler alone disables metric
  down-wood output for positive inventory years. This confirms the dependency
  in bug 24, without fixing it.
- [x] **M08a — Share the DWDVLOUT request block (implementation).**
  Completed 15 November 2026 in `fire/base/fmdwdreq_mod.f90`, called by
  both FMIN readers. Preserved the landscape rejection, GETID call count,
  optional echo and both report-window contracts behind `LMETRICVAR`.
  No other FMIN handler moved; bug 24 is unchanged.
- [x] **M08b — FMIN: DWDCVOUT handler (audit).** Completed 15 November
  2026: compared option 52, labels 5200–5215, with `fire/base/fmin.f90`
  and traced shared FMDOUT/DBSFMDWCOV. Both readers reject landscape calls
  before changing state, allocate IDDWCV only when zero, and optionally echo
  the same message; neither uses a numeric keyword field. Metric sets
  `IDWCVB/IDWCVE = -1/-1`; imperial uses `IY(1)/IY(1)+999`. FMDOUT's
  LPRINT3 uses only `IDWCVB > 0` for metric, versus the inclusive year window
  and zero-year exception for imperial. Keep those contracts paired (bug 24).
  The consumer sends fourteen unconverted percent-cover values, hard then
  soft, to DBSFMDWCOV before optional text; its database-only return, IDCPAS
  header counter and metric size-class labels belong to the consumer.
- [x] **M08d — Share the DWDCVOUT request block (implementation).**
  Completed 15 November 2026 in `fire/base/fmdwcvreq_mod.f90`, called by
  both FMIN readers for option 52 only. Landscape rejection, GETID count,
  optional echo and LMETRICVAR-selected windows are preserved. FMDOUT and
  its metric sentinel contract are unchanged; bug 24 remains unfixed.
- [x] **M08c — FMIN: FUELOUT handler (audit).** Completed independently
  15 November 2026: option 15, labels 1500–1515, is statement-equivalent
  in both readers. Landscape rejection precedes all updates; GETID is called
  only for zero IDFLAL, both set `IFLALB/IFLALE = IY(1)/IY(1)+999`, and
  echo is optional. No keyword-field conversion is involved. Shared FMDOUT
  skips both DBSFUELS and text for positive metric IFLALB (bug 24), while
  imperial accepts its year window, with the zero-year exception. When
  admitted, metric fuel loads use `TItoTM/ACRtoHA`, including conversion
  before NINT; database-only output bypasses text and IFAPAS header updates.
  Sharing the request must retain the existing suppression, not repair it.
- [x] **M08e — Share the FUELOUT request block (implementation).**
  Completed independently 15 November 2026 in `fire/base/fmfuelreq_mod.f90`.
  Both readers call it for option 15 only, preserving rejection, allocation,
  echo and identical year assignments. Positive metric begin years still
  suppress the report (bug 24); no producer/consumer correction was mixed
  into this extraction. Other FMIN handlers still need bounded audits.
- [x] **M09 — FMSOUT: detailed snag record (audit).** Compared the
  post-accumulation conversion and FORMAT 300 record with `fire/base/fmsout.f90`.
  Species/age/diameter-class accumulation, weighted averages, record order,
  zero-density screen and FORMAT 300 agree. Metric converts mean DBH by
  `INtoCM`, hard/soft heights by `FTtoM`, hard/soft volumes by `FT3toM3`,
  and densities by division by `ACRtoHA`, before database and text output.
  Thus its accumulated volume retains an acre area basis; do not silently
  replace that factor with `FT3pACRtoM3pHA` during consolidation. Text
  truncates each volume and their sum separately with INT; the database gets
  the untruncated reals. Shared DBSFMDSNAG applies no further conversion and
  selects `FVS_SnagDet_Metric`. Metric headings name CM/M/M3/SNAGS/HA;
  FORMAT 200 has the known bug-22 apostrophe. Its seven work arrays are
  allocatable, with cleanup on early exits; imperial uses fixed-size locals.
- [x] **M09a — Share detailed snag record preparation (implementation).**
  Completed 15 November 2026 in `fire/base/fmsrecord_mod.f90`. Both callers
  retain accumulation, averaging and allocation lifetime. The helper converts
  the seven values once, calls DBSFMDSNAG, and handles database-only output,
  open failure, headings, records and close. Original factors, acre-based
  volumes, separate INT operations and metric heading apostrophe (bug 22)
  are retained; the heading is now FORMAT 201 in the helper.
- [x] **M10 — FMFOUT: burn-conditions report (audit).** Compared the
  first report section through label 105 with `fire/vbase/fmfout.f90`.
  The metric section multiplies wind by `MItoKM` and flame/scorch heights
  by `FTtoM` for both DBSFMBURN and FORMAT 100; moisture percentages,
  slope, fire type, fuel-model IDs and rounded weights are unchanged.
  Headings use KMH/M. Both skip negative IFIRE, allow headers without a
  database/data row for IFIRE=0, and skip this report's text when the database
  sets its output code to zero. Metric admits the section only for
  `IFMBRB < 0`, ignoring the end year; imperial uses the year window.
  BURNREPT sets the metric begin flag to `IY(1)`, so ordinary nonnegative
  inventory years suppress the requested report (bug 25). Fuel-consumption
  and mortality sections, including their CFV/MCFV choices, remain unaudited.
- [x] **M10a — Share the burn-conditions section (implementation).**
  Completed 15 November 2026 in `fire/vbase/fmburnreport_mod.f90`.
  Preserved both gates (including bug 25), unit factors, counter, FMD=0
  row, model/weight formatting and database-only branch. Fuel-consumption
  and mortality sections remain in their callers, including the stand-header
  FORMAT 44 they also use. Imperial values undergo no conversion arithmetic.
- [x] **M11 — FMPOFL: first-cycle fire-condition records (audit).**
  Compared the `ICYC=1` loop calling DBSFMPFC with `fire/vbase/fmpofl.f90`.
  Both request FMMOIS scenarios 1/3 for the severe/moderate rows, and pass
  seven moisture percentages. Metric converts PREWND with `MItoKM` and
  POTEMP with `INT(POTEMP*FtoC1+FtoC2)`; imperial truncates Fahrenheit
  directly. Preserve these existing constants and conversion-before-INT.
  Metric tests `PRESVL(IDPL,1)` with IDPL left at 2 by the earlier scenario
  loop, while reading values from row IDPM. Imperial tests row IDPM itself.
  A severe-only override is therefore omitted from the database; a
  moderate-only override also selects the inactive severe row (bug 26).
  The text condition rows use the proper row flag. This block depends on
  FMMOIS, FMFCOM's PRESVL/PREWND/POTEMP and shared `vdbsqlite/dbsfmpfc.f90`,
  which selects `FVS_PotFire_Cond_Metric` without further conversion. The
  main fire calculations, CFIM, report gate, result rows and FMPOFL_FMPTRH
  remain outside this audit.
- [x] **M11a — Share first-cycle fire-condition records (implementation).**
  Completed 15 November 2026 in `fire/vbase/fmpotcond_mod.f90`. The helper
  receives the caller's IDPL and retains the metric override selection
  (bug 26), severe/moderate FMMOIS call order, MOIS state, conversions and
  truncation. Scenario calculations, text and main result rows stay in the
  callers. Asymmetric override cases match both references exactly.
- [x] **M12 — MISIN: keyword dimensions and BC state (audit).** BC's
  copy declares ARRAY/LNOTBK/KARD with seven entries; shared KEYRDR and
  KEYDMP require twelve (bug 5). Both KEYRDR call sites, including DMLIGHT's
  supplemental reads, are affected. The imperial declarations have twelve.
  The first eleven table slots and their handlers match statement for
  statement except MISTPRT's centimetre-to-inch threshold and converted
  echo. BC has 30 slots (28 names), versus 11 (10 names); MISKEY exposes
  that table too, so widening fields does not authorize dropping keywords.
  BC additionally resets NEWMOD on entry and includes `canada/newmist/DMCOM.f90`:
  - NEWSPRED enables the NISI model; DMCRTHRD changes crown-third DMDMR;
    DMSED changes the separate DMRNCM random stream.
  - DMOPAQ/DMOPQMLT set opacity; DMKTUNE/DMSTUNE/DMITUNE/DMETUNE set
    species spread/intensification controls; DMLIGHT changes response
    points; DMDEATH/DMCAP/DMFLOWER change life-history parameters.
  - DMCLUMP/DMAUTO schedule activities 2008/2009 for DMOPTS; DMTABLE
    enables LDETAIL. Slot 26 is blank/defunct.
  - DMBCI defines up to five biocontrol agents in DMBCCM. DMBCA should feed
    DMCYCL's activity 2010, but schedules 2004 and reads an unassigned
    IPRMPT. DMBCI's echo omits a required integer argument (bug 28).

  DMMIST contains crown geometry in two-metre mesh units, infection pools,
  light and spatial state, including biocontrol pools. MISIN0/DMINIT,
  `canada/newmist/mistoe.f90`, DMOPTS and DMCYCL initialize/consume that
  state. This is a separate scientific extension, not an LMETRICVAR layer.
- [x] **M12a — Share MISTPRT keyword handling (implementation).**
  Completed 15 November 2026 in `mistoe/mistprt_mod.f90`, retaining inches
  internally, both input/echo conventions and PRTMIS. BC's NEWMOD reset,
  30-slot MISKEY table and all NISI handlers remain in MISIN. The isolated
  prerequisite changes only six declarations: ARRAY/LNOTBK/KARD become
  twelve fields in both BC INITRE and MISIN. All keyword field indices are
  unchanged. This repairs BC's part of bug 5, not ON's INITRE. Real KEYRDR
  fixtures match F77 and the immediate baseline with only their corresponding
  widths repaired; no equivalence is claimed for original buffer overruns.
- [x] **M13 — MISPRT: diameter-class summary (audit).** Compared the
  class accumulation, ten-column summary and DBSMIS3 call with
  `mistoe/misprt.f90`. BC first converts DBH from inches to centimetres,
  then uses `IVAL/5+MOD(IVAL,5)`; imperial uses integer inches and
  `IVAL/2+MOD(IVAL,2)`. Both clamp to 1–20 and fold classes 11–20 into
  summary class 10. BC's formula is non-monotonic and disagrees with the
  five-centimetre headings (bug 27); retain it during structural work.
  DMRMIN remains in inches. DCTPA/DCINF/DCMRT are accumulated per acre
  and divided by ACRtoHA for text and DBSMIS3; DCDMR/DCDMI ratios are
  unchanged. Labels use TPH and metric class headings; shared DBSMIS3
  selects `FVS_DM_Sz_Sum_Metric`. BC continues without infected trees when
  NEWMOD is true and calls its extra DBSMIS5/6 writers under PRTMIS/NEWMOD.
  Keep those state/output gates. Species, stand-composite, detail and NISI
  database reports require separate audits; bug-22 FORMAT 905 is untouched.
- [x] **M13a — Share diameter-class accumulation (implementation).**
  Completed 15 November 2026 in `mistoe/misclass_mod.f90`. The per-tree
  helper preserves both index formulas (including bug 27), threshold,
  mortality input and accumulation order, and returns P for the caller's
  following stand-volume calculations. Report selection, folding, formatting
  and BC's spatial writers stay in the callers. All 1,120 species/diameter/
  infection/threshold cases per tested consumer match both references.
- [x] **M14 — ESUCKR: sprout height/DBH calculation (audit).** Compared
  the block from `PROB(ITRN)=PREM*SMULT` through `ICR(ITRN)=70` with both
  selected imperial counterparts: `estb/esuckr.f90` (AK/CI/EM/IE/KT) and
  `strp/esuckr.f90` (the other 17 imperial variants). Arithmetic and call
  order agree: SPRTHT, height multiplier, BACHLO rejection outside [-1,1],
  height perturbation by HT/5.5, the 4.5-foot breast-height breakpoint,
  logarithmic DBH equation with the existing HT1/AA choice and 0.1-inch
  floor, then crown ratio 70. The calculation assumes PREM/PROB per acre,
  SITEAR/HTI/HT in feet and DSTUMP/DBH in inches; ESTUMP stores unconverted
  DBH and PREM.
  Only later reporting converts density/height to per hectare/metres.
  BC/ON select the four-argument VSTRP ESSPRT; ESTB's five-argument VESTB
  routine also receives NUMSPR. Preserve that caller distinction. BC's
  ESHAP layout also remains distinct. Outside the audited block, metric
  omits newer tree-field initialization and lacks STRP's zero-total report
  guard; ESTB also lacks that guard (bug 29). These are not unit differences.
- [x] **M14a — Share sprout size calculation (implementation).**
  Completed 15 November 2026 in `estb/essize_mod.f90`, called by all three
  ESUCKR files. Preserved arithmetic order, native units, thresholds, crown
  ratio and metric diagnostic spacing. Checks use real SPRTHT, BACHLO and
  ESRANN, comparing numeric bits and final generator state for every species
  in the tested variants. Species mapping, ESSPRT interfaces, initialization,
  report tails (including bug 29) and common-block layouts are unchanged.

Verification for M08–M14: selected all consumers from the 24 current source
lists; checked the scoped statements against F77 `c2105365` and HEAD
`ed5e92fb`. Temporary GNU Fortran 15.2.0 block drivers compare exact text
and numeric bits: 16 DWDVLOUT cases per copy, seven converted values/six
detailed-snag rows, 36 burn-report cases per copy, eight first-cycle fire
condition rows, 100 metric mistletoe class indices, and 90 sprout-size cases
for all three original/current copies, including random draw counts.
Separate controls reproduce the DMBCI diagnostic error and zero-total sprout
division in original/current code. These are scoped block checks with supplied
callee results, not integrated simulations or whole-file equivalence. Source
lists and `git diff --check` pass; the full simulation suite was not rerun
for this documentation-only change. P03 and bug 5 remain unresolved. All
temporary drivers, executables and outputs were discarded.

Verification for M08a/M09a–M14a on 15 November 2026: seven modules with
explicit caller interfaces are selected by all applicable manifests (24
consumers each, except MISTPRT/MISCLASS with 19). All 24 variants build;
the source-list check, all-variant CMake configuration and `git diff --check`
pass. The original-reference suite ends with `FAIL 23/24 variants; working
tree e89db818+changes vs original c2105365; failed: bc`: all 22 US variants
match, and ON matches its original keyword-input failure. BC now reads
keywords after the isolated buffer repair, whereas pristine F77 cannot.
Its 11 generated files match the immediate baseline with only the same
six buffer declarations repaired. StandStructure still raises SIGILL in
both of those builds; YSM-SkyRanch now produces a summary. This is controlled
baseline agreement, not pristine-original BC simulation parity.

Direct original/baseline/candidate checks cover BC/ON/IE/SN: 24 DWDVLOUT
cases, 32 snag-output cases, 256 burn-report cases and 32 potential-condition
calls per consumer; BC/IE additionally cover 20 MISTPRT and 1,120 MISCLASS
cases. Sprout checks cover every species with five ages, four multipliers,
both height-equation flags and both debug settings, using real random and
height routines. All numeric bits, arrays, relevant common state, text and
random-generator state match. The one known F77 text difference is the
pre-existing metric snag-heading apostrophe (bug 22); its immediate-baseline
text is preserved exactly. Seven temporary controls changing one branch or
factor, one per helper, are all detected. Full BC MISIN checks use the real
twelve-field KEYRDR for 20 cases, guard both caller buffers, and compare
NEWMOD reset/enable and all 30 MISKEY slots against minimally widened F77
and baseline readers. Temporary drivers, builds and outputs were discarded.

- [x] **M15 — DISPLY: removal/residual tree-distribution record (audit).**
  Completed 15 November 2026: FORMAT 9004 and its two writes have the same
  layout as `vbase/disply.f90`. CUTS supplies ONTREM/ONTRES: positions 1–5
  are percentile diameters, 6 is maximum diameter, and 7 is trees per acre.
  Metric multiplies the six diameters by INtoCM and divides density by
  ACRtoHA; the four species percentages/codes and labels are unchanged.
  Neither writer changes these arrays. ITABLE(1), LSTART and positive
  ONTREM(7) gate both records identically; blank lines remain in the caller.
  Volume, accretion, current-stand, binary sample-tree and summary records
  are outside this audit.
- [x] **M15a — Share the FORMAT 9004 writer (implementation).** Completed
  15 November 2026 in `vbase/disptrees_mod.f90`, called for removal and
  residual records by both DISPLY copies. The helper converts only printed
  values, retains the format and species order, and leaves input arrays in
  native units. Gates, blank lines and all other DISPLY records stay in
  their callers, including the existing summary-array stride (bug 30).
- [x] **M16 — FVSSTD: normal-precision live-tree record (audit).** Completed
  15 November 2026: compared the first FORMAT 21 write with `vbase/fvsstd.f90`.
  Field order, descriptors and the `P/DP < 9999.9995` selection agree.
  Metric converts P/DP to trees/ha; DBH, diameter increment and PDBHI to cm;
  heights, height increment, crown width and PHTI to m; point BAL uses
  `NINT(PTBALT*FT2pACRtoM2pHA)`. Truncation/normal heights use
  `NINT(FLOAT(value+5)*.01*FTtoM)`, not imperial integer division by 100.
  Current volume fields use CFV/WK1/BFV and old volumes use
  PTOCFV/PMRCFV/PMRBFV, all times FT3toM3. Imperial uses CFV/MCFV, except
  CS/LS/NE/SN select MCFV/SCFV and the corresponding old-volume arrays;
  board feet remain unconverted. IDs, crown ratio, mistletoe, defects,
  percentile, YRDLOS, birth year and plot ID are unchanged.

  The requested Ontario cord-conversion check resolves to its volume-scaling
  contract: `canada/on/varvol.f90` labels the default OBFVOL result NMV,
  not cords. `canada/on/vols.f90` divides BBFV by FT3toM3 for METHB != 8,
  compensating for the output multiplication; method 8 keeps cubic-foot NMV.
  Do not remove either factor or reinterpret BFV solely from its name.
  OPFIND/OPGET selection, tree order, PDBH/PHT resets and volume preparation
  remain caller responsibilities. Header, FORMAT 20 and dead-tree output
  need separate audits before a whole FVSSTD merge.
- [x] **M16a — Share the first FORMAT 21 writer (implementation).** Completed
  15 November 2026 in `vbase/fvsstdrec_mod.f90`. Only the first live-tree
  write calls the helper. Imperial callers retain their four selected
  current/old cubic-volume values; metric callers explicitly pass
  CFV/WK1/PTOCFV/PMRCFV. LMETRICVAR selects the original conversions and
  integer-height arithmetic, retaining ON's compensating VOLS scaling.
  Scheduling, density-format selection, tree order and PDBH/PHT resets
  remain in FVSSTD. Header, FORMAT 20 and dead-tree writes are unchanged;
  FORMAT 21 remains in the callers because the dead-tree writer uses it.

M08d/M08e/M15a/M16a verification at immediate baseline `7691150f`:
all 24 variants select all four canonical helpers (96 manifest additions)
and build successfully. The whole original-reference suite ends with
`FAIL 23/24 variants; working tree 7691150f+changes vs original c2105365; failed: bc`.
The BC difference is the existing M12a keyword-buffer repair, not a new
extraction difference. The complete immediate-baseline comparison ends with
`PASS 24/24 variants; working tree 7691150f+changes vs immediate baseline 7691150f`.
All generated files, SQLite schemas/typed rows and restart files use the
existing exact comparator and its documented metadata/undefined-value rules.
ON still matches its original keyword-input failure; neither Canadian
variant gains pristine-original simulation acceptance from these checks.

Focused original-F77/immediate-baseline/candidate comparisons cover 48 cases
per request helper per variant, eight removal/residual records per variant,
and 40 normal-precision live-tree records per variant. They check landscape
rejection with echo on/off, existing/new IDs, negative/zero/positive years,
unchanged native arrays, first/last species, imperial eastern volume choices,
ON default/alternate volume scaling, near-threshold densities and integer
height rounding. Each of four temporary helper mutations is detected.
Expanding the new calls back to their baseline blocks reproduces every
remaining caller statement, including gates and the other record types.
Source-list checks and `git diff --check` pass. Common blocks, scientific
equations, FMDOUT, Ontario VOLS, reference outputs and both pinned NVEL
revisions are unchanged. Temporary drivers, builds and results were discarded.
- [x] **M17 — INITRE: buffer contract and DESIGN family (audit).** Completed
  15 November 2026: only ON selects `metric/vbase/initre.f90`; its
  ARRAY/LNOTBK/KARD still have seven elements, while KEYRDR/KEYDMP access
  twelve (bug 5). BC's separate INITRE buffers are already widened by M12a.
  This audit does not repair ON's undefined calls or merge either reader.
  Compared option 10, labels 2000–2030, with `vbase/initre.f90`: metric
  converts negative BAF and inverse plot area with `/HAtoACR`, nonnegative
  BAF with M2pHAtoFT2pACR, and break DBH with CMtoIN. These ARRAY conversions
  precede the LNOTBK-controlled assignments. Plot counts, SAMWT fallback,
  stocking fraction/percent normalization and XXG calculation agree.
  Echo converts BAF by sign, FPA by /ACRtoHA and BRK by INtoCM; its FORMAT
  and final-design notice agree. The PROCESS default-design echo also uses
  FORMAT 2010 and the metric XBAF preparation, so that format must remain
  available until its other caller is migrated. Other keywords are unaudited.
- [x] **M17a — Repair ON's keyword buffers (implementation prerequisite).**
  Completed 15 November 2026: only ARRAY/LNOTBK/KARD declarations change
  from seven to twelve in metric INITRE. Guarded real KEYRDR/DESIGN cases
  match explicitly defined twelve-field F77 and baseline fixtures. ON's
  whole suite matches the immediate baseline with only these same three
  declarations widened; this is not pristine-original simulation parity.
- [x] **M17b — Share the DESIGN handler (implementation).** Completed
  15 November 2026 in `vbase/initdesign_mod.f90`, with explicit interfaces
  in imperial and metric INITRE. Conversion/mutation order, seven consumed
  fields, presence tests, sampling defaults, stocking and both echo layouts
  are preserved. PROCESS retains its local FORMAT 2010 and finalization.
- [x] **M17c — BC INITRE: DESIGN family (audit).** Completed 15 November
  2026 at `62db12b5`: compared option 10, labels 2000–2030, in
  `canada/bc/initre.f90` with metric/shared INITRE and original F77
  `c2105365`. Only BC selects this reader. Its DESIGN block is
  statement-equivalent to metric/ON and its original F77 block; the shared
  imperial block has the unit-layer differences already mapped in M17.
  BC now has twelve ARRAY/LNOTBK/KARD entries (M12a), but DESIGN still
  consumes only the original seven fields:
  - Fields 1–3 set BAF, FPA and BRK: negative BAF and inverse plot area
    use /HAtoACR, nonnegative BAF uses M2pHAtoFT2pACR, and DBH uses
    CMtoIN. ARRAY conversions precede the presence-controlled assignments.
  - Fields 4–6 set IPTINV/NONSTK using IFIX and SAMWT directly. Nonpositive
    SAMWT falls back to FLOAT(IPTINV) when that plot count is positive.
  - Field 7 accepts a fraction in (0,1] or a percentage in (1,100], with
    the original multiplication by .01. Other values retain GROSPC.
    This field is tested without LNOTBK(7). XXG previews the fraction
    from plot counts when GROSPC remains negative; it does not finalize
    GROSPC. Fields 8–12 are unused by this handler.

  All seven state variables are in shared PLOT, with native-unit defaults
  supplied by BC GRINIT before the first handler. DESIGN reads no BCPLOT
  symbols and calls no BEC/HABTYP or growth-model routines. The enclosing
  BC reader still owns its ecological state and twenty-character habitat
  fields. PROCESS owns final plot counts, the automatic stocking fraction,
  the final DESIGN echo through FORMAT 2010, and the subsequent GROSPC
  inversion. Keep those steps, BC's separate report layouts and its BEC
  handling outside this extraction; FORMAT 2010 still has two callers.

  Temporary bounds-checked fixtures use the real twelve-field KEYRDR and
  1,296 cases per copy, covering blank/partial fields, BAF signs, fractional
  plot counts, sampling-weight fallback, stocking boundaries, echo on/off
  and populated unused fields 8–12. Native original-F77, current BC and
  current metric blocks produce identical text and state bits, with all
  buffer guards intact. A deliberate DBH-factor mutation is detected.
  Fixtures supply defined twelve-field buffers; they bypass original
  INITRE's seven-field overflow and establish no Canadian simulation parity.
  Only this tracker changes; source lists and `git diff --check` pass.
  No full-suite rerun is needed for this audit. Temporary files were discarded.
- [x] **M17d — BC DESIGN caller migration (implementation).** Completed
  15 November 2026: BC option 10 calls INITDESIGN; its manifest selects the
  helper. Guarded BC cases match native F77 and the immediate baseline,
  with no changes to ecological state, PROCESS or local FORMAT 2010.
  All eleven BC suite files match the immediate baseline; P03 remains.
- [x] **M18 — SUMOUT: cycle-summary data record (audit).** Completed
  15 November 2026: compared FORMAT 20, with the identical disk FORMAT 9014,
  against `vbase/sumout.f90`. DISPLY supplies rounded imperial-unit IOSUM
  integers plus SUMTAB density, basal area, height, QMD and MAI arrays.
  Metric applies INT after area/height/volume conversion to those already
  rounded integers; QMD and MAI remain real conversions. Year, age, CCF,
  period length and classification codes do not change. All six current/
  removal volume fields use FT3pACRtoM3pHA, including ON's BFV-derived NMV.
  Imperial additionally writes IOSUM(21/22) saw-cubic fields: 29 data items
  versus metric's 27, with nine versus seven consecutive I6 descriptors.
  LPRT/LDSK gate the two text destinations; ICALLF=0 independently permits
  DBSSUMRY, receiving metric conversions exactly once. Headings and file
  opening are outside this record extraction.

  The shared OUTCOM declares IOSUM(22,MAXCY1), but metric DISPLY passes
  I20=20 to SUMOUT's IOSUM(I20,LENG). Rows after the first therefore use
  the wrong stride (new bug 30); this is present in F77 too. Preserve that
  caller contract during extraction and repair it separately. The distinct
  28/30-argument DBSSUMRY interfaces must also remain paired (M21).
- [x] **M18a — Share the cycle-summary text writer (implementation).**
  Completed 15 November 2026 in `vbase/sumoutrec_mod.f90`. Both SUMOUT
  callers retain destination gates, headings and their DBSSUMRY calls.
  The helper preserves both field sets, INT/conversion order and supplied
  IOSUM leading dimension, including bug 30. Full-writer cases compare
  printed/disk files and captured database arguments against both references.
- [x] **M19 — DBSFMPF: fuel-model/weight output group (audit).** Completed
  15 November 2026: compared the four moderate IDs/weights and optional four
  severe IDs/weights with `vdbsqlite/dbsfmpf.f90`. Both bind IDs as integers
  and weights as `DBLE(INT(weight*100.+0.5))`; weights are percentages, with
  the original REAL arithmetic before DBLE. These fields use no unit factor.
  Metric selects East for ON, while imperial selects East for CS/SN; BC and
  the other imperial consumers use West. East appends the severe group;
  CREATE and INSERT list orders differ, but named columns pair correctly
  with the moderate-then-severe bindings. Metric uses the `_Metric` tables.
  Earlier West-only fire fields change the starting ColNumber, so a helper
  must accept/update that index. IPOTFIRE=0 returns, =2 suppresses text via
  KODE; DBSCASE, SQL prepare/step/finalize and failure handling stay in the
  caller. FMPOFL prepares dimensional fields before this writer; do not add
  conversions here. Other potential-fire fields remain outside this audit.
- [x] **M19a — Share fuel-model/weight bindings (implementation).**
  Completed 15 November 2026 in `vdbsqlite/dbsfmpfbind_mod.f90`. Callers
  retain REAL percent rounding before DBLE, East/West selection, schemas,
  other fields and statement lifetime. The helper advances the supplied
  column index in the original moderate-then-severe order and never reads
  uninitialized West severe weights. Full SQLite routines match native F77
  and baseline status, schemas and typed rows for IE/CS/BC/ON.
- [x] **M20 — DBSSTANDIN: sampling-design fields (audit).** Completed
  15 November 2026: compared BASAL_AREA_FACTOR, INV_PLOT_SIZE and BRK_DBH
  read/assignment/echo blocks with `vdbsqlite/dbsstandin.f90`. Both read
  REAL values into RSTANDDATA(10:12), mark presence before conversion using
  NullReal, and leave state unchanged for SQL NULL/absent fields. Metric
  converts BAF by sign (`/HAtoACR` for negative, M2pHAtoFT2pACR otherwise),
  inverse plot area by /HAtoACR and DBH by CMtoIN; imperial stores inputs
  directly. The separate assignments depend on the original presence flags.
  Metric echoes FPA/ACRtoHA and BRK*INtoCM, but always echoes BAF/ACRtoHA:
  positive area-factor input 4.5 prints 48.4 despite correct internal BAF
  (new bug 32). Preserve that diagnostic in cleanup. Shared DBSIN invokes
  this reader for selected stand SQL; query/column dispatch, overlaid integer
  storage, BC BEC/HABTYP handling and other fields remain in the reader.
- [x] **M20a — Share sampling-design field handling (implementation).**
  Completed 15 November 2026 in `vdbsqlite/dbsstanddesign_mod.f90`.
  SQL traversal and reads stay in the callers; presence is tested before
  conversion, including the original NULL-sentinel conversion. Assignments
  and echo preserve exact factors and bug 32. Full SQLite-reader cases
  match F77/baseline state bits and echo for IE/BC/ON; BC ecological
  processing and the overlaid stand arrays remain in the reader.
- [x] **M21 — DBSSUMRY: ordinary Summary record (audit).** Completed
  15 November 2026: compared DBSSUMRY only, not DBSSUMRY2, with
  `vdbsqlite/dbssumry.f90`. It runs only for ISUMARY=1 and receives converted
  metric values from SUMOUT; it must not convert them again. Metric has
  28 arguments and 27 placeholders; imperial has 30/29, adding saw-cubic
  volume/removal arguments. Year/age, densities, basal areas, SDI, CCF,
  heights, volumes, period, accretion/mortality and classification use integer
  binds; QMD, after-treatment QMD and MAI promote REAL to DOUBLE PRECISION
  before binding. Metric tables use Tph, metre/cm/m3 units versus imperial
  Tpa, foot/inch/ft3 and board-foot fields. Imperial also adds missing
  SCuFt/RSCuFt columns to existing tables; metric has no equivalent migration.

  BC's FVS_Summary_Metric uses BdNA/RBdNA for its legacy third volume slots.
  ON chooses FVS_Summary_East_Metric; its CREATE defines TCuM/MCuM/CCuM
  and RTCuM/RMCuM/RCCuM, while INSERT requests MCuM/SCuM/NCuM and
  RMCuM/RSCuM/RNCuM. A fresh table rejects preparation, sets ISUMARY=0
  and writes no row (new bug 31, also in F77). A future schema correction
  must be separate from sharing bindings. Case identity, stand ID, schema,
  argument order, error exits and statement lifetime stay explicit.
- [x] **M21a — Share the ordinary Summary binding prefix (implementation).**
  Completed 15 November 2026 in `vdbsqlite/dbssumrybind_mod.f90`.
  Year through QMD retain seven integer binds, caller-promoted DOUBLE
  PRECISION QMD and the supplied column-index progression. Both public
  interfaces, volume fields, schemas and bug 31 remain; DBSSUMRY2 is
  byte-identical. Full SQLite comparisons include ON's fresh-table failure
  and an INSERT-compatible existing table that exercises its actual bindings.

Verification for M17a/M17b/M17d and M18a–M21a on 15 November 2026:
all five modules have explicit interfaces and are selected by all 24 source
lists. All 24 variants build. The original-reference suite ends with
`FAIL 22/24 variants; working tree 62db12b5+changes vs original c2105365;
failed: bc on`. All 22 U.S. variants match every generated original-F77
output, including restart files. Both Canadian variants now pass the old
keyword-input failure: BC's eleven generated files match the immediate
baseline at `62db12b5`; ON's five files match that baseline with only
ARRAY/LNOTBK/KARD widened in its INITRE. BC StandStructure still fails
later. These controlled comparisons do not establish pristine-original
Canadian simulation parity.

Temporary bounds-checked GNU Fortran fixtures compare native F77,
immediate baseline and candidate: 1,296 guarded real KEYRDR/DESIGN cases
per IE/BC/ON copy; 32 full SUMOUT cases per IE/CS/BC/ON copy with three
rows, signed values, both destinations and captured database arguments;
12 full DBSFMPF/DBSSUMRY calls per IE/CS/BC/ON copy with disabled/text
switches and rounding-boundary weights; and 16 full SQLite DBSSTANDIN
cases per IE/BC/ON copy with NULL, absent, sentinel, reordered and signed
fields and echo on/off. ON bindings also match with an INSERT-compatible
existing schema; its fresh-schema failure is separately retained. Each
of five deliberate helper mutations is detected. Caller projection checks,
source-list validation, all-variant CMake configuration and `git diff --check`
pass. Common blocks, equations outside the extracted blocks, reference
outputs and both pinned NVEL revisions are unchanged. Temporary fixtures,
builds, databases, suite outputs and results were discarded.

M08b/M08c and M15–M21 were audited at `6dcf25a6` against original main
`c210536536d81fd9b46dccb9a571f0de1e55e944`; the pinned original NVEL
`38548071d5aa652bb90c7f111f86b427f798a1c9` and current NVEL `a73e697e`
were checked locally. No volume-library code was needed by the isolated
drivers. All named metric/shared pairs serve BC/ON and the 22 U.S. variants,
respectively, except INITRE: ON selects metric, BC selects its separate file,
and the 22 U.S. variants select shared INITRE. Shared FMDOUT serves all 24.

Verification is bounded audit evidence, not Canadian simulation acceptance:
48 cases per FMIN handler/copy compare rejection, echo, allocation and state;
four FORMAT 9004 distributions, the first FORMAT 21 write with ON method-8/
alternate scaling, 20 DESIGN cases per copy, signed cycle-summary records
and eight SQLite sampling-input cases per copy match original F77 exactly.
Actual DBSFMPF/DBSSUMRY routines linked to SQLite match original status,
schemas and typed rows for IE/CS/BC/ON, including ON's prepare failure.
Original/current SUMOUT with a two-column 22-row input reproduces bug 30.
Tests bypass INITRE's undefined seven-field calls; they do not repair them.
Source lists and `git diff --check` pass. Only this tracker and FOUND_BUGS
change; compiled inputs, model sources, reference outputs and NVEL are
unchanged. No full-suite rerun is required for these documentation-only
audits. Temporary drivers, databases, outputs and builds were discarded.

## D — Unbuilt copies and documented retirement

The files below were excluded from all 24 current source lists at the audit
baseline. They therefore cannot execute in those current products. Recheck
selected include dependencies and supported auxiliary targets before deleting.
Keep an active replacement even when it has the same routine name.

These are **deletion reviews**. For a scientific alternative, record meaningful
differences and an exact historical recovery reference before deciding to
retire it. Unknown historical intent is not evidence of abandoned research.

- [x] **D01 — `sn/sitsetOld.f90`.** Its header says it was replaced in Q2
  2023 and should be deleted after testing: apparently forgotten cleanup.
  Removed the unselected old implementation on 13 November 2026 after SN
  and targeted NC comparisons. No separate historical 2023 test record was
  found. The original source remains available in Git.
- [x] **D02 — `rd/rdrout.f90`.** Its header explicitly declares it obsolete
  after General Report Writer implementation in 2015. The old call is commented
  out in `base/fvs.f90`. Completed 13 November 2026: removed only the unselected
  obsolete file; preserved GENPRT and the selected `base/exrd.f90` ENTRY.
- [x] **D03 — `vbase/natcrz.f90`.** The
  [2021 retirement note](changeNotes/2021.07.28_ldavid.txt) says its cruise
  postprocessor was unavailable. Review only this unselected US implementation;
  keep `canada/bc/natcrz.f90`, which BC and ON still select. Completed
  13 November 2026: removed only the retired US implementation. The selected
  Canadian stub and historical notes are unchanged. Dormant Canadian source
  lists that still referenced it were removed with section R.
- [x] **D04 — `canada/on/findag_for.f90`.** Completed 13 November 2026:
  retired the unselected FINDAG alternative. ON keeps `canada/on/findag.f90`;
  The retired HTCALC call omitted basal area, DBH and diameter percentile;
  selected FINDAG also initializes AGET and HTMAX.
- [x] **D05 — `canada/on/unblnk.f90`.** Completed 13 November 2026:
  retired the unselected ON clone. Preserved its embedded-space semantics and
  historical purpose; original and current builds select `base/unblnk.f90`.
- [x] **D06 — `canada/bc/unblnk.f90`.** Completed 13 November 2026:
  independently verified BC exclusion and retired its clone. The shared
  utility and its existing space-removal behavior are unchanged.
- [x] **D07 — `metric/dbsqlite/dbstreesin.f90`.** Completed 13 November 2026:
  retired the older 30-argument reader. Shared DBSTREESIN remains selected;
  the live 33/30 mismatch stays open in `FOUND_BUGS.md`. Verification tracing
  distinguishes the retired reader from the live mismatched interface.
- [x] **D08 — `canada/fire/on/fmcrowe.f90`.** Completed 13 November 2026:
  retired the unselected ON alternative; preserved species mapping and
  interface differences. The live FMSVL2 mismatch remains unfixed.
- [x] **D09 — `base/myopen_pc.f90`.** Completed 13 November 2026:
  retired the unused compiler-specific alternative after checking current
  Make/Windows/CMake selection of shared MYOPEN. OPEN behavior differences
  and exact recovery identity are preserved.
- [x] **D10 — `canada/on/r9init.f90`.** Completed 13 November 2026:
  retired the unselected initializer. Preserved signature, merchantability,
  coefficient/DATA and full replacement differences against selected NVEL.
- [x] **D11 — `volume/r9clark_fvsMod.f90`.** Completed 13 November 2026:
  retired the formerly active workaround after tracing selection back to
  NVEL in 2025. Preserved its power guards and exact diff; a separate,
  unconfirmed numerical concern is logged for later investigation.

Original sources remain in Git: use `git show 3b26bc36:<path>` for D01–D03
and `git show 9bc35a16:<path>` for D04–D13. Review the original implementation
before reintroducing any historical scientific alternative.

### D12 — Five separate ESTUMP copies

Every current variant selects `estb/estump.f90`. These five completed reviews
preserve individual scientific differences and keep that shared implementation.

- [x] **D12a — `ca/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 44 and bookkeeping differences against shared ESTUMP.
- [x] **D12b — `cr/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 20 in its original Git version.
- [x] **D12c — `tt/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 6 in its original Git version.
- [x] **D12d — `ut/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 6 in its original Git version.
- [x] **D12e — `wc/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 26 in its original Git version.

### D13 — DMCW alternatives, one file per task

These retired files defined unselected DMCW implementations. Selected
`canada/newmist/dmcw.f90` still uses `CRWDTH`; the BC mistletoe pathway remains
active. Each file's equations and DATA tables remain available in its original
Git version. No explicit abandonment decision was found
for these historical regional alternatives. D13 retains the original audit's
ID for Alaska. All fifteen retirements completed 13 November 2026.

- [x] **D13 — `canada/newmist/dmcwak.f90`.** Retired; Alaska coefficient tables preserved.
- [x] **D13-02 — `canada/newmist/dmcwbm.f90`.** Retired; BM tables preserved separately.
- [x] **D13-03 — `canada/newmist/dmcwci.f90`.** Retired; CI tables preserved separately.
- [x] **D13-04 — `canada/newmist/dmcwcr.f90`.** Retired; CR tables and commented subvariant history preserved.
- [x] **D13-05 — `canada/newmist/dmcwec.f90`.** Retired; EC tables preserved separately.
- [x] **D13-06 — `canada/newmist/dmcwem.f90`.** Retired; EM tables preserved separately.
- [x] **D13-07 — `canada/newmist/dmcwib.f90`.** Retired; CRWDTH-copy and declaration differences recorded; no coefficient DATA.
- [x] **D13-08 — `canada/newmist/dmcwkt.f90`.** Retired; KT tables preserved separately.
- [x] **D13-09 — `canada/newmist/dmcwnc.f90`.** Retired; NC tables preserved separately.
- [x] **D13-10 — `canada/newmist/dmcwni.f90`.** Retired; NI tables preserved separately.
- [x] **D13-11 — `canada/newmist/dmcwso.f90`.** Retired; SO tables preserved separately.
- [x] **D13-12 — `canada/newmist/dmcwtt.f90`.** Retired; TT tables preserved separately.
- [x] **D13-13 — `canada/newmist/dmcwut.f90`.** Retired; UT tables preserved separately.
- [x] **D13-14 — `canada/newmist/dmcwwc.f90`.** Retired; WC tables preserved separately.
- [x] **D13-15 — `canada/newmist/dmcwws.f90`.** Retired; WS tables preserved separately.

### D14 — Dormant ESUCKR copies: removed

Removed 14 November 2026 from the tree at `7eef343f` (recover with
`git show 7eef343f:<path>`). None of the nine was in a source list here or in
upstream at `c2105365`. Each is an older version of the routine its variant
now builds: it computes aspen sucker density and sprout counts in line, with
that variant's species numbers written into the code, where the built routine
calls the shared `ESASID`, `ASSPTN`, `NSPREC` and `ESSPRT`. The two Canadian
copies call `NSPREC` and `ESSPRT` with argument lists those routines no longer
accept. The seven US copies were last edited upstream by the FIA change of
2025-08-28 (`08e833b2`).

| Removed copy | Routine the variant builds | Variant detail kept only in Git |
|---|---|---|
| `ci/esuckr.f90` | `estb/esuckr.f90` | Aspen as species 13 |
| `em/esuckr.f90` | `estb/esuckr.f90` | Site-based sprout height for species 11–17 |
| `ca/esuckr.f90` | `strp/esuckr.f90` | Aspen as species 44 |
| `cr/esuckr.f90` | `strp/esuckr.f90` | Aspen as species 20; multipliers for species 23–28 |
| `tt/esuckr.f90` | `strp/esuckr.f90` | Aspen as species 6 |
| `ut/esuckr.f90` | `strp/esuckr.f90` | Aspen as species 6 |
| `wc/esuckr.f90` | `strp/esuckr.f90` | Aspen as species 26 |
| `canada/bc/esuckr.f90` | `metric/strp/esuckr.f90` | Metric height–diameter dubbing |
| `canada/on/esuckr.f90` | `metric/strp/esuckr.f90` | 15 cm stump limit; random management code; `ONHTDBH` diameters |

- [x] **D14 — `ci/esuckr.f90`.** Removed.
- [x] **D14-02 — `ca/esuckr.f90`.** Removed.
- [x] **D14-03 — `cr/esuckr.f90`.** Removed.
- [x] **D14-04 — `em/esuckr.f90`.** Removed.
- [x] **D14-05 — `tt/esuckr.f90`.** Removed.
- [x] **D14-06 — `ut/esuckr.f90`.** Removed.
- [x] **D14-07 — `wc/esuckr.f90`.** Removed.
- [x] **D14-08 — `canada/bc/esuckr.f90`.** Removed.
- [x] **D14-09 — `canada/on/esuckr.f90`.** Removed.

### D15 — Remaining unbuilt single files: removed

- [x] **D15 — Eight files in no source list.** Removed 14 November 2026 from
  the tree at `7eef343f`. None is in a source list here or in upstream at
  `c2105365`.

| Removed | Lines | Why it was dead |
|---|---:|---|
| `rd/rdinca.f90`, `rd/rdinitca.f90`, `rd/rdblk1ca.f90`, `rd/RDPARMCA.f90` | 4,106 | Root disease routines sized for the 49-species CA variant. CA does not link the root disease model; the variants that do build `rd/rdin.f90`, `rd/rdinit.f90`, their own `rd/rdblk1xx.f90` and `rd/RDPARM.f90` |
| `canada/bc/estab.f90` | 812 | BC builds `strp/estab.f90` |
| `canada/bc/esinit.f90` | 71 | BC builds `strp/esinit.f90` |
| `canada/bc/esetpr.f90` | 133 | Every variant builds `estb/esetpr.f90` |
| `canada/bc/bcspp.f90` | 331 | Species-code lookup `BCSPP`; nothing calls it |

Checks for D14 and D15: all 24 source lists are byte-identical, the one
matching entry in `bin/fortran_includes.txt` was removed, and
`tests/parity/check_suite.py` passes for all 24 variants.

## R — Unbuilt directories: removed

The maintainer decided to remove code that no build uses, as in the earlier
reverted commit `289fca18`. On 14 November 2026, 496 files (135,361 lines) were
removed from the tree at `0245846c`. None was in any of the 24 source lists,
and none is in any upstream source list at `c2105365`. The 41 matching entries
in `bin/fortran_includes.txt` were removed with them.

Checks: all 24 source lists are byte-identical before and after, every list
entry exists, the include/compile classification of every listed file is
unchanged, and all 24 variants build. No compiled input changed, so no output
comparison was needed.

To recover a file, use `git show 0245846c:<path>` for the free-form version,
or the same path with its original extension at upstream `c2105365` for the
fixed-form original.

| Removed | Files | Lines | Why it was dead |
|---|---:|---:|---|
| `wsbwe/` (budworm) | 61 | 17,996 | Left the last upstream source list on 2022-07-18 (`4cc51a97`); variants link the `base/exbudl.f90` stub |
| `wwpb/`, `metric/wwpb/` (westwide pine beetle) | 63 | 19,080 | Left the last upstream source list on 2012-05-22 (`939005c2`); variants link `base/exbm.f90` |
| `wpbr/` (blister rust) | 44 | 8,680 | Left the last upstream source list on 2022-01-16 (`b24f98fe`); variants link `base/exbrus.f90` |
| `lpmpb/` (mountain pine beetle) | 58 | 6,002 | Left the last upstream source list on 2022-01-16; variants link `base/exmpb.f90` |
| `dftm/` (tussock moth) | 40 | 5,141 | Left the last upstream source list on 2022-01-16; variants link `base/exdftm.f90` |
| `dfb/`, `metric/dfb/` (Douglas-fir beetle) | 31 | 4,137 | Left the last upstream source list on 2022-07-18; variants link `base/exdfb.f90` |
| `dbs/`, `vdbs/`, `metric/dbs/`, `metric/vdbs/` (ODBC database) | 58 | 18,410 | Left the last upstream source list on 2022-07-18; every variant uses `dbsqlite/` |
| `archive/bgc/` | 31 | 5,053 | Archived upstream 2018-05-15 (`9a866907`); the [2021 note](changeNotes/2021.07.28_ldavid.txt) records that the project ended |
| `archive/ni/`, `archive/ak/`, `archive/fire/`, `archive/PPEcommons/` | 60 | 14,956 | Archived upstream; never in a source list from that location |
| `metric/rd/`, `metric/ppbase/` | 14 | 20,486 | Metric copies never in any upstream source list |
| Other unbuilt `metric/` copies (`ak`, `base`, `cr`, `estb`, `ls`, `mistoe`, `ne`, `ni`, `ut`, `vbase`, `wc`) | 20 | 8,809 | In no source list; the imperial or shared routine is the one built |
| `canada/bin/` | 2 | 1,235 | Historical source lists that no recipe reads |
| Single unbuilt files: `base/exdbs.f90`, `exdbsqlite.f90`, `exestb.f90`, `exfire.f90`, `exsv.f90`; `canada/on/cuts.f90`, `fvsstd.f90`, `update.f90`; `fire/fofem/ansi_mai.c`, `fof_disp.c`; `mistoe/misintni.f90`; `rd/rdppatv.f90`; `sn/formcl.f90`; `vls/vols.f90` | 14 | 5,376 | In no source list; another file supplies the routine, or the real extension is always linked so its stub is unused |

The list-removal dates come from the last upstream commit that changed how
often each directory appears in `bin/` source lists. Apart from BGC, no change
note was traced for the individual extensions.

- [x] **R01 — BGC.** Removed with `archive/bgc/`. ON still selects the
  `base/exbgc.f90` stub, which is unchanged.
- [x] **R02 — ODBC.** Removed `dbs/`, `vdbs/` and their metric copies.
- [x] **R03 — Douglas-fir beetle (`dfb/`).** Removed; stub unchanged.
- [x] **R04 — Tussock moth (`dftm/`).** Removed; stub unchanged.
- [x] **R05 — Mountain pine beetle (`lpmpb/`).** Removed; stub unchanged.
- [x] **R06 — Blister rust (`wpbr/`).** Removed; stub unchanged.
- [x] **R07 — Budworm (`wsbwe/`).** Removed; stub unchanged.
- [x] **R08 — Westwide pine beetle (`wwpb/`).** Removed; stub unchanged.
- [ ] **R09 — Acadian model (`acd/`, 27 files).** Not removed: the reverted
  commit kept it. Unbuilt; needs a keep-or-remove decision.
- [x] **R10 — Northern Idaho archive (`archive/ni/`).** Removed.
- [x] **R10b — Alaska archive (`archive/ak/`).** Removed.
- [x] **R10c — Fire archive (`archive/fire/`).** Removed.
- [x] **R10d — Parallel-processing declarations (`archive/PPEcommons/`).**
  Removed. Active stop/restart support in `pg/` is unchanged.

## U — Unreachable assignments inside active stubs

The audit counted 102 constant-true return sites across 15 selected files.
Reviewed examples put unused-argument warning-suppression assignments after
`if (.true.) return`. That explains their presence; it does not make the
procedure or its ENTRY points disposable.

Each item is an **audit followed by a narrowly scoped implementation** only
after the compiler-warning treatment is settled. Trace labels and ENTRY flow
before calling a statement unreachable. Preserve signatures, entry points,
default output flags, and unsupported-keyword behavior. Record whether the
outcome is removal or deliberate retention of the suppression idiom.

The six checked stub tasks below were completed together on 14 November 2026:
`PASS 24/24 variants`, subject to P03. Live ENTRY signatures, default flags and
suppression idioms are retained; the other nine stub tasks remain pending.

- [ ] **U01 — `base/exdfb.f90`.** Review suppression assignments only.
- [x] **U02 — `base/exdftm.f90`.** Removed unused TMHED and its
  suppression declarations.
- [x] **U03 — `base/exmpb.f90`.** Removed unused MPBHED and its
  suppression declarations.
- [ ] **U04 — `base/exbrus.f90`.** Review suppression assignments only.
- [x] **U05 — `base/exrd.f90`.** Removed unused RDROUT/RDPPATV and their
  dead declarations.
- [ ] **U06 — `base/exmist.f90`.** Review suppression assignments only.
- [x] **U07 — `base/exbm.f90`.** Removed unused PPE/RR entries; retained
  BMPPIN because BMIN references its dummy arguments (bug 13).
- [ ] **U08 — `base/execon.f90`.** Review suppression assignments only.
- [x] **U09 — `base/exppe.f90`.** Removed the uncalled stub and BC/ON
  manifest entries; no selected source references its routines.
- [ ] **U10 — `base/exclim.f90`.** Review suppression assignments only.
- [ ] **U11 — `base/excov.f90`.** Review suppression assignments only.
- [ ] **U12 — `base/exorganon.f90`.** Review suppression assignments only.
- [x] **U13 — `base/exbudl.f90`.** Removed unused BWEPPATV/BWEPPGT/BWEPPPT
  and their suppression declarations.
- [ ] **U14 — `base/exbgc.f90`.** Review suppression assignments only.
- [ ] **U15 — `base/exfert.f90`.** Review suppression assignments only.

## B — Build and declaration cleanup

- [x] **B01 — CMake source form (build change).** Completed 14 November
  2026 with B02: use the free-form suffix for all selected Fortran sources.
  All 24 CMake configurations generated successfully in a temporary copy;
  integrated output verification uses the GNU make parity suite.
- [x] **B02 — Repeated make setup rules (build change).** Completed
  14 November 2026: one generated rule replaces 24 copies, with identical
  expanded prerequisites/recipes for all 48 Unix and 48 Windows targets.
  CMake accepts a variant list or `all`; existing source manifests are retained.
  `PASS 24/24 variants`; BC/ON retain the P03 keyword-input limitation.
- [x] **B03 — ESPARM folded into PRGPRM (implementation).** Completed
  14 November 2026 for all 25 existing files (24 built variants plus ACD).
  Parameter statements and scientific commentary are preserved; removed 102
  redundant includes and updated manifests/include classification. AK ESCOMN
  and BC ESHAP layouts remain distinct. `PASS 24/24 variants`; P03 still applies.

## L — Larger families:

These counts describe the originally selected files, not removable lines or
proven duplicate algorithms. L01–L03 now record their difference maps,
implementation and explicit retention decisions. L04 retains its complete
audit and separately verified implementation stages.

- [x] **L01 — PVREF6 (7 files).** Completed 15 November 2026 in
  `vbase/pvref6.f90` for BM/CA/EC/NC/OC/OP/PN/SO/WC. One 4,277-row union
  stores each PV/reference pair once, with seven separate habitat columns
  (CA/OC and OP/PN already shared native tables). Scientific habitat values
  remain variant-specific. An absent row is distinct from a real blank
  habitat; partial-match flags, leading blanks, unchanged ARRAY2 on a miss
  and BM's first-match blank-pair behavior are retained. Removed seven native
  copies: 18,088 lines become 4,463. Against both pinned F77 and the immediate
  baseline, all nine compiled variants pass 76,008 queries each, including
  every native pair, misses, partial matches and all initial flag states.
- [x] **L02 — MORTS (21 files).** Completed 15 November 2026 with 23 variants
  selecting `vbase/morts.f90`; AK explicitly retains `ak/morts.f90`.
  Shared families are SDI mortality (BM/CA/CR/CS/EC/EM/LS/NC/NE/OC/SN/SO/
  TT/UT/WS), Prognosis (CI/IE/KT), and Pacific (OP/PN/WC), with separate
  BC and ON kernels. MORCOF retains regional coefficients and species maps;
  PRGCON retains habitat/forest lookup banks. The species multipliers,
  diameter-period conversions, redwood/sequoia floors, SN rate, EM/TT hybrid
  paths, ORGANON, climate calls and diagnostic formats remain explicit.
  MORTMULT and FIXMORT processing are shared where their contracts agree.
  The public MORTS/MORCON entries are unchanged. Ontario's four-argument
  public VARMRT is retained in `canada/on/onmrt.f90`, with a local explicit
  interface distinguishing it from other variants' three-argument calls.
  Unread CI PMD and Pacific PMSC/PSP tables are not carried forward.
  Removed 20 superseded files: 20,212 lines become 4,190.

  **AK retention:** its original code reads KBIG/KPOINT outside the activity
  loop that defines them. A scratch extraction changed control flow on this
  undefined path, so that extraction was discarded. The native file and its
  selection are preserved. FOUND_BUGS section 37 records the defect and the
  separate prerequisite for any future AK extraction; no model repair is
  included here.
- [x] **L03 — REGENT (23 files).** Completed 15 November 2026 in
  `vbase/regent.f90` for all 24 variants, retaining public REGENT/REGCON
  contracts. Nineteen family kernels retain the regional models:

  - CA/OC share SMHTGF, height/diameter blending and calibration; OC retains
    its ORGANON tree flags, full-weight growth overrides and diagnostics.
  - CS/LS/NE share the height-age calculation, with native DIAM tables
    (96/68/108 slots), HTCALC equation selectors, LS tree age, CS BAL, NE
    crown rounding and its 1.5-inch blending threshold. A local adapter
    preserves NE's three-argument BALMOD beside CS/LS's six-argument forms.
  - OP/PN/WC share SMHGDG processing. OP retains one five-year step and
    ORGANON overrides; PN/WC retain two steps and a ten-year REGYR. Native
    redwood arithmetic, random draws and reporting columns are unchanged.
  - AK/BM/BC/ON/CI/CR/EC/EM/IE/KT/NC/SN/SO/TT/UT/WS retain separate native
    equation bodies, tables, types and initialization. BC's metric/V2/V3
    state and the differing SMHTGF/HTCALC/DUBSCR contracts are preserved.

  Local DATA arrays retain native extents; common blocks retain the selected
  variant's includes. REGCON enters without passing REGENT's absent dummy
  arguments, and its native RHCON, READCORR and ZRAND initialization remains
  in the corresponding kernel. Removed 23 superseded copies: 20,575 lines
  become 18,527. EM's existing DUBSCR mismatch and unset calibration indices
  are recorded in FOUND_BUGS sections 38–39, not repaired.

  **L01–L03 verification (15 November 2026, `7d02d6c7` + changes):** 382
  native DATA blocks match the immediate baseline, with NE's blending and
  PN/WC's period selectors checked separately. All 528 focused real-engine
  cases pass against that baseline (16 mortality and eight regeneration
  scenarios per US variant): model files, database rows, exit status and
  diagnostic text, plus 32 common-block fingerprints before/after 12,144
  routine calls. Scenarios cover mortality multipliers, FIXMORT modes,
  density/BA limits, thinning, period lengths, calibration, random growth,
  tripling, regeneration multipliers and size caps. Temporary comparison
  copies define only the unset diagnostic locals in FOUND_BUGS 37/39;
  TT's existing undefined IGL (bug 11) is masked during hashing and restored
  before model execution. Repository routines and comparators are unchanged
  by these controls. Planted habitat, mortality and growth changes are all
  rejected by the comparisons.

  The whole existing suite matches the immediate baseline for **24/24
  variants, 229 generated files**, including all 11 BC and five ON outputs.
  The pristine-original suite ends `FAIL 22/24 variants; working tree
  7d02d6c7+changes vs original c2105365; failed: bc on`: only the already
  documented Canadian keyword-buffer differences remain. This does not
  establish pristine-original BC/ON simulation parity. Source lists and
  `git diff --check` pass; ACD, NVEL, test inputs and reference outputs are
  unchanged. Main-repository Fortran files decrease from 1,455 to 1,408,
  a net reduction of 31,691 source lines. Temporary builds/results are
  discarded; no test archive or separate completion report is retained.
- [x] **L04 — DGF (24 files, complete audit).** Expanded at the user's
  request on 15 November 2026 at `b4b1ee3d`. All 24 selected implementations,
  all 979 native species slots, every DGF/DGCONS equation path, and delegated
  growth equations are mapped in the [complete audit below](#l04-complete-dgf-audit-and-generalization-plan).
  The initial CA/OC sequoia/redwood comparison is included in that map.
  All 24 statement/DATA/type/label/FORMAT projections match pinned F77;
  direct original/current probes reproduce three defects, now recorded in
  FOUND_BUGS sections 33–35. Source-list validation passes. This completes
  the source audit and plan, not implementation or whole-family runtime
  verification. All model code and build inputs remain unchanged.
- [x] **L04a — Share the CA/OC GS/RW growth block (implementation).**
  The Castle GS/RW block is `DGFRWGS` in `vbase/dgfeqs.f90` (equation core
  `RWGSDG`), called once by the shared Pacific body for CA/OC.
- [x] **L04b — Preserve the DGCONS signatures (implementation).** One public
  DGF/DGCONS in `vbase/dgf.f90`; the build selects `common/DGCONS_ENTRY.f90`
  (`entry DGCONS`) or EM's `em/common/DGCONS_ENTRY.f90` (`entry
  DGCONS(IDTYPE)`). RCON calls are unchanged. Each family is an internal
  procedure behind constant VARIANTCD guards, so its calls (DGFASP, BADIST)
  are compiled only into its own variants.
- [x] **L04c — Extend the GS/RW core (implementation).** OP/PN/WC use
  `DGFRWGS` through the Pacific body, WS calls it directly (its earlier PBAL
  is the same expression), and NC calls the core `RWGSDG` with its own
  PBAL<=0 fallback, diagnostics and halving.
- [x] **L04d — CA/OC ordinary equations (implementation).** CA/OC share one
  bank and one code path (`DGFPAC`); species-indexed DGDSQ/ATTEN, tanoak
  doubling and OC's halving kept. OBSERV is integer in all banks: its
  values are exact integers below 2**24, so the integer and real literals
  assigned to ATTEN were identical.
- [x] **L04e — Coastal ordinary equations (implementation).** OP/PN (one
  20-row bank) and WC (19 rows) joined `DGFPAC` with bank-indexed DATA;
  WC's forest remapping, equation-indexed DGDSQ/ATTEN (bug 33), ISPDSQ
  writes, MH/WO site handling, elevation cap and both 9030 formats kept.
- [x] **L04f — Red-alder calculation (implementation).** `DGFRA` in
  `vbase/dgfeqs.f90` for EC, SO, OP, PN and WC; each caller keeps its
  bark call, jump, diagnostics and period wrapper.
- [x] **L04g — Juniper polynomial (implementation).** `DGFJUN` (future
  diameter with the one-inch cap) for BM, CI, CR (GEMDG), EM, IE, SO, TT,
  UT (both uses) and WS; BA floors, stagnation and correction order stay
  with the callers.
- [x] **L04h — Cottonwood calculations (implementation).** `DGFCOA` (CI,
  EM, IE, CR with BAT) and `DGFCOB` (TT, UT, CR); the 36-inch ceiling and
  the 1.05 multipliers stay with the callers in their original order.
- [x] **L04i — Interior ordinary equations (implementation).** IE and EM
  share `DGFIE` (bank-indexed DATA; NI, EM, juniper, aspen and cottonwood
  groups; per-variant DGCONS habitat, slope, CCF and ATTEN rules). CI and
  KT moved unchanged into `DGFCI`/`DGFKT` with native table sizes; all 11
  shared-DGF manifests list CICOM and KOTCOM, which are unused elsewhere.
- [x] **L04j — TT/UT calculations and preparation (implementation).** TT
  and UT share `DGFTU`, including the age-range/stagnation preparation;
  TT PP, UT's PCCF term, species-5 multiplier, DGCCF(8) reset and site
  classes kept. CR's matching preparation remains for L04s.

  **L04a–j verification (15 November 2026, at `b4b1ee3d`+changes).** For
  each changed variant a scratch driver linked the variant's real objects
  with the old or new DGF, set random valid stand/tree/site states (300–600
  cases, debug on in a third, skipped diameters, PBAL fallback, AVH=0,
  LDCOR2/COR2 alternatives, all forests and habitat indices), called DGCONS
  and DGF for DBH and WK3, and hashed every common block after each call.
  All 17 changed variants matched bit for bit, including debug text, except
  the undefined diagnostic values recorded in FOUND_BUGS section 36.
  Planted changes (index, term order, PBAL test, UT multiplier, EM habitat
  limit, TT elevation-cap species, CI site coefficient) were detected.
  Original-F77 suite: PASS 22/24; BC/ON differ at the existing Canadian
  keyword-input boundary and none of their build inputs changed. `git diff
  --check` clean. `vbase/dgf.f90` replaces 11 native DGFs (6,212 lines)
  with 3,755; net Fortran change −2,364 lines.
- [x] **L04k — CS/LS stand preparation (implementation).** CS and LS share
  `DGFCLS` in `vbase/dgf.f90`: one DIAM>=5 weighted preparation (0.005454154,
  BAGE5 default 10) for both variants.
- [x] **L04l — CS/LS growth calculation (implementation).** One polynomial,
  bark conversion with current DBH and both -9.21 clamps, using two-column
  banks (CS 96 rows, LS 68; values copied verbatim). Each variant keeps its
  own QMD and CR cap lists in native order, and QMDGE5 is still lowered in
  place as species are visited.
- [x] **L04m — EC/SO regional calculations (implementation).** EC and SO
  moved unchanged into `DGFEC`/`DGFSO` with native table sizes (32/33).
  Their banks, forms, WSPR/PCCF switches and DGCONS rules stay separate, as
  the audit requires; they share only the existing DGFRA/DGFJUN/DGFASP.
- [x] **L04n — NC/WS remaining calculations (implementation).** NC and WS
  moved unchanged into `DGFNC`/`DGFWS` (12/43 rows): density fallbacks, RF
  routing, latitude/period handling, NC's -8.52 pre-clamp and WS's late
  DDSMAX (bug 35) kept.
- [x] **L04o — AK family kernel (implementation).** AK moved unchanged into
  `DGFAK` (23 rows); LPERM, YR, hardwood factors and DGCONS's addition to
  the incoming DGCON kept. Its PF MODIFIER diagnostic prints an unset
  PFCOMP2 for non-permafrost species (added to FOUND_BUGS section 36).
- [x] **L04p — SN family kernel (implementation).** SN moved unchanged into
  `DGFSN` (90 rows); the ordered IFORTP/PCOM selectors, IFOR=20 overrides
  and global BA/ICR defaults kept.
- [x] **L04q — NE annual kernel (implementation).** NE moved unchanged into
  `DGFNE` (108 rows); ten annual steps, BADIST/BALMOD calls and corrections
  kept.
- [x] **L04r — ON annual kernel (implementation).** ON moved unchanged into
  `DGFON`: 35-row bank, 72 mappings, quality pairs, metric iteration,
  disabled hybrid and bug 34 kept.
- [x] **L04s — CR/GEMDG integration (implementation).** CR's DGF is
  `DGFCR`; GEMDG is unchanged. Its age-range and stagnation preparation was
  statement-identical to TT/UT's and is now one internal `DGFSTG` called by
  `DGFCR` and `DGFTU` (SDICAL(1,...), FINT/FINTM scaling and DSTAG writes
  unchanged). CR's AGERNG initialisation now follows DBCHK, which reads only
  DBSTK.
- [x] **L04t — BC LV2 kernel (implementation).** BC moved unchanged into
  `DGFBC` with its local MD_STR/SS_STR types, 15-species LV2 tables, SEILTDG
  interface and V2SEICOR writes.
- [x] **L04u — BC V3 kernel (implementation).** The V3 lookup and prediction
  moved within `DGFBC` unchanged: 28/25 records, NUL sanitation, KAM
  ICHmw2 remapping, first-match order, `/all` fallback and no-match state.
- [x] **L04v — Complete DGF dispatch and inventory (build change).** BM moved
  into `DGFBM`; the host also includes METRIC and BCPLOT. All 24 manifests
  select `vbase/dgf.f90`, one DGCONS entry include (EM its own), CICOM and
  KOTCOM. The 13 superseded native DGFs (9,572 lines) are removed;
  `acd/dgf.f90` (R09) and the helpers GEMDG, BALMOD, DGFASP, SEILTDG and
  `vbase/dgfeqs.f90` remain.

  **L04k–v verification (15 November 2026, at `0e16036a`+changes).** Old
  (`0e16036a`) and new builds of the 15 changed variants (the 13 moved plus
  TT/UT for `DGFSTG`) were linked with one scratch driver: 600 random valid
  states each (BC 1,200, cycling all 196 V3 patterns under both LV2 and V3,
  plus the KAM remap and a no-match zone), every species slot, DBH boundary
  values, zero CR/BA/AVH/SLOPE, extreme COR, COR2=0 and LDCOR2, LSTART with
  recent mortality, SDI maxima, IFOR/IFORTP/PCOM/IMODTY/LPERM/IGL/TLAT/ICL5/
  IOSUM ranges and debug on in a third. DGCONS, DGF(DBH) and DGF(WK3) were
  called and every common block hashed after each call. All 15 matched bit
  for bit, as did stdout and debug text apart from unset diagnostic values
  (AK PFCOMP2, TT CR; FOUND_BUGS section 36). A planted change in every
  family was detected; NC's -8.52 pre-clamp moved to -8.53 is not observable
  behind the final -9.21 clamp, and moving it to -7.0 was detected.
  Original-F77 suite: PASS 22/24; BC/ON differ at the existing Canadian
  keyword-input boundary and match the immediate baseline in all 11/5 suite
  outputs. `git diff --check` clean. `vbase/dgf.f90` grew from 3,755 to
  12,759 lines; net Fortran change −568 lines.
- [x] **L05 — DGDRIV (22 files).** Consolidated in `vbase/dgdriv.f90` for
  all 24 variants; retained tables, calibration, ORGANON and output differences.
  Original-F77 parity: PASS 24/24; BC/ON additionally checked by statement diff.
- [x] **L06 — CRATET.** All 24 variants now use `vbase/cratet.f90`; removed
  the remaining 19 copies (14,325 active lines to 3,782). Retained regional
  equations, native data/types, call order, ORGANON, metric paths and output.
  Original-F77 suite: PASS 24/24 variants; 38 focused engine cases also match
  F77 and the immediate baseline. All 24 statement/format/label projections
  match; BC/ON diffs are empty. KT's existing undefined reads are recorded in
  `FOUND_BUGS.md` section 19 and retained. ACD is unchanged.
- [x] **L06a — TT/UT CRATET.** Joined CS/NE in `vbase/cratet.f90`; preserved
  site/height equations, native species indices, age/calibration order and output.
  All 246 species/profile statement comparisons and eight focused real-engine
  cases match the originals/baseline. Original-F77 suite: PASS 24/24 variants.

### L04 complete DGF audit and generalization plan

#### Scope, evidence and entry contracts

Audited 15 November 2026 at `b4b1ee3d`, following the user's expansion of
L04. The 24 current manifests select 24 distinct DGF files, totalling
15,838 physical lines and 979 native species slots. These are inventory
counts, not a forecast of removable code. Every native slot has a primary
equation classification below; conditional regional, size, management and
calibration paths are listed separately. Species indices refer to each
variant's actual BLKDAT/JSP order, including blank/reserved slots. Matching
species abbreviations across variants do not establish matching equations.

All 24 complete source projections match their own pinned F77 sources at
`c210536536d81fd9b46dccb9a571f0de1e55e944`: executable statements, DATA
initializers, declarations/types, labels and FORMAT/literal text. Comparison
accounts for free-form continuation, declaration syntax, include suffixes,
labelled-DO conversion and BC's equivalent split CHARACTER declarations.
Controls reject changed coefficients, diagnostic case, types and labels.
This establishes preservation of the inspected sources, not interchangeability
between variants or runtime coverage of every input combination.

The delegated `cr/gemdg.f90`, `ut/dgfasp.f90`, `ne/balmod.f90`, BC's
SEILTDG entry in `canada/bc/becset.f90`, and unbuilt `acd/dgf.f90` also
match their original source projections. Source-list static validation
passes. Direct bounds-checked original/current calls reproduce bugs 33–35;
those probes use actual DGF routines, native includes and actual bark
routines, with density/debug callees controlled where needed. Reported growth
values and all coastal ATTEN slots match original/current bits exactly. No whole-model
runtime suite was rerun for this documentation-only audit. The current
Canadian acceptance boundary remains the one in Contributing.md: original
BC/ON keyword input differs from the repaired current buffers, and BC
StandStructure still fails later. A matching direct routine is not Canadian
integrated simulation parity.

All selected DGF consumers use `vbase/dgdriv.f90`: `DGF(DBH)` for projection
and `DGF(WK3)` at its two calibration calls. DGF must continue using the
passed diameters where the native implementation does; several models also
deliberately read current DBH. `ie/rcon.f90` calls argumentless DGCONS for
23 variants. EM alone selects `em/rcon.f90`, which decodes ICL5 through
IHCODE(122), passes habitat index 1–118 to `DGCONS(IDTYPE)`, and lets EM DGF
replace indices above 117 with 30. Do not replace that argument with ITYPE
or introduce an optional argument without an explicit-interface/ABI review.
No DGF/DGCONS references were found in C/header/export-map sources; preserve
their external Fortran entry points regardless of that absence.

The remaining DGF file, `acd/dgf.f90`, is outside all current manifests.
Its 108-species annual NE-style model has the same B1/B2 data as NE, an
extra unused X1 declaration, and its own unbuilt support context. Retain it
under R09; it is not a 25th migrated or runtime-verified variant.
`em/smdgf.f90`, `tt/smdgf.f90` and `mistoe/misdgf.f90` are separate routines,
not extra DGF definitions; retain their existing consumers and contracts.

#### Complete species and equation inventory

Each listed path is selected only by the variant named on its row. A range
or complement covers every native slot in that range, not just species
present in today's test decks. All 979 slots were checked for omissions
and double classification. The period column is BLKDAT's native YR; the
equation-specific conversions described below remain necessary as well.

| Variant / selected path | Native slots / YR | Primary equation paths and data identity |
|---|---|---|
| AK — `ak/dgf.f90` | 1–23 / 10 | Species-indexed annual increment; permafrost branch 4–7,13,16–23; postmultipliers 14,21,23 and 22. |
| BC — `canada/bc/dgf.f90` | 1–15 / 10 | LV2ATV selects legacy tables for all species. V3 uses zone/site lookup and separate power or exponential forms, detailed below. |
| BM — `bm/dgf.f90` | 1–18 / 10 | Small/large blend 1–5,7–10,17; juniper 6; simple interior 11,12; aspen 15; coastal-style 13,14,16,18. |
| CA — `ca/dgf.f90` | 1–50 / 10 | 13 mapped equation rows; GS/RW row 12 differs functionally; tanoak row 13 has period conversion. Complete map below. |
| CI — `ci/dgf.f90` | 1–19 / 10 | Aspen 13; juniper 14; mountain-mahogany 15; cottonwood 17,19; ordinary table form 1–12,16,18, with BAL scaling for 11,12,16. |
| CR — `cr/dgf.f90` | 1–38 / 10 | Delegates every species to GEMDG; all species/model-type groups listed below. |
| CS — `cs/dgf.f90` | 1–96 / 10 | One coefficient row per native species, with species-dependent QMD/CR caps and two bark-conversion clamps. |
| EC — `ec/dgf.f90` | 1–32 / 10 | Original conifer form 1–10,12,16,31; coastal-style form 11,13–15,17–21,23–30,32; red alder 22. |
| EM — `em/dgf.f90` | 1–19 / 10 | Juniper 6; aspen 12,17; cottonwood 11,13–16,19; simple interior 4,5; managed/unmanaged interior form 1–3,7–10,18. |
| IE — `ie/dgf.f90` | 1–23 / 10 | Juniper 15,16; aspen 18,20,21; cottonwood 19,22; ordinary interior form 1–14,17,23. |
| KT — `kt/dgf.f90` | 1–11 / 10 | Native species-indexed quadratic-CCF/management equation; slot 11 has distinct constant/BAL scaling. |
| LS — `ls/dgf.f90` | 1–68 / 10 | CS-style arithmetic with independent 68-row coefficients and different QMD/CR caps. |
| NC — `nc/dgf.f90` | 1–12 / 5 | Redwood 12; revised point-density form 2,6,9; ordinary form 1,3–5,7,8,10,11. |
| NE — `ne/dgf.f90` | 1–108 / 10 | Ten annual potential-basal-area iterations per tree; independent B1/B2 and BALMOD B3 rows for every species. |
| OC — `oc/dgf.f90` | 1–50 / 5 | Same 13-row map as CA; native observation type and final period conversion differ. |
| ON — `canada/on/dgf.f90` | 1–72 / 10 | 35 mapped Ontario equation rows; eight species have an IMC-dependent alternative row. All mappings below. |
| OP — `op/dgf.f90` | 1–39 / 5 | 20-row coastal bank, mapped red alder and redwood exceptions; row 17 is not selected by MAPSPC. |
| PN — `pn/dgf.f90` | 1–39 / 10 | Same mapping/numeric coefficient values as OP; native OBSERV type and period differ. |
| SN — `sn/dgf.f90` | 1–90 / 5 | Native species-row model plus forest-type, plantation and ecological-unit terms; IFOR=20 overrides species 8,13. |
| SO — `so/dgf.f90` | 1–33 / 10 | Juniper 11; aspen 24; red alder 22; native general form for all remaining slots, overridden at IFOR=10 for 1,2,6,7,18,32. |
| TT — `tt/dgf.f90` | 1–18 / 10 | Conifers 1–3,5,7–9,17; PP 10; aspen 6,14; juniper 4,11,12; mixed hardwoods 13,16; cottonwood/stagnation 15,18. |
| UT — `ut/dgf.f90` | 1–24 / 10 | Juniper 11–16,24; aspen 6; conifers 1–5,7–10,23; mixed hardwoods 20,21; stagnation group 17–19,22, with a separate polynomial for 17. |
| WC — `wc/dgf.f90` | 1–39 / 10 | 19-row coastal bank with its own mapping and coefficients; red alder/redwood exceptions. |
| WS — `ws/dgf.f90` | 1–43 / 10 | California group 9,10,12,14–17,19,20,25–27; MC 41; GB 21; GS/RW 4,23; revised RF 7; remaining slots use the default form and conditional size/period adjustments. |

Paths in this table are as audited at `b4b1ee3d`. Since L04a–v, every
variant's body is an internal procedure of `vbase/dgf.f90` and the native
files are removed.

CA/OC share the following complete MAPSPC partition. CA calls species 4
WF and OC calls it GF; the numeric mapping is unchanged. Actual BLKDAT
calls species 21 WJ, despite the DGF commentary's JU abbreviation.

| Equation row | Native species indices, CA and OC |
|---|---|
| 1 — IC/cedars | 1–3 |
| 2 — WF/GF/BR | 4,22 |
| 3 — RF/SH | 5,6 |
| 4 — DF | 7 |
| 5 — KP group | 11,13,14,20,21,24,47 |
| 6 — LP/WB | 10,12 |
| 7 — SP/WH/MH | 8,9,16 |
| 8 — WP | 17 |
| 9 — PP group | 15,18,19,25 |
| 10 — oaks/other hardwoods | 26–36,40,41,43–46,48,49 |
| 11 — MA/GC/DG | 37–39 |
| 12 — GS/RW | 23,50 |
| 13 — TO | 42 |

The coastal maps use native indices as follows. OP's 23/24 are MA/TO;
PN/WC's are WA/PB. OP/PN slot 6 is SS, whereas WC leaves its slot 6 blank.
Blank slots still have native table mappings and must not be dropped.

| Equation row | OP / PN native species | WC native species |
|---|---|---|
| 1 | 1 | 1 |
| 2 | 2,3 | 2,3 |
| 3 | 4 | 4 |
| 4 | 5,7 | 7 |
| 5 | 13,14 | 13,14 |
| 6 | 12,15 | 12,15 |
| 7 | 16 | 16 |
| 8 | 18 | 18 |
| 9 | 19 | 19 |
| 10 | 20 | 20 |
| 11 | 9,10,30–33 | 9,10,30–33 |
| 12 | 21 | 21 |
| 13 — RA | 22 | 22 |
| 14 | 23–27,29,34–39 | 23–27,29,34–39 |
| 15 | 8 | 8 |
| 16 | 11 | 11 |
| 17 | Unused coefficient row | 5,6 |
| 18 | 6 | 28 |
| 19 | 28 | 17 |
| 20 | 17 | No row 20 |

ON's full OSPMAP, in native order 1–72, is retained literally here so the
quality-dependent equation selection is reviewable without species-name
inference:

```text
 1–10:  5, 4, 3, 4, 1, 7, 8,12, 9,14
11–20: 13,11, 9, 9,26,28,35,17,17,33
21–30: 17,17,17,19,29,15,15,24,28,24
31–40: 17,24,15,22,26,26,33,33,17,35
41–50: 35,35,21,15,31,31,32,31,21,17
51–60: 17,17,32,17,17,17,17,17,17,17
61–70: 17,17,17,17,17,17,17,17, 6, 2
71–72:  8,10
```

Only native species 15,19,20,24,25,26,28,34 have LQUAL true. For those,
IMC=1 selects OSPMAP+1 and AGS=1; otherwise use OSPMAP and AGS=0.
DGCONS's observation lookup still uses OSPMAP, not the quality-shifted row.
All 35 rows are reachable through this map plus its eight quality choices.

CR's complete GEMDG partition is separate from its DGF stand preparation:

| Native species | GEMDG equation / model-type alternatives |
|---|---|
| 1,2 | AF/CB; IMODTY<=2 versus other types. |
| 3 | DF polynomial. |
| 4 | Direct log-DDS from CI GF, fixed reference site. |
| 5 | WF polynomial. |
| 6 | Direct log-DDS from NI MH, fixed reference site. |
| 7 | Direct log-DDS from NI RC, fixed reference site. |
| 8 | Direct log-DDS from NI WL, fixed reference site. |
| 9,12,16,23–27,29–35,37 | Juniper/pinyon polynomial and one-inch cap. |
| 10 | Direct log-DDS from UT LM. |
| 11 | LP polynomial. |
| 13,36 | PP: SWMC blend, SWPP, Black Hills with age-range transition, or other model types; Black Hills increment multiplier 0.80. |
| 14 | Direct log-DDS from EM WB, fixed reference site/slope scaling. |
| 15 | SW polynomial. |
| 17 | BS polynomial. |
| 18 | ES: IMODTY<=2 versus other types. |
| 19 | WS polynomial. |
| 20–22,28,38 | Cottonwood/aspen polynomial: IMODTY<=2 versus other types, then multiply predicted diameter by 1.05. |

IMODTY=1–5 also changes basal-area floors (1,21,5,5,14) and, for type 1
below BA=65, BGTTBA. Direct-log and future-diameter branches take different
routes to DIAGR, then apply DBHMAX where appropriate, ISTAGF/DSTAG and the
final DDS clamp. The default GEMDG branch remains `DDS=-9.21, IDDS=1`.

#### Coefficient banks, calculations and state differences

**CA/OC and coastal models.** All CA/OC growth/site coefficient and mapping
DATA blocks agree; OBSERV is integer in CA and real in OC (13 rows, with
8928/8928. for GS/RW). Their MAPLOC is 10x13 and DGFOR 5x13. Ordinary
species evaluate two successive DDS assignments; do not combine them or
add coastal DGBA terms with zero coefficients. DGDSQ is species-indexed.
Tanoak 42 uses `EXP(DDS)` then `ALOG(TDDS*2.)`. OC additionally applies
`EXP(DDS)` then `ALOG(TDDS/2.0)` for every species before the -9.21 clamp;
the two tanoak conversions must not be cancelled algebraically.

OP/PN's other 21 named DATA blocks agree exactly; OBSERV uses real literals
in OP and integer literals/type in PN. WC's 19-column banks are distinct
from their 20-column banks, including every corresponding initializer block.
Coastal ordinary DDS has its additional DGBA*BA term and equation-indexed
DGDSQ. DGCONS also writes ATTEN by equation index, an existing defect
(bug 33), while DGCON/SMCON/COR2 handling remains species-indexed. OP
halves DDS in real scale for all species; PN/WC do not. WC maps IFOR
7/8/9/10 to JFOR 3/6/5/4, uses IFOR for 1–6, and defaults other forests
to 3; OP/PN use their native six-forest maps directly. MH site is multiplied
by 3.281, WO uses the unguarded site transformation, and equation 14's
elevation is capped at 30. Preserve the native debug FORMAT difference.

**GS/RW common calculation.** CA/OC 23,50; OP/PN/WC 17; NC 12; and WS
4,23 use the same Castle coefficients and site constant:

```fortran
DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))
DGLT = EXP(CONSPP + 0.185911*LOG(D) -0.000073*D*D &
     -0.001796*PBAL -0.42078*PRD +0.589318*LOG(CR*100) &
     -0.000926*SLOPE*100 -0.002203*(SLOPE*100)*COS(ASPECT))
```

Convert outside-bark DGLT by calling native BRATIO once within this block,
reusing it for D and D+DGLT, then `LOG(TEMPD2**2-TEMPD1**2)`. COR and
LOG(COR2) are added afterwards, even when LDCOR2 is false; DGCONS excludes
this group's COR2 addition. CA/OC and OP/PN/WC have the same 11-statement
PBAL-through-second-debug-write block. NC/WS prepare PBAL outside that
block and have different diagnostics or period handling. NC substitutes
BAL when PBAL<=0; the other implementations use PBAL<0. WS already calls
BRATIO earlier and calls it again inside the block. Preserve those calls,
the caller's point-density preparation and each variant's period policy.

**Red alder.** EC 22, SO 22 and OP/PN/WC equation 13 (native 22) share
CONST=3.250531-0.003029*BA, the quadratic through D=18, the declining
line above 18, and minimum DIAGR=0.1. Their bark selection and debug/control
flow differ: coastal code uses BRATIO(22,...) then jumps past common tree
preparation; EC uses ISPC; SO has already called bark and calls it again.
The return expression adds ALOG(COR2)+COR directly. OP's later period
conversion still applies. Preserve the skipped/stale debug locals on the
coastal early-jump path rather than adding general initialization.

**Juniper and cottonwood.** The recurring juniper prediction is
`0.25897 + 1.03129*DPP -0.0002025464*BATEM +0.00177*SI`, with the
one-inch increment cap. Consumers are BM 6, CI 14, EM 6, IE 15,16,
SO 11, TT 4,11,12, UT 11–16,24 and 17, WS 21, and CR's listed group.
DPP is floored at 1; BATEM is generally floored at 1, but UT 17 and WS
21 use 5 and CR uses its model-type floors. UT 17/WS 21 apply stagnation.
Some callers clamp before adding corrections, others afterwards, and the
placement of CONSPP differs. Share the raw polynomial before sharing
the conversion/clamp wrapper.

CI 17,19, EM 11,13–16,19 and IE 19,22 share the cottonwood polynomial
`0.24506 +1.01291*DPP -0.00084659*BA +0.00631*SI`, a DF ceiling of 36,
and the native correction/clamp sequence. TT 15,18 and UT 18,19,22 use
the other form `(1.55986 +1.01825*DPP -0.29342*ALOG(BATEM) +0.00672*SI
-0.00073*BAUTBA)*1.05`, BA floor 5 and optional DSTAG. CR selects related
forms by IMODTY and multiplies DF by 1.05 at a different point; do not fold
that multiplier into coefficients. Aspen already has one shared helper,
`ut/dgfasp.f90`, for BM/CI/EM/IE/SO/TT/UT's listed species. It takes CR in
percent, uses RMSQD/BA, caps BA>=310 to 305, and returns log-DDS before
the caller's explicit COR2/COR. No new aspen equation is needed.

**Interior banks.** IE uses 23 species, DGHAB/OBSERV 6x23, habitat maps
30x23 and forest/DSQ maps 11x23. EM uses 19 species, DGHAB 8x19,
OBSERV 6x19, habitat maps 117x19 and forest/DSQ maps 7x19. CI uses 19
species, DGHAB 12x19, ICHBCL 130x19, forest maps 6x19 and integer
IBSERV(6,3)/OBSERV(19); its ICHBCL column 11 has no DATA initializer.
KT uses 11 species, habitat maps 175x11, DGHAB/OBSERV 9x11, forest map
10x11 and DGFOR 7x11, with integer OBSERV. Preserve native table dimensions,
sparse initialization, observation conversions and row order; another
variant's MAXSP is not a safe declaration for these banks.

IE's ordinary formula uses BAL/100, factored crown terms and a log(DBH+1)
denominator. EM uses that form for 4,5, but its ordinary conifers have
unscaled BAL, log(RELDEN), squared RELDEN and MANAGD-dependent PCCF terms.
EM species 4 retains both its DGCCF assignment and extra -0.199592 density
addition. Its DGCONS chooses IDTYPE versus ITYPE by species, rotates aspect
for 4,6,12,17 and scales slope by 1/10 for 1–3,7–10,18 (zero for 18).
IE's aspect/site/observation choices are different. CI separates stand BAL
from point PBAL, scales stand BAL only for 11,12,16, and gives species 15
its extra BA term and elevation cap. KT retains quadratic RELDEN, two
management PCCF terms, log(BA), and slot 11's 0.01 and /100 scaling.
CRMAX/DBHCH are diagnostic/inactive-limit information in KT; do not activate
the commented constraints while generalizing.

**TT/UT and stand preparation.** TT has 18 species, IDGSIM/DGSIC 5x18,
forest/DSQ maps 4x18, IBSERV 5x18, ICHBCL(363) and DGHAB(7). UT has 24
species, IDGSIM 7x24, DGSIC 5x24, maps 6x24 and IBSERV 5x24. Their
conifer expression uses separate `CR*DGCRS+CR*CR*DGCRS2` operations, rather
than the interior factored expression. UT additionally uses PCCF, scales
the whole log-DDS result by 0.95 for species 5, and mutates DGCCF(8) for
high site classes during DGF. UT uses NINT for that site's class input;
TT's analogous site-class setup uses INT. TT PP 10 uses point PBAL and
subtracts 0.257322*ALOG(BA); its TEMBA floor is not used by that logarithm. TT/UT
mixed hardwoods keep their PCCF/BA terms. DGCONS uses SITEAR(ISISP), native
ISMAP/IDGSIM, aspect rotations, logarithmic site choices and elevation caps;
TT PP also has a habitat addition and distinct attenuation counts.

CR/TT/UT share the age-range and stagnation preparation pattern: BADIST,
ABIRTH/height screening, current DBH/probability sums, optional recent
mortality at LSTART with FINT/FINTM scaling, and SDICAL(1,...). Keep its
AGERNG/DSTAG writes, native guards, division behavior and call position.
IE/EM share only the age-range part and use SDICAL(0,...); WS computes
QMD from all live records and also uses SDICAL(0,...). They are not one
interchangeable preparation routine.

**BM.** Eighteen-row large-tree tables coexist with five-row small-tree
tables, SMMAPS(18), SMMAPH(92,5) and integer IBSERV(5,3). The blend group
converts the small-tree prediction by doubling diameter increment and
recomputing squared increment, then blends log-DDS: weight 1 below D=3,
(10-D)/7 above it, floored at zero. Keep WP's BAL/RMAI additions, WL's BA
addition and the separate small-tree COR2 update. Species 11,12 use BAL/100;
the coastal group has log(BA), stand BAL and BA terms. DGCONS rotates
aspect for 6,11,12,15, transforms species 7's site, converts MH site by
3.28, caps elevation for 16,18, and uses different attenuation sources.
Native debug statements can read locals retained from preceding groups;
do not make those locals shared cross-variant state.

**CS/LS.** The arithmetic is shared, but all 12 corresponding DATA blocks
differ: 96 rows versus 68. Preparation includes only DIAM>=5, weights by
PROB, uses 0.005454154, and defaults empty BAGE5 to 10. CR is percent,
defaults to 10 when nonpositive, then receives native caps. RELDBHSQ is
`D*D/QMDGE5`, not the square of relative diameter. The native QMD variable
is reduced in place as species are visited; preserve that order-dependent
state rather than restoring an uncapped copy during cleanup. After the
log-DDS equation, both convert with current DBH and native BRATIO, clamp
before and after conversion, and store WK2. Using passed DIAM in that
conversion would change calibration behavior.

| Cap | CS native species | LS native species |
|---|---|---|
| QMD 11 | 51 | None |
| QMD 12 | 50 | None |
| QMD 13 | 3,10–13 | 1,2,10,37–39,59 |
| QMD 15 | None | 11 |
| QMD 17 | 91 | None |
| QMD 20 | 44–46,59 | None |
| QMD 25 | 14–17,28,53–56 | 17,30–33 |
| QMD 30 / 40 | 48,49 / 24 | None |
| CR 50 / 60 | 7 / 28,41 | None / 17 |
| CR 75 / 80 | 8–10 / 44–46 | None |
| CR 85 | 32,78–84 | 60,65 |

**EC/SO.** EC's native 32-row bank has forest/DSQ maps 7x32. Its original
conifer form includes DUMMY, RELHT*PCCF and PCCF squared; slot 1 has an
extra RELHT term. DUMMY is 1 only for 4,6,16; species 12,31 subtract
0.000021*RMAI*RELDEN from CONSPP. Its coastal form has BA, WH's relative-height
subtraction and WO's BAL subtraction. DGCONS retains flat-slope SL0DUM for the conifer
group, MH/OS site conversion by 3.281, WO's guarded site conversion, AF's
0.3835 addition, and elevation caps for 19,23–27,29,30,32.

SO's 33-row banks use forest/DSQ maps 10x33 and IBSERV(5,33). At IFOR=10,
native 1,2 share WP/SP arithmetic, while 6,7,18,32 have their own IC/LP/RC/OS
equations and site/observation setup. Those branches omit the ordinary
RELDEN/RMAI constant terms. The general formula retains DGMACC*RMAI*RELDEN,
PP's log(RELDEN)/PCCF-squared term with the >400 switch, ES relative height,
SF's -0.799079 dummy and the species-dependent BA/log(BA)/relative-height
terms. Preserve the flat-slope defaults, MH site handling, aspect rotations
for 11,16,24 and elevation caps for 21,25,26,28–31,33. EC and SO do not
become equivalent by filling a common coefficient vector with zeros.

**NC/WS.** NC's fixed 12-row bank has forest/DSQ maps 7x12. Revised
species 2,6,9 use crown-squared/log(DBH+1), point PBAL/log(PBA), a -8.52
preclamp and real-scale halving; row 12 also halves after correction.
Other NC rows are already on the native five-year basis and must not be
halved. ILAT is fixed at 5; default DGCONS uses LOG(SITEAR(3)).

WS's 43-row bank uses forest map 7x43, DGFOR 3x43, latitude tables and
species-specific DDSMX bounds. The California group has a BAL term except
for 9,10; MC 41 uses BA instead of log(BA). GB 21 uses juniper with
stagnation; RF 7 has its own point-density equation. The default branch
uses small-tree replacements for D<10 on species 2,3,6,13,22. That nested
replacement also names species 7, but outer CASE(7) bypasses it. Hardwoods
28–40,43 double squared increment through EXP/ALOG and use DF's site in
DGCONS. IC 5 receives its post-equation site adjustment. Relative-height
caps differ by branch; PBA fallback and PBAL's strict-negative test differ
from NC. The late DDSMAX assignment does not update WK2 (bug 35).

**AK.** All 23 native rows first predict annual outside-bark increment
from scaled elevation/slope, COS(aspect), log(site), log(DBH), DBH squared,
PTBALT, point relative density and log(percent crown). Permafrost modifiers
for 4–7,13,16–23 use separate PF coefficients: LPERM caps the modifier above
at 1, while its alternative floors it at 1. Multiply by YR, then by 0.45
for 14,21,23, by 0.65 for 22, or by 1 otherwise, and perform bark/DDS
conversion. DGCONS assigns ATTEN and conditionally adds LOG(COR2) to the
existing DGCON; it does not initialize DGCON, DGDSQ or SMCON like other
variants. Preserve that initialization/call dependency.

**SN.** Preserve all 90 native coefficient rows as scientific data. The
ordinary formula uses LOG(percent crown), relative height, BA and point
PBAL. Nonpositive BA and ICR are replaced in shared state with 25, and
point PBA/PBAL fall back on nonpositive values. Seven forest-type dummies
and the MANAGD>0 plantation dummy modify the equation. DGCONS adds site,
slope/aspect and eleven PCOM ecological-unit terms; mappings include
221DD/DE to S231T, 251 to P255 and 255CH to S231L. Preserve the complete
ordered IFORTP and PCOM selectors (now `vbase/dgf.f90:8102–8154, 8957–9026`),
including no-match/default states. The IFOR=20 overrides for native 8/13
use their two explicit pine equations and DIAGR floor 0.01, replacing the
earlier DDS rather than appending its correction terms. Their ATTEN values
are 2056/689. DGCONS does not assign SMCON; do not add a generic reset.

**NE/ON.** Both perform ten annual iterations, but have different state,
units and equations. NE uses 108 B1/B2 rows and 108 B3 rows in BALMOD;
BADIST prepares competition, BALMOD shifts/caps its diameter class and
floors the modifier at 0.5, and potential basal-area growth is multiplied
by 0.7. After iteration it uses bark at grown diameter, guarded COR2 on
diameter increment, a 0.0001 increment floor and final ALOG(DDS)+COR.

ON uses its 35-row B0/B1/B2/BBAL/BHT/BSI/BBA/BDBHQ/BAGS/B95 bank in
metric units. Stand/point competition, height, site and QMD terms are held
fixed during the ten annual diameter iterations; log increment is clipped
to [-5,5] and real increment to [0.0001,B95]. Preserve BAMSP quality sums,
leading-species logic, guarded COR2 and the existing final-loop bark-ratio
defect (bug 34). LHYBRID is set false after its eligibility calculation,
so the following stand-level adjustment is inactive; do not activate or
delete its retained design code during DGF sharing. ON and NE are separate
kernels, even though their loop counts match.

**BC.** Preserve both complete model banks. LV2 uses native 15-species
tables, DGHAB/OBSERV 6x15, habitat/CCF maps 30x15 and forest/DSQ maps
11x15. SEILTDG copies BEC terms into DGF's local BECADJ/SEI arrays, converts
their units and returns LSPPOK. Unsupported species zero the local SEICCF;
DGCONS follows its adjusted versus unadjusted branch and writes V2SEICOR.
Retain the exact current LV2 DDS expression and common MAX/LOG tail; do
not substitute the superficially similar IE equation's output contract.

V3 has 28 ZNKONST model records and 25 SSKONST site-intercept records,
with ordered species lists and up to 30 character(15) patterns per record.
DGCONS sanitizes NUL characters in these DATA-backed strings, remaps the
KAM ICHmw2 site-series names, then searches native first-match order with
one `/all` fallback. Overlapping patterns are significant; converting the
lookup into a dictionary would change selection. Missing either lookup
skips coefficient/ATTEN/SMCON assignments after setting DGCON=0, so do
not invent a fallback model or initialization.

| BC native species | ZNKONST records | SSKONST records |
|---|---|---|
| 1 PW | 1 | 1 |
| 2 LW | 2 | 2 |
| 3 FD,14 OC | 3–9,28 | 3 |
| 4 BG,9 BL | 17 for both; 26 additionally for 9 | 20–23 |
| 5 HW | 10 | 4,5 |
| 6 CW | 11,12 | 6–8 |
| 7 PL | 13,14,22–24 | 9–14 |
| 8 SE | 15,16,27 | 15–19 |
| 10 PY | 18 | 24 |
| 11 EP,15 OH | 19,20 for both; 25 additionally for 15 | 25 |
| 12 AT,13 AC | 21 for both; 25 additionally for 12 | 25 |

Records 22–28 cover SBS/SBPS alternatives as stored; many site-intercept
patterns remain restricted to ICH/IDF. Keep no-match behavior. V3 species
12/13 use the power form; 11/15 use it in IDF and the exponential form in
ICH; other species use the exponential form. Retain RELDEN floors of 100
for HW in ICH, PL in IDF, and EP/OH in ICH/IDF, and 125 for BL in ICH.
HW's ICH crown ratio and EP/OH's ICH/IDF crown ratios are capped at 0.8.
Retain the D>=1 power-form bound, native INtoCM and
basal-area conversions, BRATIO on the converted D argument, CMtoIN squared,
the 0.001 real-scale floor, final log/clamp and PW's elevation bounds.
Outside the handled zones, the 11/15 prediction can retain an earlier or
undefined DDS; this is a source-level hazard, not a verified valid-input
failure. Keep the original behavior pending a separate model investigation.

#### Generalization design and acceptance

Use one public DGF implementation in `vbase/`, with narrow dispatch to
family calculations and native coefficient banks. Share actual arithmetic
blocks; keep distinct AK, SN, NE, ON, CR and BC algorithms as named family
kernels. A common species-name switch, universal coefficient dot product,
unit-normalized equation or tolerance-based comparison is insufficient.
Keep default-real kinds, literal spelling, arithmetic grouping, statement
boundaries, native table extents/types and the observed global side effects.
Do not silently repair bugs 33–35, in-place QMD caps, sparse initialization,
skipped outputs, unguarded logs/divisions, inactive alternatives or debug
locals while extracting kernels.

For public DGCONS, preserve the two native ENTRY signatures. A concrete
way to retain one DGF body is two build-selected `DGCONS_ENTRY.f90`
declaration includes: one containing `entry DGCONS`, the EM selection
containing `entry DGCONS(IDTYPE)`. This follows the existing selected-include
pattern; each manifest must select exactly one basename, both includes must
be classified, and only EM's branch may use IDTYPE. Keep all RCON calls
unchanged. Validate this adapter stage independently before moving any
Canadian model. Do not use a missing nonoptional argument or assume an
implicit-interface caller can safely discover OPTIONAL.

The pending tasks L04a–L04v above form the deployment sequence. Each
consumer extension or sub-block is a separate implementation/checkpoint;
the complete audit does not authorize consolidating all families in one
patch. Keep native DATA with its family initially. Share a coefficient bank
only after matching every row, declaration and initialization dependency;
CA/OC and OP/PN are candidates with their observation types kept explicit.
No numerical savings or performance benefit is claimed before an actual
implementation is measured.

For every migration checkpoint:

- Compare original F77, the immediate pre-change baseline and the candidate
  with identical compiler/settings. Use defined native common-block state,
  each variant's selected includes and actual delegated equations/bark
  routines. Verify both DGF diameter inputs and every DGCONS entry contract.
- Exercise every native species slot and every selected equation row,
  including duplicate species mappings and reserved slots. Check literal
  data for retained unused rows without inventing a native species route
  to those rows. Check all DGCON/DGDSQ/DGCCF/ATTEN/SMCON
  outputs and other mutated state; do not check WK2 alone.
- Cover equation switches and boundary values: D<=0 and existing WK2,
  D=1/2/3/5/10/18/36 limits, CR floors/caps and percent/fraction use,
  AVH/BA/RELDEN guards, PBAL negative/zero and PRD branches, site/habitat/
  forest/latitude lookups, managed/unmanaged and permafrost alternatives,
  LSTART/recent mortality, ISTAGF/DSTAG, all period conversions and clamps,
  COR/COR2/LDCOR2, and debug on/off with the native diagnostic sequence.
- Add family cases: CS/LS ordered multi-species caps and calibration DBH
  versus DIAM; SN every IFORTP/PCOM arm and IFOR=20; CR every IMODTY and
  PP blend; NE/ON full annual iteration, ON all quality pairs and disabled
  hybrid; BC LV2 supported/unsupported SEI and every V3 record/pattern,
  first-match overlap, fallback and no-match state. Preserve storage-order
  defects explicitly until their separate repairs are authorized.
- Require real mismatches to fail. Compare exact state bits, return/status,
  output inventory, diagnostics, text, SQLite rows and restart files; retain
  only the already documented undefined-value exceptions. If a new undefined
  diagnostic prevents comparison, record its exact source/coverage limit
  before choosing separate evidence; do not broaden normalization silently.
- Recheck manifest membership/one selected basename, source/include
  classification, then run the whole parity suite and `git diff --check`.
  Report Canadian pristine-reference differences separately from controlled
  immediate-baseline comparisons. Discard temporary probes/builds/results.

The audit leaves all Fortran, manifests, reference outputs and NVEL unchanged.
Only this tracker and the three reproduced-defect descriptions are updated.


## Material to preserve

- [organon/orgfert.f](organon/orgfert.f) is a deferred design discussion, with
  explicit reasons the proposed fertilization integration was left out. It
  records potentially useful research intent. Preserve it; any later move into
  documentation should retain the complete discussion and its provenance.
- `vbase/evpred.f90` is live through its FISHER entry point, called by EVTSTV.
- `base/evtact.f90` is live through EVALSO, EVTHEN, and other ENTRY points.
- Active `base/ex*.f90` stubs satisfy extension contracts. Their full scientific
  implementations being dormant does not make the stubs dead.
- Public FVS/NVEL procedures can be invoked through shared libraries. Missing
  internal callers are insufficient grounds for deletion; inspect C wrappers,
  exports, tests, supported external consumers, and alternate library targets.
- NVEL `r8dib.f90` is outside mFVS source lists but selected by NVEL's
  standalone project. Keep the submodule's own build/support scope in view.
- BC's `ESHAP` common-block layout differs from `common/ESHAP.f90`, including
  order and additional state. Do not treat the includes as interchangeable.
- Historical change notes and exact original-reference revisions are evidence.
  Preserve them throughout cleanup, along with existing reference outputs.

## Suggested first sequence

1. P01 and P02: establish the original-reference comparison on a small case.
2. P03: document/reproduce the BC/ON verification boundary; leave affected
   simulation acceptance blocked until separately resolved.
3. D01 and D02: review the two most clearly documented obsolete files, one at
   a time, with replacement and recovery evidence.
4. C02, C03, then C05: small active consolidations with explicit differences
   and targeted original-reference comparisons.
5. Select the next individual item based on available coverage and dependencies.
