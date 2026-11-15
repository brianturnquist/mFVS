# mFVS cleanup audit and tracker

Created: 13 November 2026.

Audit baseline: main repository `776491e8850b447e638f7b267b8bcaab641584f4`,
with NVEL submodule `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.

This checklist stores the redundancy and dormant-code findings before cleanup.
P01–P03, D01–D15, C01–C19, B01–B03 and the unbuilt-directory removal (R01–R08, R10–R10d)
are complete. Six stub tasks, the metric audits M01–M14 and the implementation
tasks M01a–M07a, M08a and M09a–M14a are also complete (M06a is included in M04a);
the checklist now records 115 completed and 23 pending items, including R09.
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
- [ ] **M08b — FMIN: DWDCVOUT handler (audit).** Independently compare
  option 52 and its cover-report consumer before sharing that handler.
- [ ] **M08c — FMIN: FUELOUT handler (audit).** Independently compare
  option 15 and its all-fuels consumer, including bug 24. Subsequent FMIN
  handlers still require their own bounded tasks.
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

- [ ] **M15 — `metric/vbase/disply.f90`.** Compare one display-record format
  with `vbase/disply.f90`.
- [ ] **M16 — `metric/vbase/fvsstd.f90`.** Compare one output-record type
  with `vbase/fvsstd.f90`, retaining Ontario's cord-conversion behavior.
- [ ] **M17 — `metric/vbase/initre.f90`.** Identify the keyword-array defect,
  then compare one keyword family with `vbase/initre.f90`. BC's separate
  `canada/bc/initre.f90` requires its own follow-up task. Do not attempt a
  complete INITRE merge in this task.
- [ ] **M18 — `metric/vbase/sumout.f90`.** Compare one cycle-summary record
  with `vbase/sumout.f90`.
- [ ] **M19 — `metric/vdbsqlite/dbsfmpf.f90`.** Compare one database output
  group with `vdbsqlite/dbsfmpf.f90`.
- [ ] **M20 — `metric/vdbsqlite/dbsstandin.f90`.** Compare one group of stand
  input fields with `vdbsqlite/dbsstandin.f90`.
- [ ] **M21 — `metric/vdbsqlite/dbssumry.f90`.** Compare one summary record
  and its units with `vdbsqlite/dbssumry.f90`.

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

## L — Larger families: first bounded comparison only

These counts describe selected files, not removable lines or proven duplicate
algorithms. Each item is a **read-only audit of one pair and one block**. Its
deliverable is a difference map and subsequent small implementation tasks;
do not consolidate the whole family in one run.

- [ ] **L01 — PVREF6 (7 files).** Begin with BM/EC and one equation branch or
  coefficient-table section. Distinguish shared calculation from scientific data.
- [ ] **L02 — MORTS (21 files).** Begin with IE/KT and one mortality block.
  Preserve species-specific model behavior and diagnostic output.
- [ ] **L03 — REGENT (23 files).** Begin with CA/OC and one regeneration
  calculation block; record coefficients and state dependencies.
- [ ] **L04 — DGF (24 files).** Begin with CA/OC and one species/equation
  group. Do not infer equivalence from matching routine names.
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
