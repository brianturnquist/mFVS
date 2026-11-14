# mFVS cleanup audit and tracker

Created: 13 November 2026.

Audit baseline: main repository `776491e8850b447e638f7b267b8bcaab641584f4`,
with NVEL submodule `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.

This checklist stores the redundancy and dormant-code findings before cleanup.
P01–P03, D01–D15, C01–C18 and the unbuilt-directory removal (R01–R08, R10–R10d)
are complete. R09 and the remaining blocks are pending: 75 completed and
46 pending items. BC/ON simulation parity remains blocked by the existing bugs
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

The mortality opportunity crosses routine names: `VARMRT`, `NWCMRT`, `SCOMRT`,
`TTMRT`, and `UTMRT` repeat allocation logic. The three FIA writers likewise
share long binding sequences despite having distinct names. Whole-file or
same-name-only duplicate searches miss these opportunities.

## M — Active metric copies: bounded audits first

There are 21 selected metric copies. All tasks in this section are initially
**audits**, not authority to merge whole files. Each should produce a difference
map for one record group, keyword handler, or calculation block, identify state
and interface dependencies, and propose the next implementation-sized task.
Split further if a block is still too large. BC/ON parity remains subject to P03.

- [ ] **M01 — `metric/base/evldx.f90`.** Compare one event-variable family
  and its unit conversions with `base/evldx.f90`.
- [ ] **M02 — `metric/base/intree.f90`.** Trace the database-input call and
  decay handling against `base/intree.f90`. The verified 30-versus-33
  argument defect blocks consolidation until separately resolved.
- [ ] **M03 — `metric/base/oplist.f90`.** Compare one activity-output record
  with `base/oplist.f90`.
- [ ] **M04 — `metric/base/stats.f90`.** Compare one statistics calculation
  and output group with `base/stats.f90`.
- [ ] **M05 — `metric/dbsqlite/dbsin.f90`.** Audit keyword-array dimensions
  first, then one handler against `dbsqlite/dbsin.f90`. Keep defect repair
  separate from consolidation.
- [ ] **M06 — `metric/dbsqlite/dbsstats.f90`.** Compare one database row,
  including schema, binding types, and conversions, with `dbsqlite/dbsstats.f90`.
- [ ] **M07 — `metric/fire/base/fmdout.f90`.** Trace `FMSVL2` calls against
  `fire/base/fmdout.f90`. The documented incompatible interfaces require a
  separate repair before consolidation.
- [ ] **M08 — `metric/fire/base/fmin.f90`.** Compare one keyword handler
  with `fire/base/fmin.f90`; schedule subsequent handlers independently.
- [ ] **M09 — `metric/fire/base/fmsout.f90`.** Compare one fuel-output
  record with `fire/base/fmsout.f90`.
- [ ] **M10 — `metric/fire/vbase/fmfout.f90`.** Compare one report section
  with `fire/vbase/fmfout.f90`.
- [ ] **M11 — `metric/fire/vbase/fmpofl.f90`.** Compare one calculation and
  output block with `fire/vbase/fmpofl.f90`.
- [ ] **M12 — `metric/newmist/misin.f90`.** Audit keyword dimensions and
  BC-specific mistletoe state against `mistoe/misin.f90`. The different state
  model means this is more than a unit switch.
- [ ] **M13 — `metric/newmist/misprt.f90`.** Compare one report type with
  `mistoe/misprt.f90`, including BC state dependencies.
- [ ] **M14 — `metric/strp/esuckr.f90`.** Compare one sprouting calculation
  and its dimensional assumptions with the selected imperial counterparts.
  The eventual shared home is `estb/`; do not merge all sprouting copies here.
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

- [ ] **U01 — `base/exdfb.f90`.** Review suppression assignments only.
- [ ] **U02 — `base/exdftm.f90`.** Review suppression assignments only.
- [ ] **U03 — `base/exmpb.f90`.** Review suppression assignments only.
- [ ] **U04 — `base/exbrus.f90`.** Review suppression assignments only.
- [ ] **U05 — `base/exrd.f90`.** Review suppression assignments only.
- [ ] **U06 — `base/exmist.f90`.** Review suppression assignments only.
- [ ] **U07 — `base/exbm.f90`.** Review suppression assignments only.
- [ ] **U08 — `base/execon.f90`.** Review suppression assignments only.
- [ ] **U09 — `base/exppe.f90`.** Review suppression assignments only.
- [ ] **U10 — `base/exclim.f90`.** Review suppression assignments only.
- [ ] **U11 — `base/excov.f90`.** Review suppression assignments only.
- [ ] **U12 — `base/exorganon.f90`.** Review suppression assignments only.
- [ ] **U13 — `base/exbudl.f90`.** Review suppression assignments only.
- [ ] **U14 — `base/exbgc.f90`.** Review suppression assignments only.
- [ ] **U15 — `base/exfert.f90`.** Review suppression assignments only.

## B — Build and declaration cleanup

- [ ] **B01 — CMake source form (build change).** `bin/CMakeLists.txt` still
  defaults Fortran to fixed form and explicitly marks only `main.f90` and
  `version.f90` as free form. Audit and correct this stale configuration
  separately. CMake was not executed during the audit, so a resulting build
  failure is not claimed as reproduced evidence.
- [ ] **B02 — Repeated make setup rules (build change).** Consolidate only
  the nearly identical setup rules for 24 variants in `bin/makefile`. Compare
  expanded commands and dependencies before and after, then verify affected
  builds and outputs. Do not redesign source manifests in the same task.
- [ ] **B03 — First identical ESPARM pair (implementation).** The 24 selected
  `ESPARM.f90` files have 16 normalized statement forms. Start with identical
  CA/OC declarations and a shared home in `common/`. Preserve parameter
  values, dimensions, include order, and common-block layouts. Schedule other
  groups separately after this first verified pair.

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
- [ ] **L05 — DGDRIV (22 files).** Begin with KT/NC and one calibration
  branch. Their `LDGCAL` control flow differs; preserve it.
- [ ] **L06 — CRATET (23 files).** After the separate CS/NE task C09, begin
  a TT/UT comparison of one calculation group. Record numerical and diagnostic
  differences before proposing extraction.

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
