# mFVS cleanup audit and tracker

Created: 13 November 2026.

Audit baseline: main repository `776491e8850b447e638f7b267b8bcaab641584f4`,
with NVEL submodule `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.

This document stores the redundancy and dormant-code findings before cleanup
begins. The audit was read-only. No builds or simulations were run during the
audit, and no source, build configuration, source list, test input, or reference
output was changed. Creating this tracker was a documentation-only step.
P01–P03 verification preparation and D01–D13 retirement are now complete; see
the [verification report](tests/parity/evidence/2026-11-13-p01-p03/README.md) and
the retirement reports for [D01–D03](tests/parity/evidence/2026-11-13-d01-d03/README.md)
and [D04–D13](tests/parity/evidence/2026-11-13-d04-d13/README.md).
D14 remains pending. C01–C06 are now complete; the user accepted the exact
routine and runtime-object evidence for C01/C06 while leaving BC/ON simulation
parity blocked. See the [C01–C06 report](tests/parity/evidence/2026-11-14-c01-c06/README.md).
C07–C12 are now complete; the user accepted the routine/file-output and
executable-statement evidence for C07/C10 while leaving BC/ON simulation parity
blocked. There are 47 completed and 73 pending checkbox items. See the
[C07–C12 report](tests/parity/evidence/2026-11-14-c07-c12/README.md).

The findings combine repository-wide source-list inventory, targeted source
comparisons, call/ENTRY inspection, and historical notes. They are not an
exhaustive equivalence proof for every routine or a blanket deletion list.

## Using this tracker

- Work on one bounded task at a time. Do not combine whole routine families,
  metric conversion, extension retirement, and bug fixes into one change.
- Keep existing IDs stable. Add a new ID when a task must be split further.
- Leave a checkbox unchecked while work is pending, in progress, blocked, or
  awaiting verification. State the status next to the item when needed.
- A completed audit or retention decision does not mean implementation or
  deletion is complete. The task type states what its checkbox covers.
- For implementation, deletion, and build changes, check the item only after
  the applicable verification below passes and its evidence is recorded.
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

For a cleanup candidate, identify three states:

1. The original upstream reference, with its original NVEL revision.
2. The current mFVS version immediately before that cleanup.
3. The candidate containing only that cleanup.

Establish the current version's comparison with the original, then compare the
candidate with both the original and the immediate pre-change version. This
distinguishes inherited differences from cleanup regressions. If a difference
already exists, record it; do not redefine the baseline to conceal it.

Follow [Contributing.md](Contributing.md), including its documented linkage-only
accommodations for building the original BC/ON reference. Record every such
accommodation separately from scientific changes. Keep the pristine reference
identity and the exact reference-build patch available with the evidence.

For each implementation task:

- Derive all affected variants from the source lists, including consumers that
  use another variant's implementation.
- Use the same compiler versions, relevant flags, platform, and inputs for
  reference and candidate comparisons. Record these details and both submodule
  revisions.
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
file. Verify that the compiled input set remains unchanged and record the
applicable build/parity evidence. A missing direct caller alone is insufficient.

The README reports original-reference agreement for 22 variants. That result
was not rerun in this audit. BC and ON currently stop during keyword input in
both versions; matching those failures does not prove simulation parity. Their
recorded defects must be handled separately. A deliberately corrected reference
can help diagnose those defects, but it must remain distinct from the original
upstream reference and cannot be reported as pristine-original parity.

- [x] **P01 — Identify and prepare the original reference (audit/build).**
  Pin both original revisions, record compiler settings and any linkage-only
  accommodations, and retain reference-build provenance. Do not fix model bugs
  in the reference as part of this task. Completed 13 November 2026: original
  and current IE/BC/ON builds and source integrity are recorded in the retained report.
- [x] **P02 — Establish one exact IE comparison (verification).** Start with
  one IE test deck. Compare original, current, and candidate outputs with narrow
  metadata normalization, and demonstrate that a real mismatch fails the
  comparison before using it to accept cleanup. Completed 13 November 2026:
  all seven IE artifacts match across original/current/candidate, and ten
  deliberately introduced mismatches are rejected. This covers `iet01` only.
- [x] **P03 — Establish the BC/ON verification boundary (audit; blocked for
  simulation parity).** Reproduce and record the keyword-input limitation and
  its relation to bugs 1, 2, and 5 in `FOUND_BUGS.md`. This task records the
  boundary; repairs and restored simulation verification require their own
  tasks. Do not check off an affected consolidation as verified while its
  required BC/ON simulation coverage remains blocked. The boundary audit was
  completed 13 November 2026: both BC decks and the ON deck reproduce input
  failures in original and current code. Model bugs remain unfixed, and
  simulation parity remains blocked.

  **Limited acceptance exception, 14 November 2026:** the user accepted exact
  routine and runtime-object comparisons for completion of C01 and C06 only,
  while leaving BC/ON simulation parity blocked. This does not relax the gate
  for other tasks. See the [acceptance record](tests/parity/evidence/2026-11-14-c01-c06/acceptance.json).

  **Additional limited acceptance, 14 November 2026:** the user accepted exact
  routine/file-output comparisons and executable-statement evidence for C07
  and C10. BC/ON simulation parity remains blocked, and this grants no general
  exception for other tasks. See the [acceptance record](tests/parity/evidence/2026-11-14-c07-c12/acceptance.json).

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

Historical source lists under `canada/bin/` still reference older components.
Current build exclusion and historical/custom-build support are separate facts.
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
- [ ] **C13 — First FIA database binding helper.** Extract one contiguous
  common field group from `dbsqlite/dbs_fiavbc_trls.f90` and
  `dbsqlite/dbs_fiavbc_atrtls.f90` into `dbsqlite/`. Preserve column order,
  types, conversions, and writer-specific tree-ID formatting. Compare schemas
  and rows; leave the report-selection logic in its existing callers.
- [ ] **C14 — FIA cut-list caller migration.** After C13, migrate
  `dbsqlite/dbs_fiavbc_cutlst.f90` to that helper. Preserve cut-list selection
  and probability semantics. Keep this separate from the initial extraction.
- [ ] **C15 — First mortality-allocation helper.** Extract the verified common
  allocation tail from CA `VARMRT` and NC `NWCMRT` into `vbase/`. Keep species
  efficiency calculations outside the first extraction. Preserve arithmetic
  order, stopping conditions, array updates, and debug messages. Include CA,
  OC, and NC consumers and cases reaching allocation/saturation boundaries.
- [ ] **C16 — TT/UT mortality caller migration.** After C15, migrate `TTMRT`
  and `UTMRT` to the helper. Preserve their distinct efficiency calculations;
  reuse only the verified allocation portion.
- [ ] **C17 — SO mortality caller migration.** After C15, migrate `SCOMRT` in
  its own change and verify SO against the original and immediate baseline.
- [ ] **C18 — WS mortality caller migration.** After C15, migrate WS `VARMRT`
  in its own change and verify WS against both baselines.

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
  2023 and should be deleted after testing. This is the strongest apparently
  forgotten cleanup. Confirm the replacement's testing record and retain the
  historical implementation reference. Completed 13 November 2026: removed
  the unselected old implementation, retained exact recovery references and
  its differences from the selected implementation, and verified standard SN
  plus targeted NC comparisons. No separate historical 2023 test receipt was
  found; the present verification and its limits are in the retirement report.
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
  lists still contain stale references, documented in the retirement report.
- [x] **D04 — `canada/on/findag_for.f90`.** Completed 13 November 2026:
  retired the unselected FINDAG alternative. ON keeps `canada/on/findag.f90`;
  HTCALC argument and initialization differences are preserved in the report.
- [x] **D05 — `canada/on/unblnk.f90`.** Completed 13 November 2026:
  retired the unselected ON clone. Preserved its embedded-space semantics and
  historical purpose; original and current builds select `base/unblnk.f90`.
- [x] **D06 — `canada/bc/unblnk.f90`.** Completed 13 November 2026:
  independently verified BC exclusion and retired its clone. The shared
  utility and its existing space-removal behavior are unchanged.
- [x] **D07 — `metric/dbsqlite/dbstreesin.f90`.** Completed 13 November 2026:
  retired the older 30-argument reader. Shared DBSTREESIN remains selected;
  the live 33/30 mismatch stays open in `FOUND_BUGS.md`. Verification tracing
  now explicitly records this absent unselected historical source.
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

The [D04–D13 report](tests/parity/evidence/2026-11-13-d04-d13/README.md)
contains an individual record, replacement diff, Git blob and recovery command
for every completed file below as well as D04–D11.

### D12 — Five separate ESTUMP copies

Every current variant selects `estb/estump.f90`. These five completed reviews
preserve individual scientific differences and keep that shared implementation.

- [x] **D12a — `ca/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 44 and bookkeeping differences against shared ESTUMP.
- [x] **D12b — `cr/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 20 and its separate replacement diff.
- [x] **D12c — `tt/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 6 and its separate replacement diff.
- [x] **D12d — `ut/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 6 and its separate replacement diff.
- [x] **D12e — `wc/estump.f90`.** Retired 13 November 2026; preserved aspen
  index 26 and its separate replacement diff.

### D13 — DMCW alternatives, one file per task

These retired files defined unselected DMCW implementations. Selected
`canada/newmist/dmcw.f90` still uses `CRWDTH`; the BC mistletoe pathway remains
active. Each file's equations, DATA tables, replacement diff and recovery
identity were recorded separately. No explicit abandonment decision was found
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

### D14 — Dormant ESUCKR copies, one file per task

Current builds select `estb/esuckr.f90`, `strp/esuckr.f90`, or
`metric/strp/esuckr.f90`, according to the variant. Record which replacement
applies and any scientific differences before retiring each dormant copy.
D14 retains the original audit's ID for CI.

- [ ] **D14 — `ci/esuckr.f90`.** Compare with CI's selected implementation.
- [ ] **D14-02 — `ca/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-03 — `cr/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-04 — `em/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-05 — `tt/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-06 — `ut/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-07 — `wc/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-08 — `canada/bc/esuckr.f90`.** Separate replacement comparison.
- [ ] **D14-09 — `canada/on/esuckr.f90`.** Separate replacement comparison.

## R — Dormant feature preservation decisions

These are **decision tasks**, not whole-directory deletion tasks. Each first
produces a build-boundary inventory, historical-purpose summary, and explicit
retain/defer/retire decision. Split any approved removal into file-sized tasks
with recovery references and verification. Checking a decision off never marks
all files in that directory as deleted.

| Area at audit baseline | Unlisted Fortran files | Historical interpretation |
|---|---:|---|
| `archive/bgc/` | 31 | Documented ended special research project |
| `dbs/` | 47 | Alternative ODBC database implementation |
| `dfb/` | 30 | Douglas-fir beetle extension |
| `dftm/` | 40 | Douglas-fir tussock moth extension |
| `lpmpb/` | 58 | Mountain pine beetle extension |
| `wpbr/` | 44 | White pine blister rust extension |
| `wsbwe/` | 61 | Budworm extension |
| `wwpb/` | 59 | Westwide pine beetle model |
| `acd/` | 27 | Unbuilt geographic model implementation |

- [ ] **R01 — BGC retention decision.** The
  [2021 note](changeNotes/2021.07.28_ldavid.txt) says the project ended, the
  model had not been built for over five years, and no future use was expected.
  Record that evidence and a recovery reference. ON still selects
  `base/exbgc.f90`; the active stub is a separate matter.
- [ ] **R02 — ODBC support decision.** Inventory the boundaries of `dbs/`,
  `vdbs/`, and their metric counterparts. Historical notes distinguish ODBC
  and SQLite products; current products select SQLite. Decide whether ODBC
  remains supported before scheduling any file removals.
- [ ] **R03 — Douglas-fir beetle support decision (`dfb/`).** Current builds
  select stubs. No evidence found establishes that the science was abandoned.
- [ ] **R04 — Tussock moth support decision (`dftm/`).** Current builds
  select stubs; establish feature intent before proposing removals.
- [ ] **R05 — Mountain pine beetle support decision (`lpmpb/`).** Current
  builds select stubs; establish feature intent before proposing removals.
- [ ] **R06 — Blister rust support decision (`wpbr/`).** Current builds
  select stubs; establish feature intent before proposing removals.
- [ ] **R07 — Budworm support decision (`wsbwe/`).** Current builds select
  stubs; establish feature intent before proposing removals.
- [ ] **R08 — Westwide pine beetle support decision (`wwpb/`).** Internal
  comments about replaced output routines do not establish retirement of the
  whole model. Preserve its science pending an explicit support decision.
- [ ] **R09 — Acadian model support decision (`acd/`).** Treat the unbuilt
  geographic implementation as a support-scope decision, not ordinary
  duplicate removal.
- [ ] **R10 — Northern Idaho archive decision (`archive/ni/`, 34 files).**
  Establish replacement relationships and preserve scientific alternatives.
- [ ] **R10b — Alaska archive decision (`archive/ak/`, 4 files).** Review
  independently of the Northern Idaho archive.
- [ ] **R10c — Fire archive decision (`archive/fire/`, 9 files).** Review
  independently and retain replacement/recovery references.
- [ ] **R10d — Parallel-processing declarations (`archive/PPEcommons/`, 13
  files).** Trace historical PPE use and distinguish it from active stop/restart
  support before deciding on retention.

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

## Completion evidence

Append a record whenever a task is completed, retained, deferred, or blocked.
Keep its checkbox state consistent with the task's stated type. A retention
decision can complete a decision task without deleting code. An audit that
finds a blocker can complete its investigation while its implementation
follow-up remains open under a new ID.

For code changes, retain comparison results outside transient terminal output
and record a stable path or committed report. Use a compact record such as:

```text
Task ID:
Date:
Task type and outcome:
Commit or patch identity:
Files changed / canonical shared home:
Affected variants and source-list evidence:
Original main and NVEL revisions:
Immediate pre-change main and NVEL revisions:
Candidate identity / working-tree patch identity:
Compiler versions, flags, platform, and reference-build accommodations:
Test decks and targeted branches exercised:
Original/current/candidate comparisons and allowed metadata normalization:
Results, receipt/report paths, and unresolved coverage:
For retirement: historical purpose, replacement, support decision, recovery ref:
Follow-up task IDs:
```

### Initial record

**A00, 13 November 2026 — completed documentation.** Stored the audit findings
and tracker before cleanup. No cleanup task was implemented and no new parity
claim was made. All cleanup and prerequisite checkboxes remain open. Existing
README verification claims are historical context pending task evidence here.

### P01–P03 record — 13 November 2026

**P01 completed:** prepared isolated original/current snapshots, pinned both
NVEL revisions, built IE/BC/ON with the same recorded settings, and checked
archive integrity. Only the two original Canadian link manifests changed,
exactly as retained in `reference-build.patch`; original model sources did not.

**P02 completed for `iet01`:** original, current, and the verification-only
candidate agree on all seven generated artifacts, including all 17 SQLite
tables and 14,178 rows. The candidate's complete selected build inputs equal
current mFVS, so its binary reuse is explicit; it ran in its own process and
directory. Ten negative controls reject real output changes, including a
one-ULP REAL change and an incorrect CaseID relationship. The shared STOP 20,
one FVS04 error, and four FVS09 warnings are preserved rather than suppressed.

**P03 boundary audit completed:** both BC decks and ON reproduce invalid
keywords and STOP 20 before producing simulation summaries or output
databases. Source tracing confirms bugs 1, 2, and 5 are still present. Bug 2's
documented totals were corrected to 33/30 (previously 31/28); no interface or
model bug was fixed. BC/ON simulation verification remains blocked.

Evidence, exact scope, flags, artifact replay, and known limitations are in the
[P01–P03 report](tests/parity/evidence/2026-11-13-p01-p03/README.md) and
[verification tool instructions](tests/parity/README.md). At completion of
P01–P03, every C, M, D, R, U, B, and L item remained unchecked. That work does
not establish parity for all
variants or all input branches.

### D01–D03 record — 13 November 2026

**D01, D02, D03 completed:** removed `sn/sitsetOld.f90`, `rd/rdrout.f90`, and
`vbase/natcrz.f90` only. These unselected sources total 1,115 lines / 32,464
bytes. Exact blobs, SHA256 values, and recovery commands are retained against
pre-deletion commit `3b26bc3621dffb53a64dd9ebd6dcca84a2fbf784`.

The complete selected build inputs for all 24 variants are unchanged. All
other 2,145 main-repository model source/include files checked are unchanged,
as is NVEL. The active SN SITSET, General Report Writer, root-disease stub
ENTRY, and Canadian NATCRZ stub remain intact. Two unused, already incomplete
Canadian source lists retain historical references; no current recipe selects
those lists. Their disposition remains separate from these three removals.

Fresh original/pre-deletion IE and SN builds succeeded. Four cases, each run
in original/current/candidate processes, pass all twelve pairwise comparisons:
`ie-iet01`, `sn-snt01`, and two first-stand SN cases for NC locations 81103 and
81105. All artifacts, database rows, baseline warnings/errors, and exit codes
are compared using the existing contract. Candidate binaries are explicitly
reused because selected inputs match; no recompilation is implied. Existing
repository test decks were not edited.

See the [retirement report](tests/parity/evidence/2026-11-13-d01-d03/README.md)
for historical purpose, preservation decisions, exact verification scope,
source identities, and replayable receipts. No model bug was fixed. BC/ON
simulation parity remains blocked as recorded by P03. Every other cleanup
checkbox remains open.

### D04–D13 record — 13 November 2026

**D04–D11, D12a–e, D13 and D13-02–15 completed:** independently reviewed and
removed 28 unselected files, totaling 6,145 lines / 189,493 bytes. Each file has
its own historical-purpose assessment, scientific/platform differences,
selected replacement, exact Git blob/SHA256, DATA literals, replacement diff
and recovery command. The pre-deletion revision is
`9bc35a16d6dec5fc79f513ae524297077e5298e3`; NVEL remains
`a73e697e220ffd0f792aa7ac10d84c911edb7ad7`. The per-file plan was stored before
deletion. All 28 alternatives were also unselected in original F77 `c2105365`.

Every variant's complete selected build input set is unchanged. The other
2,117 tracked main-repository model source/include files, all current NVEL
files and 34 checked build recipes are unchanged. Existing shared model homes
remain selected; no new model definitions or wrappers were introduced. Two
unused, already incomplete Canadian manifests retain their historical
references, with their path resolution and missing-input counts documented.

Eight fresh original/pre-deletion IE, SN, BC and ON builds succeeded. Four
candidate binaries were explicitly reused because complete selected input
hashes and flags match. Separate original/current/candidate IE and SN runs pass
all six pairwise comparisons, including all generated artifacts and SQLite
schemas/typed rows. Three Canadian original/current comparisons also pass at
the known invalid-keyword/STOP 20 boundary. These are not BC/ON simulation
parity claims; candidate Canadian execution is not claimed. The nine reports
replay identically from the retained raw artifact archive.

The retirement verifier rejects selected-source changes, unselected-model
changes and reintroduced retired files. The signature tracer was adapted only
to recognize the absent, unselected D07 source and identify Canadian runs by
variant. Its live-interface checks and all output comparison rules remain
strict. No model bug was fixed. D07/D08 bug-tracker references now describe
completed retirements; all five confirmed defects remain open. D11's formerly
active numerical guard is recoverable and logged separately as an unconfirmed
concern requiring later reachability/impact investigation.

See the [per-file findings and verification report](tests/parity/evidence/2026-11-13-d04-d13/README.md)
for scientific differences, history, scope limits, source identities, exact
commands and replay instructions. Prior P01–P03 and D01–D03 evidence bytes are
unchanged. D14 and every C/M/R/U/B/L task remain pending. The tracker now has
35 completed and 85 pending items.

### C01–C06 record — 14 November 2026

**All six implementations are complete and checked off.** The user accepted
the alternative Canadian evidence for C01/C06 on 14 November 2026.
The fourteen selected definitions now use six canonical definitions in `estb/`
and `fire/base/`, reducing model source by 1,484 lines (2,652 to 1,168).
The pre-cleanup revision is `1a8aef8b210ebf532d8c94291fea511f11a6a19a`;
NVEL remains `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.
The per-task plan was stored before edits. Each task retains its consumer map,
public signature, old-source hashes/blobs, replacement diffs and recovery
commands in the [C01–C06 report](tests/parity/evidence/2026-11-14-c01-c06/README.md).

All 23 affected variants build in original, pre-cleanup and candidate states:
69 fresh builds, with no candidate reuse. All 72 pairwise comparisons across
24 integrated decks pass. Those comprise 63 comparisons for 21 US consumers'
simulation outputs and nine Canadian input-failure boundary comparisons.
All 126 additional direct comparisons pass across 42 routine/variant fixtures.
Raw artifacts replay exactly. Ten deliberate model/output mismatches remain
rejected, including one-ULP REAL changes and wrong CaseID relationships.

C01 removes only the unused ESCOM2 declaration from shared ESTIME and the STRP
twin. C02/C03 preserve all equations and dimensioned tails for CI/IE/KT.
C04 preserves the complete 11/12-element mapping tables. C05 preserves OP-only
species-24 grouping, PN/WC defaults and arithmetic order. C06 preserves the
CI-only species-2/8 adjustment at DBH >= 18 for all eight consumers.
The other 2,103 model source/include files, all COMMON declarations, current
NVEL, test decks and historical evidence are unchanged. All 24 source sets
have exactly the planned definitions and filename selection.

The comparator's metadata recognizer was extended for source-verified CA/CR/TT
GROHED layouts: old title characters survive between metadata fields. Both
gaps are retained byte-for-byte; only version/revision/date/time are masked.
Controls reject changed title characters in either gap. No model/header code
was fixed. A stale NC mapping comment is recorded in `FOUND_BUGS.md`; numeric
mapping 19 and all confirmed model defects remain unchanged.

**C01/C06 acceptance, 14 November 2026:** Canadian simulations still stop on
existing input bugs. Direct tests and full runtime-object
sections/constants/relocations/common sizes match original/current/candidate
for BC/ON ESTIME and BC FMSFALL, but no Canadian integrated simulation result
is claimed. The user explicitly answered “Accept this evidence for C01/C06”,
authorizing completion of these two checkboxes using that evidence. The
[acceptance record](tests/parity/evidence/2026-11-14-c01-c06/acceptance.json)
retains the scope and response. The P03 simulation boundary remains blocked;
the exception applies only to C01/C06. No model bug was fixed and no later
cleanup task was implemented. There are now 41 completed and 79 pending items.


### C07–C12 record — 14 November 2026

**All six implementations are complete and checked off.** The user accepted
the alternative Canadian evidence for C07/C10 on 14 November 2026.
The thirteen selected definitions now use six shared definitions, reducing
model source by 2,229 lines (4,587 to 2,358). Eleven former copies are removed.
The pre-cleanup revision is `92b1deeb7554b50af60e271dddcd6c7413cbb70e`;
NVEL remains `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.
The plan was stored before edits. All source hashes/blobs, public signatures,
consumer rewrites, scientific differences and recovery commands are retained
in the [C07–C12 report](tests/parity/evidence/2026-11-14-c07-c12/README.md).

All 72 state/variant build identities succeeded: 26 fresh compilations and
46 retained binaries with exact selected input, compiler/flag/platform and
binary-hash matches. Reuse is explicit in the receipt. All 24 candidates are
fresh builds, and every state runs independently. All 81 integrated pairwise
comparisons pass: 66 standard US output comparisons, six CS/NE calibration
debug comparisons and nine Canadian failure-boundary comparisons. All 120
direct comparisons across 40 routine/variant fixtures also pass. Raw artifacts,
including actual ESADDT export/response files, replay exactly. The comparator
is unchanged and remains bound to its retained mismatch/header controls.

C07 preserves the CS/SN species-1 distinction and the eight earlier shared
consumers' executable body. C08 captures all 210 assigned initialization/helper
variables before growth. C09 exercises actual intercept/slope diagnostics
through complete engine runs. C10 preserves both seedling-bin profiles and
matches the entire original executable body after resolving its constant
species index. C11 captures all 37 initialized variables/species, including the
original final unit conversion. C12 preserves and calls all 89 mapping entries.
All 2,096 other model source/include files, COMMON declarations, NVEL, repository
input decks and earlier evidence remain unchanged. No model bug is fixed.
ESADDT data-flow concerns are recorded for later reachability/impact review.

**C07/C10 acceptance, 14 November 2026:** existing bugs still prevent BC/ON
integrated simulation. Their direct and executable-statement evidence is
recorded separately; C07's generated code has different local stack offsets,
so no runtime-object equality is claimed. The user explicitly answered
“Accept this evidence for C07/C10”, authorizing completion using the exact
routine/file-output and executable-statement evidence. The
[acceptance record](tests/parity/evidence/2026-11-14-c07-c12/acceptance.json)
retains the response and scope. The P03 simulation boundary remains blocked;
this additional exception applies only to C07/C10. No model bug was fixed and
no C13+ or D14 cleanup was implemented. The tracker has 47 completed and
73 pending items.
