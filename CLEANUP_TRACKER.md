# mFVS cleanup audit and tracker

Created: 13 November 2026.

Audit baseline: main repository `776491e8850b447e638f7b267b8bcaab641584f4`,
with NVEL submodule `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`.

This document stores the redundancy and dormant-code findings before cleanup
begins. The audit was read-only. No builds or simulations were run during the
audit, and no source, build configuration, source list, test input, or reference
output was changed. Creating this tracker is a documentation-only step; the
model cleanup tasks below have not begun. P01–P03 verification preparation is
now complete; see the [retained report](tests/parity/evidence/2026-11-13-p01-p03/README.md).

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

- [ ] **C01 — ESTIME.** Merge `estb/estime.f90` and `strp/estime.f90` into
  `estb/`. Their normalized statements differ only by the `ESCOM2` include.
  Verify its declaration effects before removing the twin. Check BC/ON coverage.
- [ ] **C02 — CI/IE ESPSUB.** Centralize in `estb/`. IE additionally zeros
  `PSUB(20:23)`; preserve species dimensions and initialization. Exercise those
  tail entries in the comparison.
- [ ] **C03 — CI/IE ESPXCS.** Centralize in `estb/`, independently of C02.
  IE additionally zeros `PXCS(20:23)`; preserve that behavior.
- [ ] **C04 — KT/NC FMCROW.** Centralize in `fire/base/`. The normalized
  statement differences are the `ISPMAP` tables. Preserve all mappings and
  table lengths; exercise every mapped species.
- [ ] **C05 — OP/PN/WC FMCFMD.** Merge `fire/vop/fmcfmd.f90` and
  `fire/vpn/fmcfmd.f90` into shared fire code. OP includes species 24 in a
  basal-area group; the PN/WC implementation excludes it. Test that distinction.
- [ ] **C06 — CI/IE FMSFALL.** Centralize in `fire/base/`. CI applies an extra
  `MAX(0.01, BASE*0.32)` adjustment for species 8 and 2. Derive all consumers of
  the IE implementation, including BC, and preserve the adjustment boundary.
- [ ] **C07 — CS/SN FMSFALL.** Centralize in `fire/base/`. Preserve the
  small-tree condition: `KSP > 2` for CS versus `KSP /= 2` for SN.
- [ ] **C08 — PN/WC GRINIT.** Centralize in `vbase/`. Preserve the four
  statement differences: variant code, default elevation, default forest, and
  default longitude. Compare initialization before growth.
- [ ] **C09 — CS/NE CRATET.** Centralize in `vbase/`. Beyond declaration
  ordering, NE debug output includes the intercept. Preserve diagnostics as
  well as calculations.
- [ ] **C10 — ESADDT.** Merge `estb/esaddt.f90` and `strp/esaddt.f90` into
  `estb/`. Preserve the different Douglas-fir indices, 5 versus 3. Bug 3 in
  `FOUND_BUGS.md` requires a separate model-changing fix, not a correction
  hidden in this consolidation.
- [ ] **C11 — PN/WC FMVINIT.** Centralize in shared fire code. Species 6 has
  separate PN defaults but joins another group in WC. Exercise that species
  explicitly and preserve the initialization sequence.
- [ ] **C12 — OC/OP ORGSPC.** Centralize in `organon/`. Preserve both complete
  species translation tables and their lengths; compare every species mapping.
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

- [ ] **D01 — `sn/sitsetOld.f90`.** Its header says it was replaced in Q2
  2023 and should be deleted after testing. This is the strongest apparently
  forgotten cleanup. Confirm the replacement's testing record and retain the
  historical implementation reference.
- [ ] **D02 — `rd/rdrout.f90`.** Its header explicitly declares it obsolete
  after General Report Writer implementation in 2015. The old call is commented
  out in `base/fvs.f90`. Strong retirement candidate.
- [ ] **D03 — `vbase/natcrz.f90`.** The
  [2021 retirement note](changeNotes/2021.07.28_ldavid.txt) says its cruise
  postprocessor was unavailable. Review only this unselected US implementation;
  keep `canada/bc/natcrz.f90`, which BC and ON still select.
- [ ] **D04 — `canada/on/findag_for.f90`.** Unselected second FINDAG
  implementation; ON selects `canada/on/findag.f90`. It appears superseded,
  but record its scientific differences before removal.
- [ ] **D05 — `canada/on/unblnk.f90`.** Unselected utility copy; current
  builds use shared `base/unblnk.f90`.
- [ ] **D06 — `canada/bc/unblnk.f90`.** Review independently of D05; the
  shared utility remains active.
- [ ] **D07 — `metric/dbsqlite/dbstreesin.f90`.** Unselected older
  30-argument implementation. The selected shared implementation has the
  newer interface; its relationship to the stale caller is documented in
  `FOUND_BUGS.md`.
- [ ] **D08 — `canada/fire/on/fmcrowe.f90`.** Unselected ON implementation
  with an old FMSVL2 call, also identified as uncompiled in `FOUND_BUGS.md`.
- [ ] **D09 — `base/myopen_pc.f90`.** Unselected platform-specific MYOPEN
  alternative. Current builds select `base/myopen.f90`. Establish whether
  supporting the old alternative remains desirable before removal.
- [ ] **D10 — `canada/on/r9init.f90`.** Unselected older volume
  implementation. Current builds select NVEL's `r9init.f90`. Record scientific
  differences and recovery provenance rather than assuming interchangeability.
- [ ] **D11 — `volume/r9clark_fvsMod.f90`.** Unselected older profile-volume
  implementation. Current builds select NVEL's Clark routines. Preserve a
  historical reference and record meaningful scientific differences.

### D12 — Five separate ESTUMP copies

Every current variant selects `estb/estump.f90`. Review and remove, if approved,
only one dormant copy per task; keep the shared implementation.

- [ ] **D12a — `ca/estump.f90`.** Verify build exclusion and replacement.
- [ ] **D12b — `cr/estump.f90`.** Verify build exclusion and replacement.
- [ ] **D12c — `tt/estump.f90`.** Verify build exclusion and replacement.
- [ ] **D12d — `ut/estump.f90`.** Verify build exclusion and replacement.
- [ ] **D12e — `wc/estump.f90`.** Verify build exclusion and replacement.

### D13 — DMCW alternatives, one file per task

These unselected files define DMCW implementations. Selected
`canada/newmist/dmcw.f90` uses `CRWDTH`. Compare and record each old equation
set before deciding whether it can be retired; do not remove the whole
`canada/newmist/` directory. D13 retains the original audit's ID for Alaska.

- [ ] **D13 — `canada/newmist/dmcwak.f90`.** First bounded comparison.
- [ ] **D13-02 — `canada/newmist/dmcwbm.f90`.** Separate comparison.
- [ ] **D13-03 — `canada/newmist/dmcwci.f90`.** Separate comparison.
- [ ] **D13-04 — `canada/newmist/dmcwcr.f90`.** Separate comparison.
- [ ] **D13-05 — `canada/newmist/dmcwec.f90`.** Separate comparison.
- [ ] **D13-06 — `canada/newmist/dmcwem.f90`.** Separate comparison.
- [ ] **D13-07 — `canada/newmist/dmcwib.f90`.** Separate comparison.
- [ ] **D13-08 — `canada/newmist/dmcwkt.f90`.** Separate comparison.
- [ ] **D13-09 — `canada/newmist/dmcwnc.f90`.** Separate comparison.
- [ ] **D13-10 — `canada/newmist/dmcwni.f90`.** Separate comparison.
- [ ] **D13-11 — `canada/newmist/dmcwso.f90`.** Separate comparison.
- [ ] **D13-12 — `canada/newmist/dmcwtt.f90`.** Separate comparison.
- [ ] **D13-13 — `canada/newmist/dmcwut.f90`.** Separate comparison.
- [ ] **D13-14 — `canada/newmist/dmcwwc.f90`.** Separate comparison.
- [ ] **D13-15 — `canada/newmist/dmcwws.f90`.** Separate comparison.

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
[verification tool instructions](tests/parity/README.md). Every C, M, D, R, U,
B, and L item remains unchecked. This work does not establish parity for all
variants or all input branches.
