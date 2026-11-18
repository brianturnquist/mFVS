# mFVS single-program plan: one program, 24 default configurations

Created 17 November 2026 from a read-only static audit of `d31a9ff0`. That tree
also had uncommitted edits to `tests/parity/check_suite.py` and
`prepare_builds.py`. All tasks are pending. No builds, simulation runs or timings
were made for this plan. The counts below come from regex parsing of the 24
source lists. They are approximate and must be refreshed in SP01/SP02 before
any edit.

## Goal

Today each variant is a separate program (`bin/FVSxx`, `FVSxx.so`) built from
its own source list and its own `PRGPRM`. The target is one program in which
each variant is a **default configuration**. Running with a variant's default
must reproduce that variant's current results exactly. A user can then inspect
each default and, in a later stage, replace individual components with an
alternative, including one taken from another variant.

## Verdict

**Feasible and worth doing.** It also fits where the code is now, but the goal
divides into two stages of different kinds:

- **Stage A: one program, 24 fixed defaults, bit-identical results.** This is
  mostly mechanical work. It is lower-risk than the semantic merges already
  done, and it ends the per-variant duplication without having to merge every
  family.
- **Stage B: user-selectable components across variants.** This is feasible
  only per component, as an explicit opt-in, with a species crosswalk and
  component-owned coefficient tables. It depends on the state-ownership phases
  of [MODULE_MODERNIZATION_PLAN.md](MODULE_MODERNIZATION_PLAN.md). Free mixing of
  every component, especially the core diameter- and height-growth equations,
  is not scientifically neutral. It should be offered as documented research use,
  not as an ordinary option.

Stage A is clearly the better end state. Stage B is reasonable component by
component, with a decision for each one. SP14 is a natural stopping point:
after it, there is one source set and only the default configuration differs.

## Audit findings

### What already favours one program

- **Shared state layouts are already unified.** There are 88 COMMON blocks in
  include files, and each is declared in exactly one file. Only three includes
  still differ by variant: `PRGPRM` (24 versions), `FMBIOGRP` (BC, IE and KT)
  and `DGCONS_ENTRY` (EM). There are 11 COMMON blocks written directly in
  source files, and each appears in only one set of files. Conflicting COMMON
  layouts would normally be the hardest part of combining programs, and that
  work has largely been done by the consolidation.
- **A dispatch pattern already exists.** `vbase/dgf.f90`, `vbase/varput.f90`,
  `vbase/dgdriv.f90` and others select per-family code on `VARIANTCD` or
  `VARACD`, and every family is compiled into every variant.
- **Variants are already compositions of borrowed equations.** Hundreds of
  source comments say so, for example "USES IE LM (ORIGINALLY FROM TT VARIANT)",
  "SURROGATE EQUATIONS FROM THE CR VARIANT" and "USING WC COEFFICIENTS".
  Treating variants as configurations matches how they were built.
- **There is a strong verification gate.** `tests/parity/check_suite.py` compares
  against the original F77 build and the corrected reference, including restart
  files. Earlier work also established shadow-comparison recipes inside a single
  executable.

### Size

| | Files | Compiled Fortran lines |
|---|---|---|
| One variant | 675–826 list entries | 183k (BC) – 246k (SO) |
| All 24 builds today (sum) | — | about 5.51 million |
| Union of all 24 lists | 1,355 compiled Fortran (+96 includes, 90 C/C++/headers) | 451,846 |

A single program compiles about 1.9 times the largest variant and about 8% of
what an all-variant build compiles today.

### Compile-time variant identity and sizes (what Stage A must convert)

| Item | Extent |
|---|---|
| `PRGPRM` values | `MAXSP` 11 (KT) – 108 (NE); `MAXTRE` 3000, BC 4000, ON 6000; `MAXPLT` 500, BC/ON/TT 200; `MAXCYC` 40 everywhere; `LMETRICVAR` true only for BC/ON |
| `VARIANTCD` (constant) | 932 lines in 60 files; about 11 inside `parameter` constant expressions (e.g. `IB = merge(…, VARIANTCD.eq.'TT')` in `vbase/dgf.f90`, `estb/esaddt.f90`, `estb/esfltr.f90`, `vbase/crown_pn.f90`) |
| `VARACD` (runtime, set in GRINIT) | 589 lines in 123 files; coexists with `VARIANTCD` |
| `LMETRICVAR` | 402 lines in 86 files; 5 in `parameter` expressions |
| `MAXSP` as loop bound or extent | 972 `do …=1,MAXSP` loops in 310 files, about 1,900 other references |
| `MAXTRE` | 97 loops and about 410 other references in 176 files. Dead/cut tree records occupy `IREC2..MAXTRE`, and API limits use it, so it governs behaviour as well as storage |
| `DATA` statements using `MAXSP` | 309, some with literal repeat counts tied to it (e.g. `ws/blkdat.f90` `688*0.0` = 16×43) |
| BLOCK DATA units | 87, about 75 of them per variant (`blkdat` ×24, `cubrds` ×20, `fmcblk` ×20, `rdblk1` ×15). A named COMMON may be initialized by only one BLOCK DATA in a program |
| Restart files | `pg/putstd.f90`/`getstd.f90` write explicit `MAXSP`/`MAXPLT`/`MAXTRE` extents and use EQUIVALENCE packing, so the file format depends on these sizes |
| API | `fvsDimSizes` returns `MAXTRE`/`MAXSP`; `fvsSpeciesAttrC` has a C-bound `dimension(MAXSP)` array; there is no variant-selection call |
| Entry names | Tests and clients run `bin/FVSxx` and load `FVSxx.so` (e.g. `tests/FVSie/makefile`) |

Sizing everything to the maximum is therefore not enough. Storage dimensions
and active extents must become separate quantities.

### Routine-name collisions in the union (each one is a link conflict)

211 external routine names have more than one definition across the 24 lists:

| Kind | Names | Extra definitions | Files | Lines |
|---|---|---|---|---|
| Variant equation families (BLKDAT, SITSET, FORKOD, CROWN/CRCONS, GRINIT, HTGF/HTCONS, CUBRDS, FINDAG, BRATIO, CCFCAL, HABTYP, DUBSCR …) | 72 | 419 | 387 | 113k |
| Fire per-variant (FMCBLK, FMVINIT, FMCBA/SNGCOE, FMCROW, FMCFMD, FMSFALL …) | 13 | 118 | 106 | 46k |
| Extension stub vs real (`ex*.f90` vs mistoe, clim, covr, rd, organon, econ …) | 81 | 113 | 109 | 28k |
| Establishment alternates (estb vs strp; AK/CI/EM/IE/KT copies) | 22 | 57 | 72 | 16k |
| Metric twins (BC/ON) | 21 | 26 | 31 | 33k |
| Root disease `RDBLK1`; `VOLS` | 2 | 17 | 19 | 3k |

The parser also reports six names defined twice inside one variant: CFTOPK
(BC, ON), BALMOD (CS, LS, NE) and R9CLARK (ON). These are probably internal or
ENTRY scopes the parser missed. SP02 must confirm this.

Extensions are chosen at link time. CS, LS, NE, SN and ON link the climate,
cover, mistletoe and root-disease stubs. ORGANON is linked only in OC/OP. Root
disease is real in 15 western variants and BC. Full establishment (`estb/`) is
used by AK, CI, EM, IE and KT, and partial (`strp/`) by the rest. BC and ON use
no NVEL (3 volume files against 152) and have their own metric, fire and
mistletoe code. They also lack pristine-original simulation parity.

### Costs and risks

- **The runtime-extent work is broad:** about 3,000 sites. A loop left on the
  storage dimension can read inactive storage. SP03's poisoned-storage mode is
  the safeguard.
- **Performance:** dispatch and loop bounds become runtime values. With
  `-ffp-contract=off` and no fast-math, floating-point results should not
  change, but R04/O21 showed that `-O2` rewrites need proof. Accept only
  bit-identical results, and time against the O34 baseline.
- **Undefined reads become visible after layout changes** (see FOUND_BUGS 53
  and 63). Repeat runs before calling a difference a regression.
- **Initialization lifetime:** BLOCK DATA values live for the whole process, and
  some are changed during a run without being reset per stand. Their
  replacements must keep that lifetime.
- **BC/ON** carry most of the remaining divergence and have the weakest
  references.
- **Stage B is science, not refactoring.** Equations index coefficient arrays by
  native species number. Those arrays sit in shared COMMON filled by one
  variant's initializer, and many components also read location, habitat or
  site codes specific to their variant.

## Relationship to MODULE_MODERNIZATION_PLAN

MOD01–MOD05 were in progress when this plan was written. Two of its tasks
assume compile-time per-variant configuration and conflict with this goal:

- MOD16 says "preserve each variant's exact values … no object/module reuse
  crosses incompatible parameter definitions".
- MOD22 says "do not introduce allocatable tree storage".

Recommended sequencing, to be confirmed in SP00:

- Start Stage A after MOD07, once module builds are proven.
- Let SP04–SP07 replace MOD16's configuration design, rather than building
  per-variant configuration modules and then undoing them.
- Have MOD12 batches create per-variant module procedures under SP11's naming,
  so each family's dispatcher gets one checked interface.
- Start Stage B only after the MOD17–MOD26 state ownership for the tables
  involved.

This plan does not edit the module plan.

## Rules for every task

1. Model results must not change. Each batch must be bit-identical against its
   immediate predecessor, the corrected reference, and original F77 (keeping the
   documented differences, and the stated BC/ON limits).
2. Work in bounded batches. Expand a family task into named batches before
   editing. Verify each batch before starting the next.
3. **Convert while 24 separate builds still exist.** SP04–SP14 keep the 24
   programs, so the existing suite verifies every step. The single program is
   linked only once every variant compiles the same sources (SP14/SP15).
4. Keep bug fixes separate. Record new defects in `FOUND_BUGS.md`.
5. The build may use nothing the original F77 build did not. Python remains
   allowed for test tools only.
6. Keep builds and outputs in scratch directories. Record a short dated result
   under each task, and add no evidence folders.
7. A rename-and-dispatch batch keeps implementations as they are. Merging two
   implementations is optional later clean-up, not part of this plan.

## Phase 0. Decisions

- [ ] **SP00. Settle scope, sequencing and interfaces.**
  Decide the following:
  - (a) Stage A only, or Stage A followed by Stage B.
  - (b) Whether BC/ON join the single program (SP19) or remain separate builds.
  - (c) How a run selects its default configuration: program name (`argv[0]`
    = `FVSxx`), `--variant=xx`, an API call, and/or the `Variant` column of
    `FVS_StandInit`.
  - (d) How the existing names `FVSxx` and `FVSxx.so` are kept, using links or
    launchers. For the library, the options are `dladdr` on its own path or a
    required selection call.
  - (e) The MOD16/MOD22 amendments and their ordering against MOD06–MOD12.
  - (f) Whether the 24 separate builds remain supported once SP15 lands.

  Acceptance: decisions recorded here, and the module plan amended by the user.

## Phase 1. Inventories and verification support

- [ ] **SP01. Configuration inventory.**
  Classify every compile-time variant input at the current HEAD into one of
  these kinds:
  - storage dimension
  - active extent
  - identity test
  - initializer (BLOCK DATA or DATA)
  - link-time selection (manifest-only files, stubs, alternate implementations)
  - per-variant include (`PRGPRM`, `FMBIOGRP`, `DGCONS_ENTRY`)

  Include `VARIANTCD`/`LMETRICVAR` inside `parameter` expressions, the `DATA`
  repeat counts, the PUTSTD/GETSTD extents, and the API arrays.
  Acceptance: every site classified, and totals reconciled with the audit tables.

- [ ] **SP02. Name-collision inventory.**
  For each of the 211 names, list the implementations, consuming variants,
  ENTRY points, argument lists, SAVE/DATA locals and callers. Compare signatures
  across the implementations of a family. A dispatcher needs one interface, so
  record every mismatch. Confirm the six apparent same-variant duplicates.
  Propose implementation names: `NAME_xx`, or `NAME_family` for files that
  several variants share, as in `vbase/htgf_ls.f90`.
  Acceptance: a family table with an ordered batch list (leaf routines first),
  with mismatches recorded.

- [ ] **SP03. Make the harness ready for a combined program.**
  Make the smallest extension to `check_suite.py` that can run any `tests/FVSxx`
  deck against a program in a chosen default configuration. Add a
  poisoned-storage mode: build with storage dimensions larger than the active
  extents, fill the inactive tail with signalling NaNs and integer sentinels,
  and run with the existing traps.
  Acceptance: a planted output change is detected, and a planted read of an
  inactive species slot traps.

## Phase 2. Make every variant runtime-ready (still 24 programs)

- [ ] **SP04. Make variant identity a runtime value.**
  Add a configuration owner, either a module or the existing CONTRL path per
  MOD decisions, and set it before anything reads it, earlier than GRINIT.
  Initially each program copies its `PRGPRM` constant into it. Convert the
  `parameter` expressions to runtime values. Reconcile `VARACD` with the new
  owner without changing anything printed or stored.
  Acceptance: no constant-expression use of `VARIANTCD` remains, and all suites
  are identical.

- [ ] **SP05. Separate species storage from the active species count.**
  Introduce the active count, defaulting to the variant's native `MAXSP`. Move
  loop bounds, array sections, restart extents and API extents to it, in
  batches by subsystem. Remove the `DATA` repeat counts tied to `MAXSP`.
  Acceptance per batch: identical results when storage equals the active count,
  and identical results under poisoned storage with `MAXSP` = 108.

- [ ] **SP06. Do the same for trees and plots.**
  Give `MAXTRE` an active limit, including the `IREC2..MAXTRE` dead-record
  region, compression thresholds and API limits. Treat `MAXPLT` the same way.
  Storage is sized for the largest configuration.
  Acceptance: identical results and byte-identical `.stop` files under both
  modes, including decks near the tree limit.

- [ ] **SP07. Convert BLOCK DATA to initialization routines.**
  Turn each per-variant BLOCK DATA into a uniquely named routine that assigns
  the same values, run once per process at the point where BLOCK DATA values
  took effect. Shared BLOCK DATA (e.g. `base/keywds.f90`, `dbsqlite/dbsblkd.f90`)
  may remain.
  Acceptance: a COMMON memory image taken right after initialization matches
  the old one byte for byte (shadow recipe), and multi-stand decks are identical.

- [ ] **SP08. Turn extension stubs into runtime switches.**
  Link the real implementations everywhere and guard them with configuration
  flags. Reproduce each stub's behaviour exactly, including messages and keyword
  rejection. Do one extension per batch: climate, cover, mistletoe, root
  disease, ORGANON, economics, fertilization, the `exhtdbh` stub, and others
  found in SP02.
  Acceptance: identical results with the extension disabled and enabled;
  extension keyword decks run in variants that previously linked the stub.

- [ ] **SP09. Establishment alternates.**
  Bring estb and strp, plus the AK/CI/EM/IE/KT copies, under the SP08/SP10
  pattern.
  Acceptance: identical regeneration output in both groups.

- [ ] **SP10. Rename and dispatch the variant equation families.**
  There are 72 names in about 113k lines, handled in the SP02 batch order. Give
  each implementation, and its ENTRY points, a unique name. One dispatcher per
  family keeps the original name and selects on the runtime configuration. Keep
  implementation bodies unchanged. Use module procedures where MOD06/MOD07 allow.
  Acceptance per batch: 24/24 identical, and no definition of the family's name
  remains outside its dispatcher.

- [ ] **SP11. Fire per-variant families.**
  Apply the SP10 pattern to 13 names in about 46k lines. `FMCBLK` follows SP07,
  and the three `FMBIOGRP` versions become per-variant data.
  Acceptance: identical results, including fire, snag and carbon outputs.

- [ ] **SP12. Remaining collisions.**
  Cover `RDBLK1`, `VOLS`, `DGCONS_ENTRY` (EM), the inline COMMON blocks, and
  anything else SP02 lists.
  Acceptance: the SP02 table has no open US-variant rows.

- [ ] **SP13. Performance check.**
  Time a representative set against the O34 baseline before the final link.
  Acceptance: an agreed bound, or the hot dispatch points named for a decision.

- [ ] **SP14. Converge the manifests (checkpoint).**
  Give the 22 US variants one common source list plus their own `PRGPRM`, and
  build all 22 programs from it.
  Acceptance: 22/22 identical. Stopping here is a valid outcome: one source set,
  22 builds, and only the default configuration differs.

## Phase 3. One program

- [ ] **SP15. Single program with a configuration table.**
  Use one `PRGPRM` holding the storage maxima, and a table of the 22 (or 24)
  defaults: identity, active extents, metric flag, extension flags and
  initialization routines. Implement the selection mechanism chosen in SP00,
  and fail clearly when no variant is selected.
  Acceptance: the program builds, and every default selects correctly.

- [ ] **SP16. Build and compatibility names.**
  Use one build directory for Make and CMake, and provide `FVSxx` and
  `FVSxx.so` as SP00 decided, working on Windows as well. `make FVSxx` and
  `make all` keep working.
  Acceptance: fresh, incremental and parallel builds succeed, and the test
  makefiles run unchanged.

- [ ] **SP17. Verify the single program.**
  Run the full suite for each default against original F77, the corrected
  reference, and the SP14 per-variant programs. Also run poisoned storage,
  multi-stand decks, stop/restart (byte-identical `.stop` files), API clients
  (`fvsDimSizes`, species attributes, tree add/cut), timing and memory.
  Acceptance: bit-identical results; coverage limits recorded honestly.

- [ ] **SP18. Retire the per-variant builds**, if SP00(f) says so.
  Remove the per-variant `PRGPRM` files and source lists, and update the
  README, Contributing.md and parity guidance.
  Acceptance: no dangling references, and the suite is unchanged.

- [ ] **SP19. BC and ON**, or a recorded decision to keep them as separate
  builds.
  Put `LMETRICVAR` under runtime control and handle the 21 metric twins (about
  33k lines) as runtime metric branches, keeping the BC/ON behaviour that
  depends on recorded bugs 1, 2 and 5. Bring in the Canadian fire, mistletoe
  and volume code, the absence of NVEL, and `MAXTRE` 6000.
  Acceptance: matches the immediate and corrected references, with the BC/ON
  limits stated.

## Phase 4. Configurable components (Stage B, conditional)

Start only if SP00 chose Stage B, and only after MOD17–MOD26 have given
ownership of the tables each component uses.

- [ ] **SP20. Configuration model and report.**
  Define a configuration as a default variant plus per-component overrides.
  Specify the selection syntax (a keyword or file). Print a report at the start
  of each run listing every component, the variant it comes from, and the
  species mapping. Database schema additions need explicit authorization.
  Acceptance: a design is agreed, and output is unchanged when there is no
  override.

- [ ] **SP21. Species crosswalk.**
  Map the configuration's species to a donor variant's species through FIA and
  PLANTS codes plus the donor's own surrogate rules. An unmapped species is
  either an error or requires a surrogate the user names; it is never silent.
  Acceptance: crosswalk tables for every donor variant, with tests.

- [ ] **SP22. Component pilot.**
  Choose a narrow, self-contained component such as bark ratio or crown width.
  Its coefficient tables become owned by the component rather than COMMON,
  callers pass mapped species, and the component is exposed as an override.
  Acceptance: default runs are unchanged, and override values equal the donor
  variant's equation for the mapped species (direct check).

- [ ] **SP23. Extend component by component**, in order of interface narrowness:
  height–diameter, crown, mortality method, establishment model, fire and snag
  tables. Volume is already selectable through keywords and NVEL. Where a
  component needs donor-variant location, habitat or site codes, it must map
  them. Substituting diameter or height growth comes last, is marked as research
  use, and relies on FVS's existing self-calibration.
  Acceptance per component: as SP22, plus documented provenance.

- [ ] **SP24. User documentation.**
  Generate per-variant tables of default configurations from the configuration
  table, showing which equations each variant uses and which variant they come
  from. Add guidance on when an override is scientifically defensible.
  Acceptance: the tables match the code. Changing any default is a separate,
  documented model change.

## Outside this plan

- Changing any default equation, coefficient, species list or mapping.
- Merging implementations beyond what a rename-and-dispatch batch needs.
- Dynamic tree storage beyond separating storage size from active extent.
- Running several stands or variants at once in one process. That belongs to
  the MOD28–MOD34 follow-on work.
