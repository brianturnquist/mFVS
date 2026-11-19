# mFVS modernization plan: checked modules and one program

Combined 19 November 2026 from the module and simulation-state plan (created
16 November) and the single-program plan (created 17 November). Both originals
are in Git at `28fb3ad9` as `MODULE_MODERNIZATION_PLAN.md` and
`SINGLE_PROGRAM_PLAN.md`. Task identifiers are unchanged: `MODnn` tasks come
from the module plan and `SPnn` tasks from the single-program plan, so
references in [FOUND_BUGS.md](FOUND_BUGS.md), the inventory and commit messages
still resolve. Where the two plans covered the same code, the tasks are now one
combined task. Phase 1 (MOD01–MOD05) is complete. Everything else is pending.

## Objectives

The plan has two goals that share most of their work. The first is to make
procedure calls compiler-checked and to give shared simulation data clear
definitions, ownership and initialization rules. The principal work is moving
procedures into modules and replacing internal COMMON declarations and
declaration includes with module-owned data. The second is to turn the 24
separately built variant programs into one program in which each variant is a
**default configuration**. Running a variant's default must reproduce that
variant's current results exactly.

Neither goal changes model results. Expected benefits are easier debugging,
fewer argument-contract mistakes, clearer data flow and an end to per-variant
duplication. The plan makes no execution-time or memory-saving claim. Moving
COMMON data into modules does not by itself allow independent simulations or
concurrent execution; that is the conditional MOD28–MOD34 follow-on.

The single-program goal has two stages of different kinds. **Stage A** is one
program with 24 fixed defaults and bit-identical results; it is mostly
mechanical work, lower-risk than the semantic merges already done. **Stage B**
lets a user replace individual components with an alternative taken from
another variant. That is science rather than refactoring: equations index
coefficient arrays by native species number, and many components read
variant-specific location, habitat or site codes. Stage B is therefore
conditional, component by component, and comes after state ownership.

Read [Contributing.md](Contributing.md), [FOUND_BUGS.md](FOUND_BUGS.md),
[the parity guidance](tests/parity/README.md) and
[the migration inventory](MODULE_INVENTORY.md) before starting work. This plan
refines the modernization steps in [README.md](README.md); it does not change
the scientific or product contract. The optimization checklist was retired in
`3f456123`; its decisions remain in Git at `399dfef7:OPTIMIZATION_TASKS.md`.

## How the two strands fit together

The module plan was written for compile-time, per-variant configuration. The
single-program plan makes the variant a runtime value. Followed separately,
several module tasks would build structures the single-program tasks then
undo, or would edit the same lines twice. This plan therefore orders the work
around four principles.

First, variant identity, species and tree extents, and BLOCK DATA become
runtime configuration **while the 24 separate builds still exist** (SP04–SP07),
so the existing suite verifies every step. This replaces MOD16, which would
have built per-variant configuration modules only to dismantle them.

Second, each procedure family is touched once. When an equation family is
moved into a module, its implementations receive their unique per-variant
names and dispatcher in the same batch (MOD12 with SP10). Extension stubs are
turned into runtime switches as their families are migrated, rather than
being moved into modules and then deleted (MOD14 with SP08, SP09 and SP11).

Third, BC and ON go through every phase with the U.S. variants rather than
being joined at the end (SP00(b), decided 19 November 2026). Their metric twins,
Canadian fire, mistletoe and volume code, and the larger tree limit are
handled by the same tasks that handle every other variant difference. A
Canadian readiness pass (SP19) first strengthens their weak test coverage and
repairs the few Canadian defects that would make those batches unverifiable.

Fourth, shared-state ownership (MOD17–MOD26) is done after the single program
exists, so each COMMON block is migrated once, in one source set, with storage
already separated from active extents. Restart compatibility (MOD17) is
designed after SP05/SP06 have changed which extents PUTSTD/GETSTD write.

| Original task | Change in this plan |
|---|---|
| MOD16 configuration modules | Superseded by SP04–SP07 |
| MOD12 equation families | Combined with SP10's rename-and-dispatch |
| MOD14 extension families | Per-variant fire with SP11, establishment with SP09, stubbed extensions with SP08; the rest stays MOD14 |
| MOD08 ENTRY groups | Done inside each family batch that contains them, as well as standalone |
| MOD09 API adapters | Existing symbols only; the variant-selection call and `FVSxx` names belong to SP15/SP16. Needs SP00(c)/(d) first |
| MOD17 restart design | After SP05/SP06 |
| MOD21 "do not pad layouts" | Species storage is padded to the maximum by design (SP05); native counts stay the active extents |
| MOD22 "preserve MAXTRE" | Preserve the active tree limit; storage is sized for the largest configuration. Still no allocatable storage |
| MOD25 BLOCK DATA reconciliation | Verification of SP07's initialization routines |
| SP19 BC and ON (last task of the one-program phase) | Moved before SP04 as the Canadian readiness pass; the Canadian conversion itself is spread across SP04–SP16 |

MOD06, MOD07, MOD10, MOD11 and MOD13 are unchanged in substance.

## Current starting points

These examples were inspected when preparing the plans; confirm their current
implementations and consumers before using them.

- `vbase/initdesign_mod.f90`, `estb/essize_mod.f90` and other existing modules
  provide examples of module procedures. They still include COMMON data, so
  checked calls and state migration can be separate changes.
- `base/fvs.f90` uses core includes such as `ARRAYS`, `CONTRL` and `PLOT` and
  exposes an external entry point. Its internal restructuring must preserve
  the existing library interface.
- `pg/putstd.f90` and `pg/getstd.f90` connect simulation state to restart
  files, write explicit `MAXSP`/`MAXPLT`/`MAXTRE` extents and use EQUIVALENCE
  for packing and storage reuse.
- `vbase/dgf.f90`, `vbase/varput.f90`, `vbase/dgdriv.f90` and others already
  select per-family code on `VARIANTCD` or `VARACD`, with every family compiled
  into every variant. This is the existing dispatch pattern.
- `bin/FVSxx_sourceList.txt` selects each variant's actual sources. Make
  compiles original paths, stages selected includes and uses `fvsbuild.c` for
  compiler dependencies, actual module names and USE ordering. Each variant
  builds in its own `bin/FVSxx_buildDir`; NVEL, SQLite and FOFEM objects are
  shared through `bin/.shared_build`.
- The parity suite defaults to pinned original F77. `--reference corrected`
  checks the fixed corrected source and an immediate commit separately.
  `--baseline` still controls only new-source discovery.

## Rules for every task

1. **Model results must not change.** Preserve equations, coefficients,
   species mappings, units, defaults, kinds, dimensions, expression and
   accumulation order, random draws, argument mutations, diagnostics, output
   formats, database content and restart data. Each batch must be
   bit-identical against its immediate predecessor and the corrected
   reference, and must keep the documented differences from original F77 and
   the stated BC/ON limits. Do not weaken comparison masks or introduce
   numerical tolerances to accept restructuring.
2. **Work in bounded batches.** Expand a family task into named routine or
   COMMON-block batches before editing, and verify each batch before starting
   the next. If a migration fails, correct or undo only that migration.
3. **Convert while the 24 separate builds still exist.** SP04–SP14 keep the 24
   programs, so the existing suite verifies every step. The single program is
   linked only once every variant compiles the same sources.
4. **Keep model corrections separate.** If a checked interface or a layout
   change exposes an invalid legacy call or undefined state, record it in
   `FOUND_BUGS.md` and defer that migration until a separate correctness change
   defines the intended behavior. Do not conceal a mismatch with casts,
   permissive interfaces, blanket initialization or compiler flags.
5. **A rename-and-dispatch batch keeps implementations as they are.** Merging
   two implementations is optional later clean-up, not part of this plan.
6. **Preserve external symbols,** calling conventions and executable/
   shared-library behavior. Inspect external callers, callbacks and exported
   COMMON symbols before moving or renaming their definitions.
7. **Preserve the existing commentary and formatting.** Use the repository's
   free-form style, `implicit none`, two-space indentation and banner comments.
   Explain dependencies, initialization, aliases and compatibility adapters
   where those details will help future debugging. Avoid unrelated
   reformatting.
8. **Exercise what changed.** Cover affected variants and all relevant unit
   systems and entry paths, and run the full suite for shared changes.
   Supplement existing decks only where they miss a behavior the change could
   alter, including initialization, multiple stands, callbacks, error paths
   and stop/restart.
9. **Keep the build honest.** The build may use nothing the original F77 build
   did not; Python remains allowed for test tools only. Check source-list
   membership and clean/incremental builds when module dependencies change,
   and run `git diff --check`.
10. **Keep evidence out of the repository.** Builds and raw outputs go in
    disposable temporary directories. For each completed task or bounded
    subtask, add a short dated result beneath its entry: affected
    families/variants, comparison outcome, relevant build/API checks, coverage
    limits and any linked bug. Leave unverified work unchecked.

## Verification commands

Original-F77 comparison:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

Corrected and immediate comparison, used for every migration:

```sh
python3 -B tests/parity/check_suite.py --reference corrected --cache-dir /private/tmp/mfvs-corrected
```

`--immediate REV` selects a committed pre-change version (default HEAD)
without changing the fixed corrected reference. If an immediate baseline
includes uncommitted work, make its source state reproducible before beginning
a batch. Use a distinct temporary cache/build location for each reference and
compiler configuration when existing cache identities do not already
distinguish them; a cached run does not demonstrate that a new module
dependency or flag was used. After layout changes, repeat a run before calling
a difference a regression (see the next section).

## Known bugs and the tasks they affect

No recorded bug needs fixing before Phase 3 or SP01–SP03, and the corrected
reference at `d31a9ff0` remains valid until SP19's repairs are re-pinned. The
following bugs constrain particular tasks and should be checked when a batch
reaches them.

| Task | Bugs | Consequence |
|---|---|---|
| SP03 | 58, 63 | Poisoned storage must fill inactive tails at run time. `-finit-*` flags skip locals in the 146 files with ENTRY and `-finit-real=snan` crashes the compiler on `vbase/morts.f90`. |
| SP04 | 3, 10 | `IDF` in `estb/esaddt.f90` and `LTPAINV` in `estb/esfltr.f90` are among the parameter expressions to convert. Convert them unchanged; do not fix them in the same batch. |
| SP05, SP07, SP10/SP12 for `MISINT` | 4 | Renaming is safe. Do not rewrite the mislabelled CA/OC/WS DATA blocks as assignments until the fix is decided: assignment makes the last value win, which would change `ADGP`. |
| SP06, SP17, MOD17 | 60, 70 and the root-disease restart limit | Compare restarted runs with restarted runs. IE restarted and continuous runs already differ in six tables, and BC and ON restarted projections diverge from the straight run (bug 70, recorded by the SP19 restart decks). |
| SP05–SP07, SP15 | 53, 56, 63 | Undefined values that depend on memory layout. None reaches standard outputs, but debug output and direct NVEL checks can change. Repeat runs before calling a difference a regression. |
| MOD08, MOD12 batches that contain them | 8, 13, 17–19, 36–39, 45, 56 | Undefined or debug-only reads. Review in the batch; do not force an interface through with casts or blanket initialization. |
| NVEL boundary | 6 (R9DIB), 52, 53 | Preserve the documented direct-test controls and limits. |
| MOD19 | 66 | Settle transaction failure behavior before migrating database resource state. |
| SP19 (repaired 19 November 2026) | 22, 28, 44, 47 | Repaired before Phase 5: 44 and 47 were undefined, layout-dependent reads (44 also wrote out of bounds); 28's biocontrol handlers read an unassigned flag and stopped on a type mismatch; 22 was introduced by the free-form conversion. Their decks are now part of the BC/ON suites. |
| SP04–SP17 (frozen) | 2 (decay code), 17, 21, 23–27, 32, 34, 43, 48 | Deterministic Canadian defects. Keep them through rename-and-dispatch; fix later as separate model changes. 24 and 25 suppress two fire reports, so those paths stay coverage gaps until fixed. |

## Phase 1. Scope, references and dependencies (complete)

- [x] **MOD01 — Confirm the entry conditions and implementation scope.**
  Review the completed optimization work and selected bug fixes. Identify any
  remaining defects that prevent meaningful module verification. Define the
  supported variants, build paths and external interfaces in scope, including
  the treatment of NVEL and other bundled components. Do not require unrelated
  bugs to be fixed or dormant code to be modernized as a prerequisite.
  Acceptance: a concise scope and prerequisite list; no unfinished correctness
  change is mixed into a modernization batch.

  Result, 17 November 2026: optimization completion/defer decisions reviewed
  from Git, followed by the committed focused bug repairs at `d31a9ff0`.
  [MODULE_INVENTORY.md](MODULE_INVENTORY.md) defines all 24 live variants,
  GNU Make/CMake and the executable/library/API boundaries. NVEL/ORGANON
  internals and the foreign C/C++ libraries remain named compatibility
  boundaries; mFVS-side adapters remain in scope. Bugs 6/8/13/17–19/36–39/
  45–47/52–53/56/63/66 and the root-disease restart limit are assigned to
  affected later batches, not made prerequisites for the state-free pilot.
  Native Windows/Intel execution remains unverified. No model repair is
  mixed into this phase.

- [x] **MOD02 — Establish corrected-reference comparisons.**
  Identify exact main and NVEL revisions and any necessary source changes
  defining the corrected reference. Use or minimally extend the existing
  comparison tools to compare candidate output with that reference as well as
  the immediate baseline. Acceptance: repeatable corrected comparisons,
  original-reference results and explicit BC/ON coverage limits. Matching
  early failures must never be described as simulation parity.

  Result, 17 November 2026: fixed main
  `d31a9ff0d0a51e1f0622d0a9b8d431dd310b54c4` and NVEL
  `2503c0de71cb07b7d04c4d2edba38a3ff27237fc` are pinned in
  `tests/parity/prepare_builds.py`. The existing suite accepts
  `--reference corrected` and `--immediate REV`; corrected and immediate
  restarts use exact bytes after run-identifier normalization, with neither
  original-side undefined-value allowance. Both comparisons pass **24/24**;
  original F77 with the existing R04 accommodation remains **22/24**, with
  BC/ON's documented intentional differences. Seven comparator controls
  pass, and a changed year in actual IE output is rejected. The separate
  older-immediate negative control is documented with the comparison guidance.
  Identical source inputs reuse the existing temporary cache; these results
  do not claim three independent builds of identical sources. BC/ON normal
  projections are compared to the corrected source, not the failing original;
  unexercised Canadian restart/option paths still need focused cases when an
  affected batch reaches them. Raw builds/results remain disposable.

- [x] **MOD03 — Inventory live procedure contracts.**
  Map definitions, callers, functions, ENTRY groups, procedure arguments,
  callbacks, exports and existing modules across all current variant
  manifests and auxiliary targets. Acceptance: every in-scope procedure
  belongs to a migration family or a named compatibility boundary; apparent
  duplicate names and incompatible signatures are investigated.

  Result, 17 November 2026: the regenerable source index covers all 1,541
  selected paths and 1,355 Fortran compilation sources: 2,123 scoped units,
  366 ENTRY declarations and 31 statement functions. Every definition has a
  family or explicit library boundary, native selection and a source/scope
  identity. Signatures, known function/direct calls, callbacks, interfaces,
  USEs and ABI directives are indexed. DGCONS's EM argument and the other
  native signature splits remain distinct; arrays/statement functions are
  distinguished from same-named externals. All indexed call targets resolve
  to definitions or named callback/foreign/runtime boundaries. See the
  inventory for API clients and the legacy-wrapper policy. The seven source
  inventory controls pass. This is a conservative source index; each batch
  still owes semantic/alias/branch checks before introducing interfaces.

- [x] **MOD04 — Inventory state, initialization and storage associations.**
  Map every live COMMON block, declaration include, BLOCK DATA initializer,
  SAVE variable, DATA-initialized local, EQUIVALENCE and persistent module
  variable, with readers/writers, variant layouts, reset points, file/database
  handles and restart serialization. Acceptance: proposed owners and
  lifetimes are identified, with unresolved aliasing, initialization and
  external-storage contracts called out explicitly.

  Result, 17 November 2026: 99 named COMMON blocks have 103 source/scope
  layouts, with ordered members, types, native include views and proposed
  owners. DMMIST's fourteen fragments remain one layout. The index maps
  consumers, references, direct assignments, possible call/I/O mutations,
  873 persistent scopes, 38 EQUIVALENCE scopes, 87 BLOCK DATA units and the
  three mutable NVEL module owners. The inventory records initialization/
  reset owners, file/database resources, serializers, seed/workspace overlays
  and exported-storage boundaries. Member-level reset/alias proofs and the
  single-backing-store compatibility method remain explicit MOD17–MOD25 work;
  no default initialization, synchronized copies or restart change is selected.

- [x] **MOD05 — Choose module boundaries and the first pilot.**
  Establish dependency layers and choose one small routine family with few
  callers, adequate coverage and no unresolved interface or state defect.
  Acceptance: an acyclic proposed dependency graph and a bounded pilot; no
  single module imports the entire model merely to avoid choosing boundaries.

  Result, 17 November 2026: the inventory gives an acyclic target layering
  for configuration, state/callback contracts, helpers, foreign adapters,
  equations/extensions, orchestration and external adapters. The first pilot
  is **ISTFNB only**, with one internal SUMHED caller and no shared/persistent
  state. Its future ISTFNB_MOD will have private defaults and only ISTFNB
  public; preserve the external function through an assumed-length character
  wrapper, and import the checked procedure only in SUMHED. The existing
  function passes 87,381 direct string cases and five focused SUMHED title
  cases for every variant. Keep a focused screen-output check because the
  suite does not compare that output.

## Phase 2. Decisions

- [ ] **SP00 — Settle scope, sequencing and interfaces.**
  Six decisions are needed; none of the open ones blocks the next phases.

  - (a) Stage A only, or Stage A followed by Stage B. *Decided 19 November
    2026:* Stage A now. Whether to start Stage B is decided after SP17.
  - (b) Whether BC/ON join the single program or remain separate builds.
    *Decided 19 November 2026:* BC and ON join, and are converted alongside
    the U.S. variants in every phase so that the result is one cohesive
    program. SP19 is moved ahead of SP04 to prepare them.
  - (c) How a run selects its default configuration: program name
    (`argv[0]` = `FVSxx`), `--variant=xx`, an API call, and/or the `Variant`
    column of `FVS_StandInit`. Needed before MOD09.
  - (d) How the existing names `FVSxx` and `FVSxx.so` are kept, using links or
    launchers. For the library, the options are `dladdr` on its own path or a
    required selection call. Needed before MOD09.
  - (e) The module-plan amendments and their ordering. *Resolved* by this
    combined plan (see "How the two strands fit together").
  - (f) Whether the 24 separate builds remain supported once SP15 lands.
    Needed before SP18.

  Acceptance: (c) and (d) recorded here before MOD09, (f) before SP18, and
  the Stage B decision after SP17.

  Result, 19 November 2026: (a), (b) and (e) decided as above. (c), (d) and
  (f) remain open; none blocks Phases 3–5.

## Phase 3. Module builds and the pilot

- [x] **MOD06 — Support the required module dependency ordering.**
  Depends on MOD05. Recheck build improvements already made under the
  optimization plan. Address only the remaining dependency gaps in Make and
  the supported CMake path. Account for actual module names, modules using
  other modules, includes, preprocessing and the existing version module.
  Modules that include a variant's `PRGPRM` must stay in that variant's build
  directory until SP15; the end state is one build directory, so do not add
  per-variant machinery beyond what the existing `FVSxx_buildDir` layout
  provides. Preserve unique source basenames and executable/shared-library
  targets. Acceptance: fresh serial and parallel builds succeed, and edits to
  a module or nested dependency rebuild the necessary consumers without stale
  `.mod` files, cycles or simultaneous compiler writes to the same module
  output.

  Result, 19 November 2026 (Windows 11, Rtools 4.5: GNU Fortran 14.3.0, GNU
  Make 4.4.1, CMake 3.31.8): the Make path needed no dependency change.
  `fvsbuild.c prepare` already orders every consumer after the `.mod` files
  it uses, restores a missing `.mod` from its object, prunes the `.mod` of a
  renamed or removed module and rejects two producers of one module. A
  disposable five-file variant built through the real makefile and helper
  (module A re-exported through B to C, a leaf user of A, a USE inside `#if`,
  and a main program that prints the values) confirmed each case: a fresh
  `-j1` build; a no-op rebuild; an implementation-only edit recompiling only A;
  an interface edit recompiling A, B, the leaf and C but not main, whose
  view was unchanged; a deleted `.mod` restored; a renamed module's stale
  `.mod` removed; a duplicate producer rejected; and `make clean` followed
  by a fresh `-j8` build. The program printed the expected values after every
  step. `check_build_support.py` passes (cache identity, concurrent
  publication, Fortran interface invalidation). A real fresh `-j1` FVSpn
  build succeeded, and the suite's 24 candidate builds are fresh parallel builds.

  One defect was repaired (bug 69, inherited from upstream): exported
  recursive variables re-ran `$(shell cat)` for every recipe, so `-j` builds
  ran one compile at a time on Windows. They are now `:=`, with identical
  values. A variant builds in about 3 minutes at `-j12` instead of about 20.

  CMake: the recipe requires CMake 4.3, which is not installed here, so a
  scratch copy with the minimum lowered to 3.31 was used. Through CMake's own
  generated rules, all 683 Fortran objects of FVSie compiled at `-j12`, with
  ISTFNB_MOD before its consumers. Incremental rebuilds were exact: nothing on
  a no-op, the module and both consumers after an interface edit, and only
  the module after an implementation-only edit. The CMake recipe cannot link
  with MinGW (`libFVSsql.dll` has unresolved references from `apisubsc.c` to
  Fortran symbols in the other library), independently of modules; linking
  remains unverified for Visual Studio/Intel and was not rerun on Linux.

  Test tools now run on Windows: `check_suite.py` passes `OS`, `TMP`, `TEMP`
  and `GFORTRAN_TMPDIR` to make, the inventory uses POSIX relative paths, the
  comparator closes its SQLite connections, and `check_build_support.py`
  tolerates DLLs that ctypes still holds. The seven comparator controls and
  the seven inventory tests pass on Windows.

- [x] **MOD07 — Migrate and verify the procedure pilot.**
  Depends on MOD06. Move ISTFNB into `base/istfnb_mod.f90` (ISTFNB_MOD) and
  update SUMHED to import it, as planned in the inventory. Retain existing
  COMMON includes. Use explicit argument declarations; add `intent` only after
  checking mutations and aliasing. Preserve explicit-shape and legacy
  argument behavior. Acceptance: all callers use the checked definition, a
  disposable incompatible-call example is rejected by the compiler, the
  focused SUMHED screen-output check passes and corrected/immediate
  comparisons show no new differences. Record the small set of conventions
  reused below, including module procedure naming for SP10.

  Result, 19 November 2026: ISTFNB is in ISTFNB_MOD (`base/istfnb_mod.f90`,
  added to all 24 manifests before `base/istfnb.f90`). Its argument is
  `intent(in)`, since the function only reads it; the body is otherwise
  unchanged. `base/istfnb.f90` is now the external wrapper and SUMHED, the
  only internal caller, imports the module function. The full suite passes
  **24/24** against corrected `d31a9ff0` and **24/24** against immediate
  `0cf347e4` (Windows; reference runs built with each commit's own sources).
  `tests/parity/check_istfnb_pilot.py` passes for all 24 variants: SUMHED's
  screen output for seven titles and with screen output off, and ISTFNB on
  all 87,381 strings of length 0–8 against VERIFY, are byte-identical to
  `0cf347e4`, with the CR and metric header branches exercised; the module
  and wrapper agree on every string. A planted mutation (tab treated as
  blank) is detected. An INTEGER argument to the module function is rejected
  by the compiler (`Type mismatch in argument 'string'`), while the same call
  to the legacy external still compiles. The inventory test now asserts this
  shape. The original-F77 comparison was not rerun; the change touches no
  model arithmetic and the corrected reference already carries that
  relationship. Coverage limit: the index lists SUMHED's call against both
  definitions, because its call resolution does not follow USE.

  Conventions for later batches:

  1. A module lives in `<name>_mod.f90` as module `<NAME>_MOD`, with a basename
     unique in the tree. Add it to every manifest that selects a consumer,
     immediately before the file it came from.
  2. Modules use `implicit none`, `private` by default and an explicit
     `public ::` list. Move procedure bodies verbatim, including comments,
     labels and layout; add `intent` only after checking writes and aliasing.
  3. Callers write `use <NAME>_MOD, only: <NAME>` before `implicit none` and
     remove their own type declaration of that name.
  4. A procedure that may have external users keeps its legacy file as a
     wrapper with the same name, result, arguments and implicit-interface ABI,
     importing the module procedure as `<NAME>_CHECKED`. Wrappers stay until
     MOD09 accounts for their consumers.
  5. Each batch adds a compiler control (a wrong call rejected) and, for paths
     the suite does not compare, a focused check against a pinned pre-change
     commit, and updates the inventory test.
  6. For SP10/SP11, each implementation of a colliding family becomes a module
     procedure `<NAME>_<XX>` (variant) or `<NAME>_<FAMILY>` (a file several
     variants share, as in `vbase/htgf_ls.f90`), in module `<NAME>_<XX>_MOD`
     and file `<name>_<xx>_mod.f90` next to its source. ENTRY points are renamed
     the same way. The dispatcher keeps the original external name and
     interface. SP02 confirms or adjusts these names before the first batch.

## Phase 4. Inventories and verification support

- [x] **SP01 — Configuration inventory.**
  Classify every compile-time variant input at the current HEAD as a storage
  dimension, active extent, identity test, initializer (BLOCK DATA or DATA),
  link-time selection (manifest-only files, stubs, alternate implementations)
  or per-variant include (`PRGPRM`, `FMBIOGRP`, `DGCONS_ENTRY`). Include
  `VARIANTCD`/`LMETRICVAR` inside `parameter` expressions, the `DATA` repeat
  counts, the PUTSTD/GETSTD extents and the API arrays. Build this as a report
  from `tests/parity/module_inventory.py`, which already indexes PRGPRM views,
  DATA/SAVE locals and BLOCK DATA units, rather than as a separate tool.
  Acceptance: every site classified, and totals reconciled with the audit
  appendix.

  Result, 19 November 2026: `module_inventory.py --configuration` (summary;
  `--detail` for every site). All 4,729 statements outside `PRGPRM` that name
  a variant input have one kind and a use: 2,059 active extents, 1,374
  identity tests, 978 storage dimensions and 318 initializers. Seven `PRGPRM`
  values differ by variant; `NSPSPE` and `MXFRCDS`, not in the audit, are
  extents like `MAXSP`. The report also lists the three per-variant includes,
  87 BLOCK DATA units under 10 names, the link-time selection groups and the
  restart (MAXSP 210, MAXPLT 12, MAXTRE 4) and API sites. Totals reconcile
  with the appendix once its 24 `PRGPRM` files are added back: `VARIANTCD`
  932 lines in 60 files, `LMETRICVAR` 402 in 86 and `VARACD` 589 in 123
  exactly, `do …,MAXSP` 974 lines in 310 files (audit 972) and DATA using
  MAXSP 316 statements (audit 309 lines). Details are in
  [MODULE_INVENTORY.md](MODULE_INVENTORY.md); the inventory tests check that
  every site has a kind and pin the six files whose parameter expressions
  SP04 must convert.

- [x] **SP02 — Name-collision inventory.**
  For each of the 211 colliding names, list the implementations, consuming
  variants, ENTRY points, argument lists, SAVE/DATA locals and callers.
  Compare signatures across the implementations of a family; a dispatcher
  needs one interface, so record every mismatch. Confirm the six apparent
  same-variant duplicates. Propose implementation names: `NAME_xx`, or
  `NAME_family` for files several variants share, as in `vbase/htgf_ls.f90`.
  Produce it as a collisions report from the existing inventory index, which
  already records each definition's variants, signature and ENTRY group.
  Acceptance: a family table with an ordered batch list (leaf routines first)
  and mismatches recorded.

  Result, 19 November 2026: `module_inventory.py --collisions`. 211 names
  with 746 extra definitions and 957 implementations, as the audit
  estimated; per task SP10 72 names, SP11 22, SP08 84, SP09 30, SP12 3. None
  of the six apparent same-variant duplicates is real: all are explicit
  `interface` bodies. The batch order is leaf-first over the collision call
  graph, which has no cycles. 41 names have signature mismatches (29 argument
  count, 8 rank, 4 type), among them BALMOD, DUBSCR, HTCALC, SMHTGF and ON's
  NATCRS; each is listed in the inventory with the variants on each side.
  Proposed names follow MOD07's convention, except that NVEL already defines
  `FORMCL_BM` and five similar names, so those FVS implementations fall back
  to `FORMCL_FVS_XX`. 622 implementations keep SAVE/DATA locals. The
  inventory tests check that names are unique and new, that no variant
  defines a name twice, and that every dependency sits at a lower level.

- [x] **SP03 — Make the harness ready for a combined program.**
  Make the smallest extension to `check_suite.py` that can run any
  `tests/FVSxx` deck against a program in a chosen default configuration. Add
  a poisoned-storage mode: build with storage dimensions larger than the
  active extents, fill the inactive tail at run time with signalling NaNs and
  integer sentinels, and run with the existing traps. Do not rely on
  `-finit-*` flags (bugs 58 and 63). Acceptance: a planted output change is
  detected, and a planted read of an inactive species slot traps.

  Result, 19 November 2026: `check_suite.py --program TEMPLATE` runs a
  variant's decks against the command the template names (`{variant}`
  substituted), building its first word and pointing the test makefile at
  it; the template enters the run key. Until SP15/SP16 provide a combined
  target the program must be one of the `FVSxx` programs. Planted change:
  IE's decks run on the KT program fail 43 of 43 files against corrected IE.
  `--poison-storage` uses `tests/parity/poison_storage.py`: it raises the
  candidate's `PRGPRM` storage sizes to MAXSP 108, MAXTRE 6000 and MAXPLT 500,
  generates one POISON_INACTIVE routine per variant from the inventory's
  COMMON layouts (signalling NaN, `-HUGE` or `ACHAR(127)` in every tail; 20
  logical and derived-extent arrays are listed as not poisoned for IE) and
  calls it after GRINIT in the three INITRE sources.
  `check_poison_storage.py` passes 5/5 on a planted fixture: storage is
  raised once per size, unpoisonable arrays are reported, active-extent code
  gives the unpoisoned output, a planted real read of an inactive species slot
  stops with a floating-point exception, and a planted integer read changes
  the output. Real variants cannot use the mode until SP05/SP06: IE's
  generated routine compiles against its real includes, but 74 MAXSP-shaped
  DATA statements in 17 files fail, which is SP05's DATA list for IE.
  `--with-new-decks` (added for SP19) runs the working tree's test folders on
  the references too.

- [ ] **SP19 — Canadian readiness** (moved from the end of the one-program
  phase).
  Depends on SP03. BC and ON have three standard decks between them
  (StandStructure, YSM-SkyRanch, Hardwood) and no pristine-original
  simulation parity, so most batches would otherwise verify them weakly.
  First add focused BC/ON decks for the paths later batches change and the
  standard decks miss: stop/restart with byte-identical `.stop` files,
  multi-stand runs, BC mistletoe, the Canadian fire and snag reports, Canadian
  volume (including VOLEQNUM and MCDEFECT), CRNMULT, and a stand near ON's
  6,000-tree limit. Record their outputs from the current corrected source.
  Then make one separate correctness pass for the Canadian defects that would
  make batch comparisons unreliable: 44 (CRNMULT's unset species index and
  out-of-bounds writes), 47 (undefined equation codes after an invalid
  VOLEQNUM), 28 (biocontrol diagnostic type mismatch, wrong activity code and
  unassigned IPRMPT) and 22 (the thirteen remaining split FORMATs the
  free-form conversion broke). Each repair follows the usual bug-fix rules
  and is recorded in FOUND_BUGS. Re-pin the corrected reference once, after
  the whole pass. Acceptance: the new decks are repeatable across builds and
  under poisoned storage, the four repairs are verified individually, the
  U.S. variants are unchanged, and the corrected reference is re-pinned.
  The deterministic Canadian defects listed in the bug table stay frozen.

  Result, 19 November 2026 (all but the re-pin): eight decks were added to
  the BC and ON test makefiles: `BCRestart`, `ONRestart`, `ONFocused` (two
  stands; fire, fuel, snag, down-wood, carbon, burn and mortality reports;
  MCDEFECT; VOLEQNUM), `ONLimit` (5,828 tree records against ON's 6,000),
  `BCVolume`, and one deck per model repair: `BCVoleqnumInvalid` (47),
  `BCBiocontrol` (28) and `ONCrnmult` (44). BC multi-stand runs and BC
  mistletoe were already covered by YSM-SkyRanch. The restart decks found
  that BC and ON restarted projections diverge from the straight run,
  deterministically; this is recorded as bug 70 and left frozen. Bugs 22,
  28, 44 and 47 are repaired and recorded in FOUND_BUGS.
  `check_canadian_repairs.py` verifies each one individually: the 13 FORMATs
  print as their F77 originals (all 13 differed before), and on built
  programs the equation table, the DMBCI echo, DMBCA's application and
  CRNMULT's species all behave as intended; all four deck checks fail on a
  pre-repair build. `check_suite.py --reference corrected --with-new-decks`
  then ran the new decks on the corrected, immediate (`f17ff270`) and
  working-tree builds. The 22 U.S. variants match both references (22/22).
  For BC and ON, every difference is attributable to a repair: the three
  repair decks' outputs, BC's suite status (the bug 28 stop), and bug 22's
  apostrophe in the snag-list and mortality-report headers of StandStructure
  and ONFocused. Every other new deck, restart files included, is
  identical across the three builds. Poisoned-storage repeatability waits for
  SP05/SP06, since real variants cannot build in that mode yet. Two build
  defects found on the way are repaired: bug 69 (serial `-j` builds on
  Windows) and bug 71 (the build helper failing when Windows briefly locks a
  fresh object). **Still to do:** commit this pass, then pin that commit as
  the corrected reference in `tests/parity/prepare_builds.py` and the parity
  guidance.

## Phase 5. Runtime configuration (still 24 programs)

- [ ] **SP04 — Make variant identity a runtime value** (supersedes MOD16).
  Depends on SP19. Add a configuration owner module, set before anything
  reads it: earlier than GRINIT and earlier than `filopn`, which already reads
  `LMETRICVAR`. Initially each program copies its `PRGPRM` constants into it,
  including the metric flag, which becomes a runtime value for BC and ON.
  Convert the `VARIANTCD` and `LMETRICVAR` uses inside `parameter`
  expressions to runtime values; at the time of writing these are in
  `vbase/dgf.f90`, `estb/esaddt.f90`, `estb/esfltr.f90`,
  `fire/base/fmcba.f90`, `fire/base/fmsnft.f90` and `vbase/crown_pn.f90`.
  Reconcile `VARACD` with the new owner without changing anything printed or
  stored. The `METRIC` unit-system selector the inventory assigned to MOD16
  moves to this owner. Acceptance: no constant-expression use of `VARIANTCD`
  remains, and all suites are identical.

  *MOD16* (move configuration and constants into per-variant modules) is
  retired by this task. Its requirement survives: routines that may run
  before GRINIT must read identity from the configuration owner, never from
  runtime `VARACD`.

- [ ] **SP05 — Separate species storage from the active species count.**
  Introduce the active count, defaulting to the variant's native `MAXSP`. Move
  loop bounds, array sections, restart extents and API extents to it, in
  batches by subsystem. Remove the `DATA` repeat counts tied to `MAXSP` (bug 4
  applies to `MISINT`). Acceptance per batch: identical results when storage
  equals the active count, and identical results under poisoned storage with
  `MAXSP` = 108.

- [ ] **SP06 — Do the same for trees and plots.**
  Give `MAXTRE` an active limit, including the `IREC2..MAXTRE` dead-record
  region, compression thresholds and API limits. Treat `MAXPLT` the same way.
  Storage is sized for the largest configuration: 6,000 trees (ON) and 500
  plots. Acceptance: identical results and byte-identical `.stop` files under
  both modes, including the SP19 deck near ON's tree limit and decks near the
  3,000 and 4,000 limits.

- [ ] **SP07 — Convert BLOCK DATA to initialization routines.**
  Turn each per-variant BLOCK DATA into a uniquely named routine that assigns
  the same values, run once per process at the point where BLOCK DATA values
  took effect. Keep their process lifetime: some values are changed during a
  run without being reset per stand. Shared BLOCK DATA (e.g.
  `base/keywds.f90`, `dbsqlite/dbsblkd.f90`) may remain. Acceptance: a COMMON
  memory image taken right after initialization matches the old one byte for
  byte (shadow recipe), and multi-stand decks are identical.

## Phase 6. Checked interfaces and per-variant dispatch (still 24 programs)

Each task here is a family to expand into small named batches from MOD03 and
SP02, ordered by actual dependencies. MOD08 and MOD09 must precede any batch
that encounters their contracts. SP05–SP07 come first so that every
implementation's tables and initializers compile under any configuration.

- [ ] **MOD08 — Handle ENTRY groups and procedure arguments.**
  Depends on MOD07. Review one coupled ENTRY group or callback family at a
  time. Decide whether it can move intact into a module or needs a carefully
  separated implementation. Preserve shared locals, saved state,
  entry-specific arguments and alternate call paths. Define checked interfaces
  for passed procedures and callbacks (RANN, ESRANN, SVRANN; BACHLO's RANGEN).
  ENTRY groups inside an equation or fire family are handled within that
  family's MOD12/SP10 or SP11 batch, and their ENTRY points are renamed with
  it. Acceptance: each migrated entry and callback has a checked contract,
  with focused invocation-order coverage; unresolved legacy defects remain
  separate blocked batches. Eliminating all ENTRY statements is not a
  prerequisite.

- [ ] **MOD09 — Preserve external APIs and library boundaries.**
  Depends on MOD07 and SP00(c)/(d). Identify external consumers of FVS, other
  exported procedures and any data symbols. Where module procedure names would
  change linkage, retain small external adapters with the existing symbols and
  ABI. Use authoritative interfaces for C and external libraries without
  changing their calling conventions incidentally. The variant-selection call
  and the `FVSxx` compatibility names are added later in SP15/SP16; design the
  adapters here so those additions do not require redesigning them.
  Acceptance: existing clients can still call the executable/shared-library
  entry paths; internal adapter calls are checked and exported
  symbol/calling-convention changes are accounted for.

- [ ] **MOD10 — Migrate low-dependency shared helpers.**
  Depends on MOD07 and MOD08–MOD09 where applicable. Start with utilities and
  small computational helpers identified by the inventory. Reuse existing
  modules instead of introducing competing definitions or broad utility
  modules. Acceptance for each batch: all selected callers are updated, local
  state lifetime is unchanged, dependencies remain acyclic and output matches.

- [ ] **MOD11 — Migrate input, reporting and database procedures.**
  Depends on the needed MOD10 batches. Cover keyword readers, initialization
  helpers, reports, SQLite bindings and database input/output in small
  groups. Preserve fixed field widths, units, error handling, transaction
  ownership and argument mutations. Acceptance: real keyword/database paths
  and relevant diagnostic paths exercise the checked calls, with identical
  report content and SQLite schemas and typed rows.

- [ ] **MOD12 + SP10 — Move the variant equation families into modules behind
  per-family dispatchers.**
  Depends on SP02's batch order, SP04–SP07 and the relevant MOD08–MOD10
  batches. Covers the 72 colliding equation-family names (about 113k lines),
  the 21 BC/ON metric twins (about 33k lines) and the rest of the inventory's
  equation family: diameter/height growth, crown, mortality, regeneration and
  volume, including regional and Canadian implementations and their
  NVEL/ORGANON boundary adapters. A metric twin is renamed and dispatched on
  the runtime metric flag like any other implementation (for example
  `NAME_metric`); folding it into runtime metric branches of the imperial
  routine is optional later merging under rule 5. In each batch,
  give each implementation and its ENTRY points a unique module-procedure
  name (`NAME_xx` or `NAME_family`); one dispatcher keeps the original name
  and selects on the runtime configuration. Keep implementation bodies
  unchanged. Preserve native species indices, coefficient extents, precision
  and variant selection. Acceptance per batch: 24/24 identical; no definition
  of the family's name remains outside its dispatcher; an incompatible call
  to the dispatcher is rejected by the compiler; unexercised branches remain
  explicit coverage gaps rather than being inferred safe from another
  variant's results.

- [ ] **SP11 + MOD14 (fire) — Fire per-variant families.**
  Apply the MOD12/SP10 pattern to 13 names in about 46k lines, including the
  Canadian fire code under `canada/fire` and the metric fire twins. `FMCBLK`
  follows SP07, and the three `FMBIOGRP` versions (BC, IE and KT) become
  per-variant data. Acceptance: identical results, including fire, snag and
  carbon outputs and the SP19 Canadian fire decks.

- [ ] **SP08 + MOD14 (stubbed extensions) — Turn extension stubs into
  runtime switches.**
  Link the real implementations everywhere and guard them with configuration
  flags, moving each extension into its modules in the same batch. Reproduce
  each stub's behavior exactly, including messages and keyword rejection. Do
  one extension per batch: climate, cover, mistletoe, root disease, ORGANON,
  economics, fertilization, the `exhtdbh` stub, and others found in SP02. ON
  links the climate, cover, mistletoe and root-disease stubs, and BC uses its
  own mistletoe implementation (`canada/newmist`, `metric/newmist`), which is
  dispatched as an alternate implementation alongside `mistoe`.
  Acceptance: identical results with the extension disabled and enabled;
  extension keyword decks run in variants that previously linked the stub;
  internal calls in the migrated extension are checked.

- [ ] **SP09 + MOD14 (establishment) — Establishment alternates.**
  Bring `estb` and `strp`, plus the AK/CI/EM/IE/KT copies, under the SP08/SP10
  pattern, with module migration in the same batches. Acceptance: identical
  regeneration output in both groups.

- [ ] **MOD14 — Migrate the remaining extension procedure families.**
  Depends on their supporting MOD08–MOD12 batches. Covers shared extension
  code not already handled with SP08, SP09 or SP11, such as shared fire/fuels,
  sprouting and the ORGANON adapters. Split callers and callees to avoid
  cycles with core orchestration. Acceptance: each in-scope family has checked
  internal calls, relevant enabled/disabled paths are exercised and all
  variant selections remain represented in the manifests.

- [ ] **SP12 — Remaining collisions.**
  Cover `RDBLK1`, `VOLS`, `DGCONS_ENTRY` (EM), the inline COMMON blocks, and
  anything else SP02 lists. This includes the Canadian volume code (BC and ON
  use 3 volume files instead of NVEL's 152) and the apparent same-variant
  duplicates CFTOPK (BC, ON) and R9CLARK (ON). ON's thirteen-argument NATCRS
  has a different contract from the U.S. twenty-one-argument NATCRS, so it
  cannot share a dispatcher: give it its own name and keep it reached only
  through the ON adapter added for bug 55. NVEL is linked into the single
  program, but the BC/ON defaults never call it. Acceptance: the SP02 table
  has no open rows.

- [ ] **MOD13 — Migrate core simulation orchestration.**
  Depends on its callee batches above. Work through initialization, cycle
  control, tree updates, treatments and stand completion in dependency order.
  Keep the FVS external adapter stable. Acceptance: full projections,
  multi-stand execution and stop/resume preserve call order, side effects and
  output; public entry paths remain compatible.

- [ ] **MOD15 — Close the procedure-interface inventory.**
  Depends on MOD08–MOD14. Reconcile definitions and callers with MOD03,
  including functions, indirect calls and preprocessor-selected paths.
  Investigate remaining implicit internal calls with compiler diagnostics and
  source inspection. Acceptance: all in-scope internal contracts are checked;
  any external interfaces or deferred defects are individually identified. A
  clean build using permissive argument-mismatch flags alone is insufficient.

- [ ] **SP13 — Performance check.**
  Time a representative set against the O34 baseline before the final link.
  Dispatch and loop bounds are now runtime values; with `-ffp-contract=off`
  and no fast-math, results should not change, but accept only bit-identical
  results. Acceptance: an agreed bound, or the hot dispatch points named for a
  decision.

- [ ] **SP14 — Converge the manifests (checkpoint).**
  Give all 24 variants one common source list plus their own `PRGPRM`, and
  build all 24 programs from it. Acceptance: 24/24 identical, including the
  SP19 Canadian decks. Stopping here is a valid outcome: one source set, 24
  builds, and only the default configuration differs.

## Phase 7. One program

- [ ] **SP15 — Single program with a configuration table.**
  Use one `PRGPRM` holding the storage maxima, and a table of the 24
  defaults: identity, active extents, metric flag, extension flags and
  initialization routines. Implement the selection mechanism chosen in
  SP00(c), including the variant-selection API call, and fail clearly when no
  variant is selected. Acceptance: the program builds, and every default
  selects correctly.

- [ ] **SP16 — Build and compatibility names.**
  Use one build directory for Make and CMake, and provide `FVSxx` and
  `FVSxx.so` as SP00(d) decided, working on Windows as well. `make FVSxx` and
  `make all` keep working. BC's CFIM C++ code is now linked into the one
  program, so the link step needs the C++ runtime for every configuration.
  Acceptance: fresh, incremental and parallel builds succeed, and the test
  makefiles run unchanged.

- [ ] **SP17 — Verify the single program.**
  Run the full suite for each default against original F77, the corrected
  reference and the SP14 per-variant programs, including the SP19 Canadian
  decks. Also run poisoned storage, multi-stand decks, stop/restart (byte-identical `.stop` files), API clients
  (`fvsDimSizes`, species attributes, tree add/cut), timing and memory.
  Acceptance: bit-identical results; coverage limits recorded honestly.

- [ ] **SP18 — Retire the per-variant builds,** if SP00(f) says so.
  Remove the per-variant `PRGPRM` files and source lists, and update the
  README, Contributing.md and parity guidance. Acceptance: no dangling
  references, and the suite is unchanged.

## Phase 8. State ownership in the single source set

MOD17–MOD26 repeat the verified MOD18 method. They are families to expand
into block-by-block tasks, not permission for bulk conversion. Because they
follow SP17, each COMMON block is migrated once, with storage already sized
to the maxima and native counts held as active extents. Refresh the state
inventory as dependencies become clearer, and audit initialization and
serializer changes in every batch. If a family needs another family's state
first, migrate the prerequisite batch without introducing a module dependency
cycle.

- [ ] **MOD17 — Define the storage and restart migration method.**
  Depends on MOD04, SP05 and SP06. Resolve each affected EQUIVALENCE and
  COMMON alias before moving its declarations. Inspect GETSTD/PUTSTD and
  extension serializers, including seed packing, workspace overlays, record
  order and the `IOSUM`/`ROSUM` 22/20-row alias. If legacy and module users
  must coexist, keep one backing store rather than synchronized copies.
  Acceptance: the first state migration has a concrete single-storage design
  and restart checks. Unsupported storage associations are deferred
  explicitly; restart format changes and new serialization frameworks are
  outside scope.

- [ ] **MOD18 — Migrate a small state-block pilot.**
  Depends on MOD17. Select a low-coupling block from MOD04, with understood
  initialization and no unresolved overlay. Move its declarations into one
  owner module and update all consumers and initializers. Preserve types,
  dimensions, visibility, initialization timing and lifetime. Acceptance: no
  duplicate storage/declarations remain for that block, two successive stands
  preserve reset behavior, and relevant restarts and outputs match.

- [ ] **MOD19 — Migrate reporting and database state.**
  Depends on MOD18; bug 66 first. Cover output controls, summaries and
  database state in separate groups. Preserve handle ownership,
  prepared-statement lifetime, output-unit sharing and summary accumulation
  order. Acceptance: report and database content, cleanup/error paths and
  multi-stand resets match; no connection or statement acquires a different
  lifetime incidentally.

- [ ] **MOD20 — Migrate extension state.**
  Depends on MOD18 and needed core/configuration modules. Work separately
  through fire/fuels, regeneration, disease, cover, climate, economics and the
  other live extensions identified in MOD04. Acceptance: each selected layout
  and initializer is represented, enabled/disabled extension state remains
  consistent and extension stop/restart data is unchanged.

- [ ] **MOD21 — Migrate variant-specific state and tables.**
  Depends on MOD18 and the relevant extension batches. Cover native layouts,
  regional COMMON definitions and ORGANON state one family at a time.
  Acceptance: matching variable names do not conceal different types, species
  orders or dimensions; each configuration preserves its own state and
  initialization. Species storage is padded to the maximum by SP05; do not
  pad or unify any other incompatible layout without evidence.

- [ ] **MOD22 — Migrate tree arrays and shared workspaces.**
  Depends on MOD17–MOD18 and the required preceding state batches. Treat
  ARRAYS and workspace blocks as separate high-coupling migrations. Preserve
  the active tree limit from SP06, tree ordering, compression/tripling
  behavior and intentional storage reuse. Acceptance: all readers, writers,
  aliases and serializers use the same storage; boundary tree counts and
  relevant compression/restart paths match. Do not introduce allocatable tree
  storage in this step.

- [ ] **MOD23 — Migrate stand and cycle control state.**
  Depends on the supporting MOD19–MOD22 batches. Split CONTRL, PLOT, operation
  scheduling and remaining core blocks by actual declaration/storage units.
  Preserve GRINIT defaults, keyword overrides, treatment sequencing and
  stand/cycle reset boundaries. Acceptance: successive stands, repeated
  library entry and interrupted/resumed projections preserve control flow and
  results, with no accidental default initialization from the new modules.

- [ ] **MOD24 — Migrate RNG and persistent helper state.**
  Depends on MOD08, MOD17–MOD18 and the relevant model batches. Cover model
  and extension RNGs plus SAVE/DATA locals identified in MOD04. Leave locals
  local where appropriate; module scope must not silently extend their
  lifetime. Preserve seed representation, draw order and entry behavior.
  Acceptance: fixed-seed runs, reseeding, successive stands and stop/resume
  reproduce the corrected reference exactly; no reset or stream is shared
  differently. An undefined-state repair requires a separate bug-fix task.

- [ ] **MOD25 — Reconcile initialization.**
  Depends on MOD19–MOD24 and SP07. SP07 has already replaced the per-variant
  BLOCK DATA units; verify the initialization owner for every migrated object
  and the distinction between process, stand, cycle and call lifetime. Remove
  any remaining obsolete BLOCK DATA unit only after its initialization has a
  proven replacement and its source-list/archive linkage is checked.
  Acceptance: no lost initializer, conflicting initializer or duplicate
  storage; fresh-process, repeated-stand and restart paths all match.

- [ ] **MOD26 — Retire superseded declaration includes and bridges.**
  Depends on MOD25. Reconcile MOD04 with every live manifest and auxiliary
  target. Remove replaced declarations and temporary adapters only when all
  consumers have migrated. Keep executable include fragments and independent
  third-party headers unless a separate task covers them. Acceptance: all
  in-scope internal state has one authoritative module definition; any named
  COMMON required by an external boundary is explicitly documented. No live
  caller depends on a removed include, and no dormant code is deleted merely
  because it is outside this migration.

## Phase 9. Acceptance

- [ ] **MOD27 — Complete the module and shared-data acceptance review.**
  Depends on MOD15, MOD26 and SP17. Reconcile both inventories, run the full
  corrected-reference suite for every default configuration and report the
  separate original-F77 result. Exercise the supported build paths, public
  library calls, relevant extensions, multi-stand execution and stop/restart.
  Refresh focused cases where standard decks do not reach a migrated path.
  Acceptance: checked internal interfaces, authoritative state declarations,
  reliable dependency builds, one program for all 24 variants and no new
  output differences. Record unresolved variant/platform coverage honestly;
  deferred in-scope work prevents claiming full completion. Update the
  current-facing build/contribution guidance and add concise outcomes here.

The principal effort is complete after MOD27. Its acceptance does not require
derived-type conversion, an independent-instance API, concurrent execution or
user-selectable components.

## Conditional follow-on A: configurable components (Stage B)

Stage A is under way; whether to start Stage B is decided after SP17
(SP00(a)). If it goes ahead, it starts only after MOD17–MOD26 have given
ownership of the tables each component uses. Free mixing of every component,
especially the core diameter- and height-growth equations, is not
scientifically neutral; it should be offered as documented research use, not
as an ordinary option.

- [ ] **SP20 — Configuration model and report.**
  Define a configuration as a default variant plus per-component overrides.
  Specify the selection syntax (a keyword or file). Print a report at the
  start of each run listing every component, the variant it comes from, and
  the species mapping. Database schema additions need explicit authorization.
  Acceptance: a design is agreed, and output is unchanged when there is no
  override.

- [ ] **SP21 — Species crosswalk.**
  Map the configuration's species to a donor variant's species through FIA and
  PLANTS codes plus the donor's own surrogate rules. An unmapped species is
  either an error or requires a surrogate the user names; it is never silent.
  Acceptance: crosswalk tables for every donor variant, with tests.

- [ ] **SP22 — Component pilot.**
  Choose a narrow, self-contained component such as bark ratio or crown width.
  Its coefficient tables become owned by the component rather than COMMON,
  callers pass mapped species, and the component is exposed as an override.
  Acceptance: default runs are unchanged, and override values equal the donor
  variant's equation for the mapped species (direct check).

- [ ] **SP23 — Extend component by component,** in order of interface
  narrowness: height–diameter, crown, mortality method, establishment model,
  fire and snag tables. Volume is already selectable through keywords and
  NVEL. Where a component needs donor-variant location, habitat or site codes,
  it must map them. Substituting diameter or height growth comes last, is
  marked as research use, and relies on FVS's existing self-calibration.
  Acceptance per component: as SP22, plus documented provenance.

- [ ] **SP24 — User documentation.**
  Generate per-variant tables of default configurations from the
  configuration table, showing which equations each variant uses and which
  variant they come from. Add guidance on when an override is scientifically
  defensible. Acceptance: the tables match the code. Changing any default is a
  separate, documented model change.

## Conditional follow-on B: explicit per-stand simulation state

Start MOD28–MOD34 only if independent instances, embedding or another concrete
use justifies their cost. These tasks need a scope decision after MOD27; their
presence here does not start them. Threading remains separate work.

- [ ] **MOD28 — Assess the need and remaining shared-state barriers.**
  Depends on MOD27. Identify actual callers and the desired independent-
  instance behavior. Audit module globals, saved locals, RNGs, NVEL/ORGANON
  state, extension state, files, SQLite handles and process-wide runtime
  settings. Acceptance: a proceed/defer decision, a bounded instance contract
  and a complete list of barriers. Modules alone must not be described as
  reentrant.

- [ ] **MOD29 — Design explicit state types and their lifecycle.**
  Depends on a proceed decision in MOD28. Separate immutable configuration
  data, persistent stand state, extension state, workspaces and external
  resources. Define initialization, reset, completion, error cleanup and
  restart behavior. Preserve current array organization, precision and fixed
  dimensions initially. Acceptance: an ownership/dependency design and a
  compatibility path for the existing API, without an unrelated
  object-oriented or dynamic-memory rewrite.

- [ ] **MOD30 — Pilot passing a state object through one subsystem.**
  Depends on MOD29. Move one well-covered state group into a derived type and
  pass it explicitly through its procedures. Retain a default instance behind
  the legacy external API if needed. Acceptance: no hidden second copy of the
  pilot state, two independent pilot instances retain separate values, and
  single-instance results and existing external calls remain unchanged.

- [ ] **MOD31 — Migrate the remaining mutable model state.**
  Depends on MOD30. Expand by named core/extension/variant families, including
  RNG streams and persistent helper locals. Separate instance-owned storage
  from genuinely immutable shared tables. Acceptance per batch: every mutable
  object has an owner, dependencies are explicit and corrected comparisons
  pass. Any external dependency that still shares mutable state is an
  explicit barrier to independent full-model instances.

- [ ] **MOD32 — Assign resources and serialization to their owners.**
  Depends on the relevant MOD31 batches. Make file/database handles, output
  destinations and cleanup paths instance-specific where required. Adapt
  restart code to the explicit state while preserving existing file content
  and the legacy API. Acceptance: closing or failing one instance leaves
  another usable; resource cleanup and restart/resume preserve results.

- [ ] **MOD33 — Verify independent instances by interleaving execution.**
  Depends on MOD31–MOD32. Run two different stands in isolated processes and
  then interleave their supported execution boundaries in one process. Cover
  different inputs, seeds, extensions, resets, errors and stop/resume. Running
  several variants at once in one process is covered only if it is part of
  the selected API scope. Acceptance: each instance matches its isolated
  result and state, with no cross-instance changes. This demonstrates the
  tested sequential instance isolation; it does not establish thread safety
  or parallel speedup.

- [ ] **MOD34 — Close the instance-state work and document its limits.**
  Depends on MOD33. Repeat the full corrected comparison, legacy-client and
  supported-build checks. Document lifecycle, instance ownership, API usage
  and remaining process-wide limitations. Acceptance: the selected
  independent-instance contract is met, compatibility is verified and any
  later concurrency or API expansion has its own separately defined work.

## Work outside this plan

- Changes to scientific equations, coefficients, units, defaults, precision,
  species lists or mappings, RNG algorithms or numerical tolerances.
- Completing bug fixes inside a restructuring patch.
- Merging implementations beyond what a rename-and-dispatch batch needs.
- Threading model routines, adding cross-stand caches or promising speedups.
- Replacing the build system, introducing a new language/API platform or
  redesigning reports, database schemas or restart formats.
- Reducing MAXTRE, changing tree compression/tripling, or dynamic tree storage
  beyond separating storage size from active extent.
- Bulk variable renaming, unrelated formatting, historical-document cleanup or
  removal of dormant research code.

## Appendix: single-program audit, 17 November 2026

This read-only static audit of `d31a9ff0` made no builds, simulation runs or
timings. Its counts come from regex parsing of the 24 source lists; they are
approximate and are refreshed by SP01/SP02 before any edit.

### What already favors one program

Shared state layouts are already unified. There are 88 COMMON blocks in
include files, each declared in exactly one file. Only three includes still
differ by variant: `PRGPRM` (24 versions), `FMBIOGRP` (BC, IE and KT) and
`DGCONS_ENTRY` (EM). The 11 COMMON blocks written directly in source files
each appear in only one set of files. Conflicting COMMON layouts would
normally be the hardest part of combining programs, and the consolidation has
largely done that work. A dispatch pattern already exists (see "Current
starting points"). Variants are also already compositions of borrowed
equations: hundreds of source comments say so, for example "USES IE LM
(ORIGINALLY FROM TT VARIANT)" and "SURROGATE EQUATIONS FROM THE CR VARIANT".
Treating variants as configurations matches how they were built.

### Size

| | Files | Compiled Fortran lines |
|---|---|---|
| One variant | 675–826 list entries | 183k (BC) – 246k (SO) |
| All 24 builds today (sum) | — | about 5.51 million |
| Union of all 24 lists | 1,355 compiled Fortran (+96 includes, 90 C/C++/headers) | 451,846 |

A single program compiles about 1.9 times the largest variant and about 8% of
what an all-variant build compiles today.

### Compile-time variant identity and sizes

| Item | Extent |
|---|---|
| `PRGPRM` values | `MAXSP` 11 (KT) – 108 (NE); `MAXTRE` 3000, BC 4000, ON 6000; `MAXPLT` 500, BC/ON/TT 200; `MAXCYC` 40 everywhere; `LMETRICVAR` true only for BC/ON |
| `VARIANTCD` (constant) | 932 lines in 60 files; about 11 inside `parameter` constant expressions |
| `VARACD` (runtime, set in GRINIT) | 589 lines in 123 files; coexists with `VARIANTCD` |
| `LMETRICVAR` | 402 lines in 86 files; 5 in `parameter` expressions |
| `MAXSP` as loop bound or extent | 972 `do …=1,MAXSP` loops in 310 files, about 1,900 other references |
| `MAXTRE` | 97 loops and about 410 other references in 176 files. Dead/cut tree records occupy `IREC2..MAXTRE`, and API limits use it, so it governs behavior as well as storage |
| `DATA` statements using `MAXSP` | 309, some with literal repeat counts tied to it (e.g. `ws/blkdat.f90` `688*0.0` = 16×43) |
| BLOCK DATA units | 87, about 75 of them per variant (`blkdat` ×24, `cubrds` ×20, `fmcblk` ×20, `rdblk1` ×15). A named COMMON may be initialized by only one BLOCK DATA in a program |
| Restart files | `pg/putstd.f90`/`getstd.f90` write explicit `MAXSP`/`MAXPLT`/`MAXTRE` extents and use EQUIVALENCE packing, so the file format depends on these sizes |
| API | `fvsDimSizes` returns `MAXTRE`/`MAXSP`; `fvsSpeciesAttrC` has a C-bound `dimension(MAXSP)` array; there is no variant-selection call |
| Entry names | Tests and clients run `bin/FVSxx` and load `FVSxx.so` (e.g. `tests/FVSie/makefile`) |

Sizing everything to the maximum is therefore not enough. Storage dimensions
and active extents must become separate quantities.

### Routine-name collisions in the union

211 external routine names have more than one definition across the 24 lists;
each is a link conflict in one program.

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
ENTRY scopes the parser missed; SP02 confirms them.

Extensions are chosen at link time. CS, LS, NE, SN and ON link the climate,
cover, mistletoe and root-disease stubs. ORGANON is linked only in OC/OP. Root
disease is real in 15 western variants and BC. Full establishment (`estb/`) is
used by AK, CI, EM, IE and KT, and partial (`strp/`) by the rest. BC and ON use
no NVEL (3 volume files against 152) and have their own metric, fire and
mistletoe code.

### Costs and risks

The runtime-extent work is broad, about 3,000 sites, and a loop left on the
storage dimension can read inactive storage; SP03's poisoned-storage mode is
the safeguard. Dispatch and loop bounds become runtime values, so SP13 times
the result. Undefined reads become visible after layout changes (bugs 53 and
63). BLOCK DATA values live for the whole process, and some are changed during
a run without being reset per stand, so their replacements must keep that
lifetime. BC/ON carry most of the remaining divergence and have the weakest
references, which is why SP19 adds focused decks before Phase 5. Stage B is
science rather than refactoring, for the reasons given
under "Objectives".
