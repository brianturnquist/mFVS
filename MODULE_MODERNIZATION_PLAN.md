# mFVS module and simulation-state modernization plan

Created 16 November 2026. All tasks are pending. Creating this plan does not
start implementation or change the existing optimization and bug-fix work.

The proposed sequence is: finish or explicitly defer the optimization tasks,
complete the separately selected bug fixes, establish a corrected reference,
then begin this modernization. Recheck the code and these tasks at that point;
this document describes the intended work, not a completed dependency audit.

## Objective and scope

Make procedure calls compiler-checked and give shared simulation data clear
definitions, ownership and initialization rules without changing model results.
The principal work is moving procedures into modules and replacing internal
COMMON declarations and declaration includes with module-owned data.

Independent per-stand state is a conditional follow-on phase. Module variables
still provide shared global state; moving COMMON data into modules does not by
itself allow independent simulations or safe concurrent execution.

Expected benefits are easier debugging, fewer argument-contract mistakes and
clearer data flow. This plan makes no execution-time or memory-saving claim.

Read [Contributing.md](Contributing.md), [FOUND_BUGS.md](FOUND_BUGS.md),
[the parity guidance](tests/parity/README.md) and
[OPTIMIZATION_TASKS.md](OPTIMIZATION_TASKS.md) before starting work. This plan
refines the module step in [README.md](README.md); it does not change the
scientific or product contract.

## Current starting points

These examples were inspected when preparing the plan; confirm their current
implementations and consumers before using them:

- `vbase/initdesign_mod.f90`, `estb/essize_mod.f90` and other existing modules
  provide examples of module procedures. They still include COMMON data, so
  checked calls and state migration can be separate changes.
- `base/fvs.f90` uses core includes such as `ARRAYS`, `CONTRL` and `PLOT` and
  exposes an external entry point. Its internal restructuring must preserve
  the existing library interface.
- `pg/putstd.f90` and `pg/getstd.f90` connect simulation state to restart files.
  PUTSTD also uses EQUIVALENCE for packing and storage reuse. Moving the
  underlying declarations requires an explicit compatibility design.
- `bin/FVSxx_sourceList.txt` selects each variant's actual sources. Make copies
  them into a flat per-variant directory, recognizes `_mod.f90` files and has
  broad module/include prerequisites. General module-to-module dependencies
  need deliberate ordering before expanding the module structure.
- The existing parity suite compares simulation output with pinned original
  F77. Its `--baseline` option controls discovery of new source files; it does
  **not** select a corrected simulation reference.

No builds, simulation runs or performance measurements were performed to
prepare this plan. Current counts, bug status and Canadian coverage must be
refreshed before implementation.

## Rules for each implementation task

1. Work on one bounded change at a time. Expand a family task below into named
   routine or COMMON-block batches before editing; do not migrate an entire
   subsystem in one unchecked patch. Complete verification before the next
   batch. If a migration fails, correct or undo only that migration.
2. Preserve equations, coefficients, species mappings, units, defaults, kinds,
   dimensions, expression and accumulation order, random draws, argument
   mutations, diagnostics, output formats, database content and restart data.
   Preserve the current variant selection and all selected implementations.
3. Keep model corrections separate. If a checked interface exposes an invalid
   legacy call or undefined state, record it in `FOUND_BUGS.md` and defer that
   migration until a separate correctness change defines the intended behavior.
   Do not conceal a mismatch with casts, permissive interfaces or compiler flags.
4. Preserve external symbols, calling conventions and executable/shared-library
   behavior. Inspect external callers, callbacks and exported COMMON symbols
   before moving or renaming their definitions.
5. Preserve the existing commentary and formatting. Use the repository's
   free-form style, `implicit none`, two-space indentation and banner comments.
   Explain dependencies, initialization, aliases and compatibility adapters
   where those details will help future debugging. Avoid unrelated reformatting.
6. Compare each candidate with its immediate pre-change version and the fixed
   corrected reference. Continue the original-F77 comparison separately, with
   previously documented bug-fix differences distinguished from new differences.
   Do not weaken comparison masks or introduce numerical tolerances to accept
   restructuring.
7. Exercise affected variants and all relevant unit systems and entry paths.
   Run the full suite for shared changes. Supplement existing decks only where
   they miss a behavior the migration could alter, including initialization,
   multiple stands, callbacks, error paths and stop/restart as applicable.
8. Check source-list membership and clean/incremental builds when module
   dependencies change. Run `git diff --check`. Keep builds and raw outputs in
   disposable temporary directories outside the repository. Retain concise task
   outcomes and bug records, not logs, archives or separate completion reports.

Current original-F77 command, subject to the guidance above:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

Use a distinct temporary cache/build location for each reference and compiler
configuration when existing cache identities do not already distinguish them.
A cached run does not demonstrate that a new module dependency or flag was used.

## Phase 1. Establish scope, references and dependencies

- [ ] **MOD01 — Confirm the entry conditions and implementation scope.**
  Review the completed optimization work and selected bug fixes. Identify any
  remaining defects that prevent meaningful module verification. Define the
  supported variants, build paths and external interfaces in scope, including
  the treatment of NVEL and other bundled components. Do not require unrelated
  bugs to be fixed or dormant code to be modernized as a prerequisite.
  Acceptance: a concise scope and prerequisite list; no unfinished correctness
  change is mixed into a modernization batch.

- [ ] **MOD02 — Establish corrected-reference comparisons.**
  Depends on MOD01. Identify exact main and NVEL revisions and any necessary
  source changes defining the corrected reference. If the reference includes
  uncommitted work, make the exact source snapshot reproducible without
  committing unrelated changes or retaining a raw-result archive. Run its
  existing tests and document intentional differences from original F77.
  Use or minimally extend the existing comparison tools to compare candidate
  output with that reference as well as the immediate baseline. Keep this
  infrastructure change separate from module migrations; confirm a controlled
  output mutation is detected. Acceptance: repeatable corrected comparisons,
  original-reference results and explicit BC/ON coverage limits. Matching
  early failures must never be described as simulation parity.

- [ ] **MOD03 — Inventory live procedure contracts.**
  Depends on MOD01. Use all current variant manifests and auxiliary targets to
  map definitions, callers, functions, ENTRY groups, procedure arguments,
  callbacks, exports and existing modules. Include conditional compilation,
  metric paths and variant-specific selections. Distinguish internal calls
  from calls across external-library boundaries. Acceptance: every in-scope
  procedure belongs to a migration family or a named compatibility boundary;
  apparent duplicate names and incompatible signatures are investigated.

- [ ] **MOD04 — Inventory state, initialization and storage associations.**
  Depends on MOD01. Map every live COMMON block, declaration include, BLOCK
  DATA initializer, SAVE variable, DATA-initialized local, EQUIVALENCE and
  persistent module variable. Identify readers/writers, variant layouts,
  stand/cycle reset points, file/database handles and restart serialization.
  Include RNGs, extensions and NVEL where they participate in simulation state.
  Acceptance: proposed owners and lifetimes are identified, with unresolved
  aliasing, initialization and external-storage contracts called out explicitly.

- [ ] **MOD05 — Choose module boundaries and the first pilot.**
  Depends on MOD02–MOD04. Establish dependency layers for configuration,
  state, numerical procedures, model orchestration and external adapters.
  Choose one small routine family with few callers, adequate coverage and no
  unresolved interface or state defect. Plan `use ..., only: ...`, private
  implementation details and deliberate public procedures. Keep variable
  names and scientific expressions unchanged. Acceptance: an acyclic proposed
  dependency graph and a bounded pilot; no single module imports the entire
  model merely to avoid choosing boundaries.

## Phase 2. Make module builds reliable and prove the pattern

- [ ] **MOD06 — Support the required module dependency ordering.**
  Depends on MOD05. Recheck build improvements already made under the
  optimization plan. Address only the remaining dependency gaps in Make and
  the supported CMake path. Account for actual module names, modules using
  other modules, includes, preprocessing and the existing version module.
  Keep module files isolated by variant and incompatible compiler settings;
  preserve unique source basenames and executable/shared-library targets.
  Acceptance: fresh serial and parallel builds succeed, and edits to a module
  or nested dependency rebuild the necessary consumers without stale `.mod`
  files, cycles or simultaneous compiler writes to the same module output.

- [ ] **MOD07 — Migrate and verify the procedure pilot.**
  Depends on MOD06. Move the selected family into a module and update every
  internal caller to import its procedure. Retain existing COMMON includes
  during this step. Use explicit argument declarations; add `intent` only
  after checking mutations and aliasing. Preserve explicit-shape and legacy
  argument behavior unless a separate change is required. Acceptance: all
  callers use the checked definition, a disposable incompatible-call example
  is rejected by the compiler, and corrected/immediate comparisons show no
  new differences. Record the small set of conventions to reuse below.

## Phase 3. Extend checked procedure interfaces

Each family task must be split into small named batches from MOD03. Family
ordering can follow actual dependencies. MOD08 and MOD09 must precede any
batch that encounters their respective contracts.

- [ ] **MOD08 — Handle ENTRY groups and procedure arguments.**
  Depends on MOD07. Review one coupled ENTRY group or callback family at a
  time. Decide whether it can move intact into a module or needs a carefully
  separated implementation. Preserve shared locals, saved state, entry-specific
  arguments and alternate call paths. Define checked interfaces for passed
  procedures and callbacks where required. Acceptance: each migrated entry
  and callback has a checked contract, with focused invocation-order coverage;
  unresolved legacy defects remain separate blocked batches. Eliminating all
  ENTRY statements is not a prerequisite for module-based interfaces.

- [ ] **MOD09 — Preserve external APIs and library boundaries.**
  Depends on MOD07. Identify external consumers of FVS, other exported
  procedures and any data symbols. Where module procedure names would change
  linkage, retain small external adapters with the existing symbols and ABI.
  Use authoritative interfaces for C and external libraries without changing
  their calling conventions incidentally. Acceptance: existing clients can
  still call the executable/shared-library entry paths; internal adapter calls
  are checked and exported symbol/calling-convention changes are accounted for.

- [ ] **MOD10 — Migrate low-dependency shared helpers.**
  Depends on MOD07 and MOD08–MOD09 where applicable. Start with utilities and
  small computational helpers identified by the inventory. Reuse existing
  modules instead of introducing competing definitions or broad utility
  modules. Acceptance for each batch: all selected callers are updated, local
  state lifetime is unchanged, dependencies remain acyclic and output matches.

- [ ] **MOD11 — Migrate input, reporting and database procedures.**
  Depends on the needed MOD10 batches. Cover keyword readers, initialization
  helpers, reports, SQLite bindings and database input/output in small groups.
  Preserve fixed field widths, units, error handling, transaction ownership
  and argument mutations. Acceptance: real keyword/database paths and relevant
  diagnostic paths exercise the checked calls, with identical report content
  and SQLite schemas and typed rows.

- [ ] **MOD12 — Migrate variant equations and their dispatch.**
  Depends on the relevant MOD08–MOD10 batches. Cover diameter/height growth,
  crown, mortality, regeneration and volume families, including regional
  implementations and their selected NVEL/ORGANON boundaries. Preserve native
  species indices, coefficient extents, precision and variant selection.
  Acceptance: every consuming variant builds and exercised equation paths
  match the corrected reference; unexercised branches remain explicit coverage
  gaps rather than being inferred safe from another variant's results.

- [ ] **MOD13 — Migrate core simulation orchestration.**
  Depends on its callee batches in MOD10–MOD12 and relevant MOD14 batches.
  Work through initialization, cycle control, tree updates, treatments and
  stand completion in dependency order. Keep the FVS external adapter stable.
  Acceptance: full projections, multi-stand execution and stop/resume preserve
  call order, side effects and output; public entry paths remain compatible.

- [ ] **MOD14 — Migrate extension procedure families.**
  Depends on their supporting MOD08–MOD12 batches. Make separate batches for
  fire/fuels, establishment, mistletoe, root disease, cover, climate, economics,
  ORGANON and other live extensions found in MOD03. Cover both optional active
  paths and linked stubs. Split callers and callees to avoid cycles with core
  orchestration. Acceptance: each in-scope family has checked internal calls,
  relevant enabled/disabled paths are exercised and all variant selections
  remain represented in the manifests.

- [ ] **MOD15 — Close the procedure-interface inventory.**
  Depends on MOD08–MOD14. Reconcile definitions and callers with MOD03,
  including functions, indirect calls and preprocessor-selected paths.
  Investigate remaining implicit internal calls with compiler diagnostics and
  source inspection. Acceptance: all in-scope internal contracts are checked;
  any external interfaces or deferred defects are individually identified.
  A clean build using permissive argument-mismatch flags alone is insufficient.

## Phase 4. Prepare state ownership and restart compatibility

- [ ] **MOD16 — Move configuration and constants into modules.**
  Depends on MOD15. Migrate declaration-only configuration in small groups,
  beginning with the selected PRGPRM definitions and immutable parameters.
  Preserve each variant's exact values, types, bounds and early availability
  of `VARIANTCD`/`LMETRICVAR`; do not substitute runtime `VARACD` before GRINIT.
  Acceptance: every consuming variant receives its own correct configuration,
  and no object/module reuse crosses incompatible parameter definitions.

- [ ] **MOD17 — Define the storage and restart migration method.**
  Depends on MOD04 and MOD16. Resolve each affected EQUIVALENCE and COMMON
  alias before moving its declarations. Inspect GETSTD/PUTSTD and extension
  serializers, including seed packing, workspace overlays and record order.
  If legacy and module users must coexist, keep one backing store rather
  than synchronized copies. Use compatibility boundaries only where needed.
  Acceptance: the first state migration has a concrete single-storage design
  and restart checks. Unsupported storage associations are deferred explicitly;
  restart format changes and new serialization frameworks are outside scope.

- [ ] **MOD18 — Migrate a small state-block pilot.**
  Depends on MOD17. Select a low-coupling block from MOD04, with understood
  initialization and no unresolved overlay. Move its declarations into one
  owner module and update all consumers and initializers. Preserve types,
  dimensions, visibility, initialization timing and lifetime. Acceptance:
  no duplicate storage/declarations remain for that block, two successive
  stands preserve reset behavior, and relevant restarts and outputs match.

## Phase 5. Replace shared-data declarations in bounded groups

MOD19–MOD24 repeat the verified MOD18 method. They are families to expand into
block-by-block tasks, not permission for bulk conversion. Refresh the state
inventory as dependencies become clearer. Audit initialization and serializer
changes in every batch; MOD25 is the final reconciliation, not a reason to
postpone those checks. If a family needs another family's state first, migrate
the prerequisite batch without introducing a module dependency cycle.

- [ ] **MOD19 — Migrate reporting and database state.**
  Depends on MOD18. Cover output controls, summaries and database state in
  separate groups. Preserve handle ownership, prepared-statement lifetime,
  output-unit sharing and summary accumulation order. Acceptance: report and
  database content, cleanup/error paths and multi-stand resets match; no
  connection or statement acquires a different lifetime incidentally.

- [ ] **MOD20 — Migrate extension state.**
  Depends on MOD18 and needed core/configuration modules. Work separately
  through fire/fuels, regeneration, disease, cover, climate, economics and
  the other live extensions identified in MOD04. Acceptance: each selected
  layout and initializer is represented, enabled/disabled extension state
  remains consistent and extension stop/restart data is unchanged.

- [ ] **MOD21 — Migrate variant-specific state and tables.**
  Depends on MOD18 and the relevant extension batches. Cover native layouts,
  regional COMMON definitions and ORGANON state one family at a time.
  Acceptance: matching variable names do not conceal different types, species
  orders or dimensions; each selected variant preserves its own state and
  initialization. Do not pad or unify incompatible layouts without evidence.

- [ ] **MOD22 — Migrate tree arrays and shared workspaces.**
  Depends on MOD17–MOD18 and the required preceding state batches. Treat
  ARRAYS and workspace blocks as separate high-coupling migrations. Preserve
  MAXTRE, tree ordering, compression/tripling behavior and intentional storage
  reuse. Acceptance: all readers, writers, aliases and serializers use the
  same storage; boundary tree counts and relevant compression/restart paths
  match. Do not introduce allocatable tree storage in this step.

- [ ] **MOD23 — Migrate stand and cycle control state.**
  Depends on the supporting MOD19–MOD22 batches. Split CONTRL, PLOT, operation
  scheduling and remaining core blocks by actual declaration/storage units.
  Preserve GRINIT defaults, keyword overrides, treatment sequencing and
  stand/cycle reset boundaries. Acceptance: successive stands, repeated
  library entry and interrupted/resumed projections preserve control flow
  and results, with no accidental default initialization from the new modules.

- [ ] **MOD24 — Migrate RNG and persistent helper state.**
  Depends on MOD08, MOD17–MOD18 and the relevant model batches. Cover model
  and extension RNGs plus SAVE/DATA locals identified in MOD04. Leave locals
  local where appropriate; module scope must not silently extend their
  lifetime. Preserve seed representation, draw order and entry behavior.
  Acceptance: fixed-seed runs, reseeding, successive stands and stop/resume
  reproduce the corrected reference exactly; no reset or stream is shared
  differently. An undefined-state repair requires a separate bug-fix task.

- [ ] **MOD25 — Reconcile initialization and BLOCK DATA.**
  Depends on MOD19–MOD24. Verify the initialization owner for every migrated
  object and the distinction between process, stand, cycle and call lifetime.
  Remove an obsolete BLOCK DATA unit only after all its initialization has
  a proven replacement and its source-list/archive linkage is checked.
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

## Phase 6. Verify and close the principal modernization

- [ ] **MOD27 — Complete the module and shared-data acceptance review.**
  Depends on MOD15 and MOD26. Reconcile both inventories, run the full
  corrected-reference suite and report the separate original-F77 result.
  Exercise the supported build paths, public library calls, relevant extensions,
  multi-stand execution and stop/restart. Refresh focused cases where standard
  decks do not reach a migrated path. Acceptance: checked internal interfaces,
  authoritative state declarations, reliable dependency builds and no new
  output differences. Record unresolved variant/platform coverage honestly;
  deferred in-scope work prevents claiming full completion. Update the
  current-facing build/contribution guidance and add concise outcomes here.

The principal effort is complete after MOD27. Its acceptance does not require
derived-type conversion, an independent-instance API or concurrent execution.

## Conditional follow-on: explicit per-stand simulation state

Start MOD28–MOD34 only if independent instances, embedding or another concrete
use justifies their cost. These tasks need a scope decision after MOD27; their
presence here does not start them. Threading remains separate work.

- [ ] **MOD28 — Assess the need and remaining shared-state barriers.**
  Depends on MOD27. Identify actual callers and the desired independent-instance
  behavior. Audit module globals, saved locals, RNGs, NVEL/ORGANON state,
  extension state, files, SQLite handles and process-wide runtime settings.
  Acceptance: a proceed/defer decision, a bounded instance contract and a
  complete list of barriers. Modules alone must not be described as reentrant.

- [ ] **MOD29 — Design explicit state types and their lifecycle.**
  Depends on a proceed decision in MOD28. Separate immutable variant data,
  persistent stand state, extension state, workspaces and external resources.
  Define initialization, reset, completion, error cleanup and restart behavior.
  Preserve current array organization, precision and fixed dimensions initially.
  Acceptance: an ownership/dependency design and a compatibility path for the
  existing API, without an unrelated object-oriented or dynamic-memory rewrite.

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
  pass. Any external dependency that still shares mutable state is an explicit
  barrier to independent full-model instances.

- [ ] **MOD32 — Assign resources and serialization to their owners.**
  Depends on the relevant MOD31 batches. Make file/database handles, output
  destinations and cleanup paths instance-specific where required. Adapt
  restart code to the explicit state while preserving existing file content
  and the legacy API. Acceptance: closing or failing one instance leaves
  another usable; resource cleanup and restart/resume preserve results.

- [ ] **MOD33 — Verify independent instances by interleaving execution.**
  Depends on MOD31–MOD32. Run two different stands in isolated processes and
  then interleave their supported execution boundaries in one process. Cover
  different inputs, seeds, extensions, resets, errors and stop/resume. Test
  multiple variants together only if that is part of the selected API scope.
  Acceptance: each instance matches its isolated result and state, with no
  cross-instance changes. This demonstrates the tested sequential instance
  isolation; it does not establish thread safety or parallel speedup.

- [ ] **MOD34 — Close the instance-state work and document its limits.**
  Depends on MOD33. Repeat the full corrected comparison, legacy-client and
  supported-build checks. Document lifecycle, instance ownership, API usage
  and remaining process-wide limitations. Acceptance: the selected independent-
  instance contract is met, compatibility is verified and any later concurrency
  or API expansion has its own separately defined work.

## Work outside this plan

- Changes to scientific equations, coefficients, units, defaults, precision,
  species mappings, RNG algorithms or numerical tolerances.
- Completing additional bug fixes inside a restructuring patch.
- Threading model routines, adding cross-stand caches or promising speedups.
- Replacing the build system, introducing a new language/API platform or
  redesigning reports, database schemas or restart formats.
- Reducing MAXTRE, changing tree compression/tripling or converting all arrays
  to dynamic allocation as part of state ownership changes.
- Bulk variable renaming, unrelated formatting, historical-document cleanup
  or removal of dormant research code.

For each completed task or bounded subtask, add a short dated result beneath
its checklist entry: affected families/variants, comparison outcome, relevant
build/API checks, coverage limits and any linked bug. Leave unverified work
unchecked. Do not retain raw verification outputs in the repository.
