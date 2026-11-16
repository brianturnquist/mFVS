# mFVS optimization and efficiency tasks

Created 16 November 2026 from the material in [task_documents](task_documents/).
This is a new checklist; the deleted working task list is not a source for it.
O01 and O02 were completed on 16 November 2026; the remaining tasks are pending.
Creating this document does not start implementation.

The objective is to reduce execution time, build time, memory use or storage
without changing model results. Work through one bounded change at a time,
verify it, then decide whether the next candidate still offers enough benefit.

## Source material and evidence limits

The source is [mFVS_optim.txt](task_documents/mFVS_optim.txt), a transcript of a
read-only review on another machine. Its final recommendations are at lines
5483–5527; the earlier commands and outputs provide supporting detail.

| Supplied material | What is available here |
|---|---|
| `mFVS_optim.txt` | Review, sampled profiles, source excerpts, timings and a conversion microbenchmark |
| `bench-20261116T133656Z-1-001.zip` | Only an empty `bench/` directory; no benchmark files |
| `prof-20261116T133708Z-1-001.zip` | Only an empty `prof/` directory; no profiler or sample files |

The transcript reports 47–81% of sampled CPU time under formatted I/O across
selected Windows decks. It identifies repeated integer/string conversions in
STKVAL and NVEL as major contributors. Those percentages include different
workloads and are not measured speedups. The suggested combined 2–5× Windows
speedup is an estimate, not an acceptance target. No equivalent speedup has been
measured on this Mac or Linux.

The reported 24–42 MB peak working sets, binary sizes, build-directory sizes,
source counts and 177-test count also belong to that other-machine review.
Refresh the relevant measurements before using them to justify a change.

Current-tree checks while preparing this list used mFVS `3b937427` and NVEL
`a73e697e220ffd0f792aa7ac10d84c911edb7ad7`. They confirmed the repeated STKVAL
reads, the NVEL parsing paths, the R8PREPCOEF local-state concern, the paired
CWD volume calls, and the Makefile's broad include dependencies. They did not
include builds, benchmarks or simulation runs.

Two transcript assumptions need particular care:

- BC/ON have since received interface and keyword-buffer repairs. Follow the
  current [parity guidance](tests/parity/README.md) and
  [bug 5](FOUND_BUGS.md#5-canadian-keyword-readers-pass-seven-field-buffers-to-the-twelve-field-keyrdr),
  rather than treating matching original keyword failures as simulation parity.
- Database wrappers are not automatically independent of the variant. Current
  examples include `PRGPRM`, `PLOT` and `DBSCOM`; sharing their compiled objects
  requires checking the selected includes, dimensions and COMMON layouts.

## Completion rules for every implementation task

Follow [Contributing.md](Contributing.md) and preserve the existing commentary
and formatting. Add concise comments explaining new reuse boundaries, fallback
paths and invalidation rules where they help future debugging.

1. Inspect the current implementation and all consuming source lists before
   editing. Recheck dependencies when starting each task; this list is a plan,
   not proof that a proposed transformation is safe.
2. Preserve arithmetic precision and evaluation order, accumulation order,
   random-number state, argument mutations, diagnostics, keyword behavior,
   output schemas and restart behavior. Do not fix inherited model bugs as part
   of an optimization; record newly confirmed defects in `FOUND_BUGS.md`.
3. Compare the candidate with its immediate pre-change baseline, then run the
   existing original-F77 comparison for affected variants. Run the full suite
   after shared changes. Investigate differences; do not weaken comparison
   masks or introduce numerical tolerances to accept an optimization.
4. Keep controlled BC/ON comparisons separate from pristine-original parity.
   If a path cannot be exercised, state the coverage limit and leave that part
   unresolved rather than marking it verified.
5. Add focused cases only where existing decks miss a behavior the change could
   alter. Measure before/after with identical inputs and flags, using repeated
   unprofiled runs. Report CPU/wall time and memory or disk use as relevant.
6. Run `git diff --check`. Mark a task complete only after its acceptance checks;
   record a concise result here, including a decision to defer or reject a
   candidate that offers no useful benefit. Keep builds and raw results in
   temporary directories outside the repository; discard them when finished.
   Do not add test archives, logs, receipts or separate completion reports.

The existing full-suite command is:

```sh
python3 tests/parity/check_suite.py --cache-dir /private/tmp/mfvs-suite
```

For compiler and build-system experiments, also verify the actual compiler
commands and artifact reuse. An unchanged cached result is not evidence that
new flags were used; use a fresh temporary build/cache when necessary.

## 1. Establish the comparison and performance baseline

- [x] **O01 — Establish the current parity baseline.**
  Confirm the current main/NVEL revisions, compiler versions, effective flags,
  selected variants and available decks. Use the pinned original F77 main
  revision `c210536536d81fd9b46dccb9a571f0de1e55e944` and its pinned NVEL
  `38548071d5aa652bb90c7f111f86b427f798a1c9`, as specified by the existing tools.
  Run the current suite and identify existing differences before optimization.
  Acceptance: a concise baseline outcome, current test count and explicit
  Canadian comparison limits; no attempt to repair unrelated failures.

  Result, 16 November 2026: main `b344e8624f4f065d3193defeec249b9ac46abd20`,
  NVEL `a73e697e220ffd0f792aa7ac10d84c911edb7ad7`; no model-source changes.
  A fresh, uncached full suite used the pinned main/NVEL references above,
  with `--workers 4 --jobs 4`. All 22 U.S. variants matched, including restart
  files; only the documented BC/ON differences remained. The final line was:

  ```text
  FAIL 22/24 variants; working tree b344e862+changes vs original c2105365; failed: bc on
  ```

  Selection: all 24 manifests (`ak bc bm ca ci cr cs ec em ie kt ls nc ne oc
  on op pn sn so tt ut wc ws`). Their default suites contain 66 make targets,
  43 distinct keyword decks within variants and 90 model launches per tree.
  Separate API, AllCycleActivities and nested economics tests are outside
  this command. The transcript's 177-test count is not the current count.
  Original BC/ON stop with `STOP 20` at keyword input; current BC StandStructure
  still traps later, while YSM-SkyRanch and ON Hardwood run farther. This run
  establishes neither pristine-original Canadian simulation parity nor a new
  controlled Canadian comparison. No failures were repaired or masks changed.

  Environment for O01/O02: macOS 26.6.2, arm64 Apple M5 Pro, 18 cores, 64 GB;
  GNU Fortran 15.2.0, GNU C 16.2.0, GNU Make 3.81, Python 3.14.4 and SQLite
  3.51.0. Actual compiler commands used the following flags, with no `-O`
  option (effective `-O0`) and no PIC option on this Mac:

  ```text
  Fortran: -g -cpp -DCMPgcc -Wall -Wno-integer-division -ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace
  C: -DANSI -DCMPgcc -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION -Dunix
  ```

  Snapshot version macros were `unknown`, as expected outside Git. The build
  commands and generated binaries were inspected; cached results were not used.

- [x] **O02 — Refresh representative timings and profiles.**
  After O01, time IE `ie`, `iet01` and `DBReportTest`, PN `pnt01`, CA `cat01`,
  and SN `snt01`/`sn` where available. Include multi-stand and stop/restart
  coverage, and an available larger real workload before generalizing results.
  Separate profiler overhead from unprofiled timing; measure build time and
  storage separately. The missing ZIP contents do not block fresh local work.
  Acceptance: a reproducible workload and concise measurements identifying
  which transcript priorities still apply; Windows/Linux claims remain
  unverified unless measured on those platforms.

  Result, 16 November 2026: the following are local baseline measurements,
  using O01's sources, tools and flags. Each workload had one warm-up and five
  unprofiled runs, interleaved with fixed shuffle seed 1602, without concurrent
  builds. Each run started with a fresh copy of its `tests/FVSxx` folder;
  copying and inspection were outside `/usr/bin/time -l -p`. Model processes
  used `LC_ALL=C TZ=UTC`, with the suite's ambient build switches cleared.
  CPU is user plus system time; RSS is the median peak resident size in MiB.

  | Workload | Median wall seconds (range) | Median CPU seconds | Peak RSS MiB |
  |---|---:|---:|---:|
  | IE `ie` | 1.28 (1.23–1.29) | 1.28 | 16.48 |
  | IE `iet01` | 0.56 (0.56–0.57) | 0.55 | 17.53 |
  | IE `DBReportTest` | 14.62 (14.48–14.79) | 14.59 | 36.45 |
  | PN `pnt01` | 0.55 (0.54–0.58) | 0.54 | 14.14 |
  | CA `cat01` | 0.57 (0.55–0.59) | 0.56 | 13.30 |
  | SN `snt01` | 0.62 (0.59–0.63) | 0.61 | 14.47 |
  | SN `sn` | 0.60 (0.57–0.64) | 0.60 | 16.73 |
  | Native inventory, IE: 8 populated stands, 469 input trees | 10.96 (10.70–11.12) | 10.94 | 14.88 |
  | Native inventory, CA: 5 populated stands, 373 input trees | 7.06 (6.91–7.49) | 7.05 | 13.73 |

  Stop/restart pairs used the existing `iet02`, `pnt02`, `cat02` and `snt02`
  commands: stop point `2,2040` for IE/PN/CA and `2,2020` for SN, followed by
  `--restart`. Median combined wall/CPU seconds were respectively 0.58/0.58,
  0.55/0.54, 0.58/0.56 and 0.66/0.64; peak RSS was 15.39, 14.89, 14.02 and
  15.23 MiB. Retained baseline exit codes were `iet01=20`, `iet02=20,0` and
  `sn=10`; the other standard cases returned zero. CA inventory returned 10
  with habitat-default warnings after completing all five stands.

  The larger temporary inventory workload came from `tests/FVSie/FVS_Data.db`:
  select populated stands by `lower(trim(Variant))`, order by `Stand_CN`, use
  their original Stand_ID/Inv_Year, and read stand/tree rows with
  `WHERE Stand_CN= '%Stand_CN%'`. Each stand requested ten cycles and
  `FMIN/POTFIRE/FUELOUT/SNAGSUM/END`, then `PROCESS`. No tree records were
  changed or transferred between variants. An additional nine-stand SN sweep
  (660 input trees) stopped on its eighth stand at R9HT's power statement;
  its 3.99-second median wall time is an incomplete run, excluded above.
  Original F77 reproduced the trap and matched all four partial output files.
  The isolated offending stand also traps in both versions; see
  [bug 6](FOUND_BUGS.md#6-reproduced-r9-clark-power-trap-historical-guard-still-under-investigation).
  There is no production deck on this Mac, so these results do not establish
  production performance. Windows/Linux timings and speedup estimates remain
  unverified.

  Separate sampling used `/usr/bin/sample PID 60 1 -mayDie -file REPORT`
  (a ten-second limit sufficed for the short decks), three fresh runs per deck,
  with capture through process exit. Percentages below are inclusive shares
  of main-thread stack samples, not speedups; nested columns must not be added.
  Formatted I/O counts the union of stacks containing gfortran transfer,
  formatted read/write or transfer-completion routines.

  | Workload | Samples | Formatted I/O % | STKVAL % | SPEQCOEF % | FMSNAG % |
  |---|---:|---:|---:|---:|---:|
  | IE `ie` | 3,187 | 45.5 | 2.3 | 26.9 | 40.8 |
  | IE `iet01` | 1,417 | 41.0 | 9.0 | 13.3 | 0.0 |
  | IE `DBReportTest` | 38,913 | 41.0 | 0.8 | 24.5 | 51.8 |
  | PN `pnt01` | 1,110 | 63.8 | 16.3 | 25.8 | 26.7 |
  | CA `cat01` | 1,142 | 56.3 | 20.8 | 22.5 | 16.7 |
  | SN `snt01` | 1,370 | 69.7 | 34.2 | 19.1 | 11.2 |
  | SN `sn` | 1,402 | 61.0 | 28.7 | 18.5 | 11.9 |

  Profiled median wall times were 1.7–14.0% higher than unprofiled medians;
  CPU differences ranged from -1.2% to +8.3%, including run variation.
  For `DBReportTest`, profiled wall/CPU medians were 15.39/14.61 seconds.
  Profiled times are excluded from the timing table. Most STKVAL samples point
  to the final-loop READ at `vbase/stkval.f90:639`, supporting O04. NVEL parsing
  remains prominent, supporting O06–O12. FMSNAG remains worth O16's audit,
  without establishing that its calls are interchangeable. SQLite accounts
  for 21.2% of `iet01` samples but only 2.1% of `DBReportTest`; database changes
  need workload-specific evidence. No compiler or caching change was adopted.

  Build/storage measurements were separate: one fresh `make FVSxx.setup -j4`
  per variant, sequentially, producing both executable and shared library.
  Sizes below are logical MiB; build directories include copied inputs and
  compiled files, excluding the two final binaries. These are single-build
  baselines, not repeated estimates or all-variant projections.

  | Variant | Wall seconds | CPU seconds | Compile commands | Build directory MiB | Executable / library MiB |
  |---|---:|---:|---:|---:|---:|
  | IE | 19.24 | 60.32 | 712 | 35.28 | 8.23 / 8.23 |
  | PN | 18.04 | 57.19 | 695 | 35.97 | 8.57 / 8.57 |
  | CA | 17.08 | 53.64 | 629 | 33.68 | 8.10 / 8.10 |
  | SN | 15.62 | 49.33 | 575 | 31.33 | 7.44 / 7.44 |

  Source-list checks and `git diff --check` passed. Temporary builds, raw
  timings, profiles and generated outputs were discarded; only this summary
  and the updated bug record are retained.

- [ ] **O03 — Investigate the R8PREPCOEF state hazard.**
  Inspect `volume/NVEL/r8clkdib.f90`, its callers and the pinned original.
  Trace `SPECPR`, `EQNPR`, `GEOAPR` and the caller-owned `COEFFS` across repeated
  and alternating equation calls. Confirm and record any inherited defect
  not already covered by `FOUND_BUGS.md`. Acceptance: the affected paths and
  compiler/cache implications are understood. Adding SAVE, initializing state
  or changing cache behavior is separate model-bug work, not this task.

## 2. Reduce repeated species and equation conversions

Start with O04 after O01–O02. O03 informs the NVEL and compiler experiments;
an unresolved defect must not be silently corrected by those changes.

- [ ] **O04 — Hoist STKVAL's final-loop species reads.**
  In `vbase/stkval.f90`, parse the needed nonblank FIA codes once per call
  before the final `IS=1,210` × `ISPC=1,MAXSP` loop. Keep the existing loop and
  addition order. Preserve the final loop's own blank/zero behavior; do not
  import the different 998/999 handling from earlier stocking stages.
  Acceptance: identical stocking/type results and diagnostics, with fewer
  formatted reads and measured PN/CA/SN timing.

- [ ] **O05 — Assess reuse in STKVAL's other stocking stages.**
  After O04, consider the other four FIA-code read sites separately. Preserve
  their present-species guards, blank handling and zero-to-999 substitution;
  avoid parsing previously unused entries on earlier paths. Acceptance:
  focused coverage of stocking-stage branches and unchanged accumulation
  order, or a documented decision that O04 captures enough benefit.

- [ ] **O06 — Define and verify a narrow integer-read fast path.**
  Establish the actual field widths and formats required by O07–O12. Convert
  all-digit fields directly only where equivalent; retain the original
  formatted READ for other input. Check leading/embedded blanks, signs, zero,
  nonnumeric input and existing error/IOSTAT behavior. Acceptance: comparison
  against the original conversion for valid digit combinations and focused
  fallback cases. Keep the helper small and respect NVEL's separate repository.

- [ ] **O07 — Optimize SPEQCOEF conversions.**
  Apply O06 within `volume/NVEL/nsvb.f90:SPEQCOEF`. Preserve its in-place
  normalization of `VOLEQ(5:7)`, division/STDORG selection, first-match table
  search and all returned values. Acceptance: direct cases cover normalized
  and nonnormalized equation strings, missing species and fallback equations,
  followed by volume/biomass and integrated parity comparisons.

- [ ] **O08 — Optimize CALC_EQVW and NVBC conversion sites.**
  After O07, handle each remaining conversion in these routines as a bounded
  edit. Preserve short-tree error returns, Jenkins-group branches, call order
  and mutations made by callees. Acceptance: identical volumes, biomass,
  errors and strings for normal and fallback paths; no arithmetic rewrite.

- [ ] **O09 — Optimize region, forest and province parsing.**
  Review `GetRegnWF` and `NVB_EcoProv` in `volume/NVEL/nsvb.f90`, applying O06
  one routine at a time. Preserve FORST/DIST padding and in-place mutations,
  lookup order and regional defaults. Acceptance: identical outputs and
  modified arguments for digit, padded and fallback cases.

- [ ] **O10 — Optimize VOLINIT and VOLINITNVB integer reads.**
  Review `volume/NVEL/volinit.f90` after O06. Preserve substring widths,
  CONSPEC length-dependent branches, zero/default selection, and NVB/non-NVB
  dispatch. Acceptance: exact direct and integrated comparisons across the
  affected equation families, with each entry point verified separately.

- [ ] **O11 — Assess fixed-width integer WRITE replacements.**
  Separately examine `NVB_DefaultEq` and `VOLINITNVB` equation/species string
  construction. Preserve field width, blank padding, zero filling, signs and
  overflow output; retain the original WRITE outside a proven fast-path range.
  Acceptance: byte-identical strings and errors over the relevant ranges and
  boundary cases. Skip sites whose remaining cost does not justify the change.

- [ ] **O12 — Reduce NATCRS conversion overhead.**
  Inspect `vvolume/fvsvol.f90:NATCRS` after O06. Review the FIA-code READ,
  forest-number WRITE and character-only WRITE separately. Preserve exact
  character assignment/padding semantics, COMMON state and ENTRY contracts.
  Acceptance: identical volume results, strings and diagnostics; add any new
  source/include to every consuming manifest if a helper is introduced.

- [ ] **O13 — Audit the lifetime of FIA species-code data.**
  Re-inventory active `READ(FIAJSP...)` sites using current manifests; the
  transcript's 41-site count includes a different tree. Trace initialization,
  mutation, variant mappings, multi-stand/API reuse and restart behavior before
  considering a persistent integer table. Acceptance: identify measured hot
  consumers and a safe lifetime; do not add COMMON/restart state merely to
  avoid a few cold conversions.

- [ ] **O14 — Optimize other measured FIA-code consumers.**
  After O13, use local hoisting or the verified fast path in worthwhile
  consumers such as INTREE, DISPLY and selected SITSET/volume routines.
  Preserve differences between I3/I4 reads and each caller's blank/default
  rules. Acceptance: one routine or small consumer group verified at a time.
  A shared startup table proceeds only if its initialization and invalidation
  contract is established; do not edit inactive copies to satisfy an old count.

- [ ] **O15 — Re-profile after the parsing changes.**
  Repeat O02 on the accepted changes before taking on volume caching or broad
  flag changes. Acceptance: measured cumulative improvement and a refreshed
  hotspot ranking; do not add sampled percentages or assume the remaining
  workload has the same bottlenecks.

## 3. Avoid repeated snag-volume work

- [ ] **O16 — Audit CWD1/CWD2 volume-call equivalence.**
  After O15, trace `fire/vbase/fmsnag.f90`, the ENTRY paths in
  `fire/base/fmcwd.f90`, FMSVL2 and other callers such as FMSALV/CWD3. Check
  hidden state, mutable arguments, diagnostics, early returns and keyword
  effects as well as explicit species/diameter/height inputs. Acceptance:
  establish whether removing a repeated call preserves every observable
  effect; matching argument values alone are insufficient.

- [ ] **O17 — Reuse volume within a proven snag call pair.**
  Only if O16 establishes equivalence, reuse the computed total volume within
  the same snag/year operation. Handle pairs where either entry returns early
  and independent CWD callers. Avoid cross-year/stand caches. Acceptance:
  focused fall/breakage/salvage, multi-year, debug and restart comparisons,
  unchanged RNG state, fewer calls and a measured benefit. Retain explicit
  limits for Canadian snag paths affected by known defects.

## 4. Evaluate compiler optimization in isolated steps

- [ ] **O18 — Trial optimization of SQLite and its C wrapper.**
  After O15, build only `dbsqlite/sqlite3.c` and `fvsqlite3.c` with `-O2`,
  leaving Fortran and FOFEM flags unchanged. Preserve SQLite feature macros,
  threading settings and interfaces. Acceptance: actual optimized commands,
  exact schemas/typed rows, repeated open/write/close behavior and full
  affected-output comparison. Treat this as a measured trial, not risk-free.

- [ ] **O19 — Trial optimization of FOFEM C code separately.**
  After O18, evaluate `fire/fofem` with conservative `-O2`, preserving existing
  floating-point behavior and traps. Keep unrelated C sources separate.
  Acceptance: exact fire/fuel/carbon results on exercising decks and a timing
  benefit; no fast-math flags or acceptance based only on successful linking.

- [ ] **O20 — Diagnose Fortran initialization and interface sensitivity.**
  After O03 and O15, compare controlled plain `-O0` builds with diagnostic
  `-finit-integer=-99999 -finit-real=snan` builds where supported. Trace any
  difference or trap before attributing it to a cause; warning counts and
  the number of SAVE statements alone do not establish a defect. Acceptance:
  relevant hazards are recorded and the next trial is either cleared for
  investigation or explicitly deferred. Do not ship diagnostic initialization.

- [ ] **O21 — Trial conservative Fortran optimization.**
  Only after O20, evaluate `-O2 -ffp-contract=off` in fresh controlled builds,
  retaining other numerical flags. Do not use `-Ofast`, fast-math, native-CPU
  tuning, expression reassociation or changed precision. Acceptance: exact
  affected/full-suite comparisons plus available larger and repeated/API
  workloads. Any unexplained difference blocks adopting the flag; no model
  bug fixes or blanket SAVE changes are bundled into this task.

## 5. Reduce repeated compilation and build storage

These tasks can follow O02 independently of runtime work. Keep accepted
compiler settings fixed while measuring build changes.

- [ ] **O22 — Identify genuinely reusable compilation units.**
  Inventory current source lists, transitive includes, preprocessor definitions,
  module dependencies, PIC/ABI requirements and initialized COMMON/BLOCK DATA
  for SQLite, FOFEM, NVEL and Fortran database wrappers. Compare Make and CMake:
  CMake already defines SQL/FOFEM libraries within its variant builds.
  Acceptance: a current eligible-unit list and compile/storage baseline;
  variant-dependent wrappers remain separate unless equivalence is proven.

- [ ] **O23 — Compile eligible SQLite C units once per configuration.**
  After O22, reuse objects or a static archive with equivalent settings, while
  keeping each variant's runtime state separate. Avoid introducing a shared
  runtime library requirement solely to save compile time. Acceptance: clean
  and parallel builds of executable/shared targets, unchanged exports/API and
  database behavior, and measured reductions in compile jobs and build time.

- [ ] **O24 — Compile eligible FOFEM C units once per configuration.**
  Apply the same bounded approach after O22, separately from O23. Preserve
  macros, selected platform code, symbol resolution and runtime state.
  Acceptance: fire-path parity, executable/shared-library loading, clean and
  incremental build checks, and a measured benefit.

- [ ] **O25 — Reuse eligible NVEL compilation across variants.**
  After O22, build only a proven common subset once per equivalent compiler,
  flags and source revision. Preserve variant-specific selections, include
  lookup, module ordering and required archive/BLOCK DATA members. Acceptance:
  all consuming variants link and compare correctly, shared-library/API users
  still work, NVEL edits invalidate reuse, and parallel builds cannot race.
  Keep this separate from any Fortran database-wrapper sharing proposal.

- [ ] **O26 — Replace blanket include dependencies with accurate ones.**
  Review `bin/makefile` dependencies on every include/module. Track actual
  direct/transitive and preprocessed dependencies, including converted `.f90`
  includes and FVSVERSION ordering. Acceptance: clean parallel builds and
  disposable touch/change checks show that every affected object rebuilds,
  unrelated objects are reused, and incremental outputs match a clean build.

- [ ] **O27 — Reduce copied-source overhead in variant build directories.**
  After O26, measure copying cost and disk use before choosing source-path
  compilation, links or another small change. Preserve include precedence,
  basename-collision checks and platform support. Acceptance: clean and
  incremental builds use the intended current sources and cannot modify the
  originals through a linked copy. Skip this if savings are insignificant.

- [ ] **O28 — Add optional release packaging without debug symbols.**
  Measure current executable/shared-library sections, then assess split debug
  symbols or stripping of packaged copies while retaining debug builds.
  Acceptance: measured disk savings, preserved exported/API symbols and
  loading, and identical simulation outputs. Do not delete developer binaries
  or make the transcript's Windows size estimate a cross-platform promise.

## 6. Reduce database transaction overhead

- [ ] **O29 — Classify transaction candidates correctly.**
  Inspect current writers and their callers, including transaction ownership,
  user SQL, errors and nested calls. The transcript's 21 files with STEP but
  no BEGIN include readers and lifecycle routines; that search is not a list
  of 21 safe write batches. Acceptance: identify actual repeated INSERT loops
  in autocommit mode and choose only measured candidates. Preserve the known
  prepared-statement leak in bug 9 as separate bug work.

- [ ] **O30 — Batch eligible snag-summary INSERT loops.**
  After O29, start with `dbsqlite/dbsfmssnag.f90` if it still qualifies. Give
  the batch a clear transaction owner and preserve statement/row order,
  visibility required by callers, and error handling. Acceptance: exact schema
  and typed-row comparisons, repeated/multi-stand cases, no broken surrounding
  transaction, and a measured improvement on a deck that exercises the writer.

- [ ] **O31 — Batch eligible carbon and fuel INSERT loops.**
  After O29–O30, assess `dbsfmcrpt.f90`, `dbsfmfuel.f90` and `dbsfuels.f90`
  one writer at a time; skip existing or unsuitable batches. Acceptance:
  unchanged successful outputs and examined failure/transaction behavior,
  exact database comparisons and useful timing improvement for each change.

- [ ] **O32 — Batch other worthwhile report INSERT loops.**
  After O29–O31, consider `dbsstrclass.f90` and remaining measured candidates.
  Split further work by writer rather than wrapping all report calls in one
  transaction. Acceptance: the same ownership, error, output and timing checks;
  stop when small row counts or existing batching make further changes unhelpful.

## 7. Storage assessment and final review

- [ ] **O33 — Assess optional database-output size reduction; deferred.**
  Recheck page use, free pages and dominant tables on representative outputs.
  The transcript found a compact DB dominated by FVS_TreeList, so VACUUM was
  not a demonstrated opportunity. Acceptance for this assessment is a measured
  decision on whether any compatible reduction exists. Normalizing CaseID,
  changing schemas, omitting rows/columns or changing requested reports needs
  separate user authorization and is outside this parity-preserving checklist.

- [ ] **O34 — Verify cumulative behavior and summarize measured savings.**
  After the selected implementation tasks, repeat the full comparison and O02
  measurements using the final tree. Summarize accepted runtime, build-time,
  memory and disk changes, rejected/deferred candidates and coverage limits
  briefly in this checklist. Acceptance: no new unexplained output differences,
  no overstated platform guarantees and no generated result archives added to
  the repository. Reorder remaining tasks from the measured final profile.

## Boundaries retained from the review

- Keep MAXTRE, tree compression/tripling, scientific coefficients, equation
  selection and model defaults unchanged. PRGPRM explicitly warns that MAXTRE
  can affect model behavior; the reported memory use does not justify it here.
- Leave powers, transcendental functions, floating-point accumulation and RNG
  sequencing unchanged. A faster mathematical replacement can change results.
- Do not introduce threading inside model routines or cross-year/stand caches
  as an incidental optimization. Those need a separate state/lifecycle design.
- PCTILE, DIST, DBCHK and scratch-file handling were inspected in the transcript
  without a demonstrated priority improvement. Revisit only if fresh profiles
  identify a real cost; inspection alone does not create an implementation task.
- Preserve requested report content and the default database contract. Optional
  output reduction is separate from making the existing computation faster.
