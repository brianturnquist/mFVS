# mFVS consolidation findings

Fresh source audit: 15 November 2026, checkout `1cf093f3`.

The goal is one definition of each calculation, with variant differences
expressed explicitly in coefficient banks, species mappings or bounded
branches. The findings below come from current source and the 24 build
manifests. F01–F14 were authorized and completed on
15 November 2026, and F15–F25 on 16 November 2026. F21a–F21f, F22a–F22h
and F26a–F26g are proposed follow-up tasks.

At the audit baseline, excluding include files and NVEL, the manifests
selected 1,286 Fortran source files. There were 92 repeated filename
families covering 666 files. These
counts identify comparison candidates, not interchangeable implementations
or a measured amount of removable code.

## Tracking and verification

- Keep the finding IDs stable. Check an implementation task only after the
  change is implemented and verified; record a concise completion note here.
- Keep model bug fixes separate from consolidation. Preserve existing
  behavior, including documented defects, unless a fix is separately
  authorized. Newly discovered defects belong in [FOUND_BUGS.md](FOUND_BUGS.md).
- Follow [Contributing.md](Contributing.md) and the
  [parity instructions](tests/parity/README.md). Preserve coefficient values
  and extents, species order, ENTRY contracts, initialization order, random
  draws, diagnostics and restart layout.
- Run comparisons in temporary directories outside the repository and
  discard their outputs. Keep concise findings and completion notes, not
  test-result archives, logs, receipts or separate completion reports.

For implementation, run the appropriate focused comparisons and the existing
regression suite. BC/ON need their documented controlled-baseline comparisons
and focused coverage; agreement at an original early-failure boundary does
not establish simulation parity. Completion notes below identify verified
changes; unchecked findings remain proposals.

## Bounded consolidation candidates

- [x] **F01 — ESNUTR: AK and shared establishment.**

  Completed 15 November 2026 in [estb/esnutr.f90](estb/esnutr.f90).
  AK now selects the existing shared definition, and its duplicate was
  removed. Executable logic and variant-selected includes are unchanged.
  AK's complete regression set matched original F77 (7 generated files).

- [x] **F02 — VARMRT: CS, LS and NE.**

  Completed 15 November 2026 in [vbase/varmrt.f90](vbase/varmrt.f90).
  One allocator retains the original CS/LS/NE tolerance banks at 96/68/108
  entries, selected by `VARIANTCD`. The three duplicate files were removed.
  All three regression sets matched original F77. Direct checks across all
  native species matched original F77 and baseline `b8894858` exactly in
  45 cases, including allocation state, background mortality, height handling
  and diagnostics. Changing a selected coefficient was detected for each
  variant. Other VARMRT implementations remain separate findings.

- [x] **F03 — BFVOL: CI, EC, KT and WS.**

  Completed 15 November 2026 in [vbase/bfvol.f90](vbase/bfvol.f90).
  One calculation retains each original breakpoint/coefficient bank at its
  native 19/32/11/43-row extent and preserves the species/surrogate comments.
  `VARIANTCD` selects coefficients after the existing log-rule and
  user-equation bypasses. All four duplicate files were removed, and all
  four regression sets matched original F77. In 22,680 direct cases, all
  species and methods 1/2/3/4/5/7 matched original F77 and baseline
  `b8894858` exactly, including arguments and flags, top kill, transitions
  and the actual form-class/log/scaling helpers. Changing a selected
  coefficient was detected for each variant.

- [x] **F04 — LOGS: repeated taper and log calculations.**

  Completed 15 November 2026 in [vbase/logs.f90](vbase/logs.f90), replacing
  the IE/WC/NE/SO definitions for ten consuming variants. One calculation
  preserves the two special-case returns, the common CI/IE/SO mapping
  prefix and suffix, WC's model-1 mapping, and NE's native 108-row mapping.
  All ten complete regression sets matched original F77. In 110,376 direct
  cases, all native species and size/top/minimum/stump combinations matched
  original F77 and baseline `96f1abcb` exactly, including argument values.
  A changed taper coefficient was detected for every consumer.

  The remaining six implementations were reviewed and retained: AK/UT/KT/WS
  have different mappings or special-case handling, EM is a zero-volume
  stub, and NC has a different ten-argument interface and calculation.
  Their possible later consolidations require separate scope.

- [x] **F05 — BRATIO: IE, EM and BC.**

  Completed 15 November 2026 in [vbase/bratio.f90](vbase/bratio.f90).
  One calculation retains the native 23/19/15-row coefficient and equation
  banks, diameter handling and output bounds. All 2,736 direct cases matched
  original F77 and baseline `96f1abcb` exactly across native species,
  diameter floors/caps and ignored heights; selected coefficient mutations
  were detected. IE/EM complete regression sets matched original F77.
  BC's complete generated outputs matched the immediate baseline separately;
  this does not establish pristine-original Canadian simulation parity.

- [x] **F06 — SITERANGE: shared lookup structure.**

  Completed 15 November 2026 in [vbase/siterange.f90](vbase/siterange.f90).
  CI/SO/TT migrated first, then UT and BM in separately verified steps.
  The five native range banks retain their original extents and values.
  UT keeps its alternate REGENT age basis; only BM accepts caller mode 3.
  Other modes return 0/999 before reading a species index. All five complete
  regression sets matched original F77. All 832 direct cases matched
  original F77 and baseline `96f1abcb` exactly, including unsupported modes
  with invalid species indices; selected table mutations were detected.

- [x] **F07 — DUNN: CA and NC.**

  Completed 15 November 2026 in [vbase/dunn.f90](vbase/dunn.f90).
  CA, NC and the existing CA consumer OC now select one definition, with
  the original 50-row CA and 12-row NC factors and unchanged `GETDUNN`
  entry. Both duplicate files were removed. All three regression sets
  matched original F77. Direct checks matched original F77 and baseline
  `b8894858` exactly for every species, all eight codes and fractional-code
  cases (66 cases), including `GETDUNN` before and after `DUNN`. Changing a
  selected factor was detected for each consuming variant.

- [x] **F08 — ESDLAY: CI, IE and establishment.**

  Completed 15 November 2026 in [vbase/esdlay.f90](vbase/esdlay.f90) for
  CI/IE/KT. One delay calculation retains the native 19/23/11-species scale
  and shape banks, including unused zero rows, plus all six shared budworm
  banks. Thresholds, clamping and arithmetic are unchanged. All three
  complete regression sets matched original F77. All 155,820 direct cases
  matched original F77 and baseline `96f1abcb` exactly across native rows,
  advance/subsequent modes and age/BA/budworm/draw boundaries, including
  argument/common-state values; selected coefficient mutations were detected.

- [x] **F09 — FMCBLK: IE, KT and BC.**

  Completed 15 November 2026 in [fire/base/fmcblk.f90](fire/base/fmcblk.f90).
  One BLOCK DATA definition initializes the common carbon-product tables.
  Manifests select the native mapping from
  [IE](fire/ie/FMBIOGRP.f90), [KT](fire/kt/FMBIOGRP.f90) or
  [BC](canada/fire/bc/FMBIOGRP.f90) data-only includes, classified in
  `bin/fortran_includes.txt`. The FMPROP common layout is unchanged and no
  runtime initializer is needed. All 7,321 initialized values matched
  original F77 and baseline `96f1abcb` before any model call; carbon-table
  and native-mapping mutations were detected for all three variants.
  IE/KT complete regression sets matched original F77. BC's complete
  generated outputs matched the immediate baseline separately.

- [x] **F10 — FMCBA: IE, KT and BC.**

  Completed 15 November 2026 in [fire/base/fmcba.f90](fire/base/fmcba.f90).
  One calculation retains IE/KT/BC's live and initial-fuel banks at their
  native 23/11/15-column extents, selected by a constant offset. The three
  duplicate definitions were removed; both ENTRY contracts are unchanged.
  All native fuel banks, common DATA and the calculation body were checked
  against baseline `6943c310`. In 5,200 direct cases, all native species,
  bare stands, cover levels, first/later years, SVSTART, fuel/photo overrides,
  ENTRY state and diagnostics matched original F77 and the baseline exactly.
  A changed selected coefficient was detected for each variant. IE/KT's
  complete regressions matched original F77; BC's complete outputs matched
  the immediate baseline separately.

- [x] **F11 — FMCROW: eastern variants first.**

  Completed 15 November 2026 in [fire/vbase/fmcrow.f90](fire/vbase/fmcrow.f90).
  LS/ON and then CS/NE were migrated in separately verified steps. One tree
  loop retains all four native species maps and CS/NE's additional original
  diagnostics; four duplicate files were removed. In 6,264 direct cases,
  all mappings, empty lists, regrowth gates, helper arguments, crown outputs
  and diagnostics matched original F77 and baseline `6943c310` exactly.
  A mapping mutation was detected for each variant. LS/CS/NE's complete
  regressions matched original F77; ON's complete outputs matched the
  immediate baseline separately. Western extraction, such as
  [fire/ie/fmcrow.f90](fire/ie/fmcrow.f90), remains future work with explicit
  eastern/western equation routing.

- [x] **F12 — FMCFMD: LS and ON.**

  Completed 15 November 2026 in
  [fire/vbase/fmcfmd_ls.f90](fire/vbase/fmcfmd_ls.f90), replacing both copies.
  A local species selector preserves the five ON cases for species 69–72
  while leaving every other native species test unchanged. In 47,712 direct
  cases, all species, bare/mixed stands, fuel/cover thresholds, seasons,
  drought, slash, user/static/dynamic settings, candidate weights and
  diagnostics matched original F77 and baseline `6943c310` exactly. Both
  variants detected a selected mapping/table mutation. LS's complete
  regression matched original F77; ON's complete outputs matched the
  immediate baseline separately. The existing planted-white-pine crown-cover
  exclusion was reproduced and recorded as bug 43, without changing it.

- [x] **F13 — FMCFMD: CS and SN.**

  Completed 15 November 2026 in
  [fire/vbase/fmcfmd_cs.f90](fire/vbase/fmcfmd_cs.f90), replacing both copies.
  The shared calculation preserves CS's two cedar species, SN's single
  cedar species, and their different zero-density division behavior.
  In 52,640 direct cases with actual forest-type classifiers, all species,
  cover groups, moisture/fuel thresholds, cedar densities/record splitting
  and diagnostics matched original F77 and baseline `6943c310` exactly.
  These checks used NaN initialization to expose the undefined moisture
  weights, reproduced bugs 40–41, and detected a variant-specific branch
  mutation for both variants. Both complete regressions matched original
  F77. Bugs 40–41 remain unfixed.

- [x] **F14 — FMVINIT: OP and the existing PN/WC shared version.**

  Completed 15 November 2026 in [fire/base/fmvinit.f90](fire/base/fmvinit.f90).
  OP now selects the shared initialization; its duplicate was removed.
  Species 24 selects OP's original defaults and decay class while all arrays
  retain native indexing. WC's density value and softwood classifications
  are unchanged. In 48 direct initialization cases, all 39 species and 37
  initialized fields, plus eight complete surrounding COMMON blocks,
  matched original F77 and baseline `6943c310` byte for byte. Checks covered
  sentinel states, initialization before/after variant setup and repeated
  calls; OP mapping, WC density and PN default mutations were detected.
  All three complete regressions matched original F77.

## Larger consolidation opportunities

- [x] **F15 — CWCALC: LS and ON.**

  Completed 16 November 2026 in [vbase/cwcalc_ls.f90](vbase/cwcalc_ls.f90),
  formerly the LS file that CS, NE and SN already used. ON now selects it
  and its copy was removed. ON keeps its 72-entry table of US species codes
  and its five additional cases (CH, HS, NC, OL, UH), which no US variant's
  codes reach. All four US regression sets matched original F77; ON's
  complete outputs matched the immediate baseline. In 2,269,156 direct cases
  (353,956 model calls plus sweeps of every native species, both crown-width
  modes, diameter/crown-ratio boundaries and three locations), results and
  COMMON memory matched baseline `1fefe63c` bit for bit. A changed ON alias
  (ON) and a changed shared coefficient (CS/LS/NE/SN) were detected.

- [x] **F16 — GRINIT: CS, LS and NE.**

  Completed 16 November 2026 in [vbase/grinit_ls.f90](vbase/grinit_ls.f90).
  The variant code comes from `VARIANTCD`; the default forest codes (CS 1,
  LS 5, NE 2) and NE's `EBAU` initialization are kept. The only other
  differences were reordered independent assignments. Two duplicates were
  removed. All three regression sets matched original F77. In 180 direct
  cases, including COMMON memory pre-filled with three byte patterns, every
  value GRINIT sets matched baseline `1fefe63c` byte for byte; changed forest
  codes (CS, LS) and `EBAU` (NE) were detected. CS and LS executables now
  also contain the unused 50-value TWIGCOM block.

- [x] **F17 — CROWN: CA and OC.**

  Completed 16 November 2026 in [vbase/crown_ca.f90](vbase/crown_ca.f90).
  OC's ORGANON crown-ratio branch and its two original minimum-crown
  conditions are guarded by `VARIANTCD` at their original positions. OC's
  copy was removed. Both regression sets matched original F77. In 4,563
  direct cases (every call from the regression decks and an added CRNMULT
  deck, plus trials varying the start flag, crown ratios, small, top-killed
  and dead trees, density, species and ORGANON flags), COMMON memory matched
  baseline `1fefe63c` bit for bit. Mutations in CA's minimum-crown path and
  OC's ORGANON branch were detected.

- [x] **F18 — CROWN: PN, WC and OP.**

  Completed 16 November 2026 in [vbase/crown_pn.f90](vbase/crown_pn.f90).
  PN/OP's 17-row and WC's 16-row coefficient banks are concatenated and
  selected by a constant offset; each variant keeps its native species map.
  Only OP takes the ORGANON branch, guarded as in F17. Two copies were
  removed. All three regression sets matched original F77. In 6,708 direct
  cases, checked as for F17 with species reassigned across all 39 species,
  COMMON memory matched baseline `1fefe63c` bit for bit. PN-row, WC-row and
  OP ORGANON mutations were detected. Debug output matched except for the
  unassigned value recorded as bug 45.

- [x] **F19 — HTGF: CS, LS and NE.**

  Completed 16 November 2026 in [vbase/htgf_ls.f90](vbase/htgf_ls.f90).
  Each variant keeps its HTCALC equation set and diagnostic unit (CS/NE
  unit 16, LS `JOSTND`). CS/LS call their six-argument BALMOD through an
  explicit interface; NE calls its three-argument form through a local
  adapter with its own interface. Two copies were removed. All three
  regression sets matched original F77. In 6,282 direct HTGF/HTCONS cases
  varying random increments, site, heights, BAL inputs, size caps, tripling,
  species and height corrections, COMMON memory matched baseline `1fefe63c`
  bit for bit. CS BAL, LS equation-set and NE adapter mutations were
  detected. Debug output matched for all three variants.

- [x] **F20 — HTDBH: LS and ON.**

  Completed 16 November 2026 in [vbase/htdbh_ls.f90](vbase/htdbh_ls.f90).
  The banks have 72 rows: rows 1–68 are shared and rows 69–72 are ON's
  planted species, which LS never indexes. ON's copy was removed. LS's
  regression matched original F77; ON's complete outputs matched the
  immediate baseline. In 36,653 direct cases (21,517 model calls plus sweeps
  of every native species, both modes and diameter/height boundaries),
  results and COMMON memory matched baseline `1fefe63c` bit for bit. An LS
  shared-row and an ON planted-row mutation were detected.

- [x] **F21 — INITRE: extract individual shared keyword handlers.**

  Completed 16 November 2026. The 21 handlers that were statement-identical
  in the US, ON and BC INITRE moved to
  [vbase/initrekeys_mod.f90](vbase/initrekeys_mod.f90), and VOLEQNUM moved to
  [vbase/initrevol_mod.f90](vbase/initrevol_mod.f90). The VOLEQNUM copies
  differed only in code no variant could reach: the US FIA-VBC guard
  (`LFIANVB` is set only by the US FIAVBC keyword), the AK forest branch,
  ON/BC's `'BC'`/`'ON'` region entries and BC's 10-blank reset of the
  11-character `VEQNNC`. Each handler takes INITRE's locals by reference;
  an exit code reproduces INITRE's other exits. Net change: −1,373 lines.
  All 22 US regression sets matched original F77; BC/ON matched the
  immediate baseline. Coverage decks ran every extracted handler in several
  forms, including blank, invalid, PARMS and echo-off, in all 24 variants;
  outputs matched baseline `94c172ea` except the undefined bytes of bugs
  45 and 47. The remaining volume handlers are F21a–F21e.

  The follow-up tasks below are ordered by complexity. A shared handler
  keeps each differing statement behind `if (LMETRICVAR)`, so every variant
  runs its original statements. Verify each task as for F21: the regression
  suite, BC/ON against the immediate baseline, and coverage decks that run
  the handler in all 24 variants with values whose imperial and metric
  results differ. Statement counts are US/ON/BC.

- [ ] **F21a — MCFDLN and BFFDLN (low).**

  The copies differ only in region lists (`case` entries and the FIA-VBC
  guard, as in VOLEQNUM) and one echo FORMAT. Confirm that each region
  entry is unreachable where it is absent, then select the FORMAT with
  `LMETRICVAR`.

- [ ] **F21b — MCDEFECT and BFDEFECT (low–medium).**

  Three or four region hunks and two to four FORMAT hunks; BC differs from
  ON in up to four places. Same method as F21a.

- [ ] **F21c — DEFECT (medium–high).**

  220/184/162 statements. ON and BC differ only in region lists and
  FORMATs, so share the metric pair first. The US copy then differs in
  8–12 logic hunks that need `LMETRICVAR` branches.

- [ ] **F21d — BFVOLUME (high).**

  132/153/151 statements. The US copy differs from the metric copies in
  11–13 conversions and 8 logic hunks, and ON and BC also differ from each
  other (7 conversions, 6 logic hunks): a three-way merge.

- [ ] **F21e — VOLUME (high).**

  188/158/158 statements: 12–13 conversions and 9 logic, 7 region and
  2 FORMAT hunks against the US copy; ON and BC also differ. The metric
  copies add an `ITOPD8` rule, do not set `LCVOLS` as the US copy does, and
  reuse FORMAT labels for different messages. First establish whether the
  missing `LCVOLS` is a defect (record it in FOUND_BUGS if so), then merge
  with renumbered FORMATs.

- [ ] **F21f — Survey the other INITRE differences.**

  Dozens of further handlers differ between the copies (for example
  PROCESS, STDINFO, THINDBH, HTGSTOP and SDIMAX), and several identical ones
  were skipped because they use INITRE's end-of-file exit or shared labels.
  Classify them as above and add tasks.

- [x] **F22 — FMIN: extract remaining shared keyword handlers.**

  Completed 16 November 2026 in
  [fire/base/fminkeys_mod.f90](fire/base/fminkeys_mod.f90): the 15
  handlers identical in the imperial and metric FMIN (SALVSP, MOISTURE,
  POTFIRE, SNAGFALL, SNAGBRK, SNAGDCAY, SNAGOUT, FUELPOOL, PILEBURN,
  FUELMULT, DROUGHT, SNAGPSFT, FUELMODL, FUELFOTO, FMODLIST), using the
  F21 method. Net change: −344 lines. Verification as for F21. The other
  27 handlers differ between the imperial and metric FMIN in one to nine
  places; F22a–F22h group them by complexity. Each can follow the existing
  `FM*REQ_MOD` pattern with `LMETRICVAR`, verified as described under F21.

- [ ] **F22a — FUELTRET (trivial).**

  Identical apart from a space; skipped only because FORMAT 2504 was shared
  with SALVSP. Extract it with the F21 method.

- [ ] **F22b — Report-window flags (low).**

  FUELREPT, MORTREPT, CARBREPT, CARBCUT, SOILHEAT and CANFPROF each differ
  in one place: the imperial year window versus the metric −1 sentinel (the
  bug 24 rule). Follow `FMFUELREQ_MOD` and `FMDWCVREQ_MOD`.

- [ ] **F22c — One unit conversion (low).**

  FUELINIT, SNAGINIT, POTFWIND, POTFTEMP, CARBCALC, FIRECALC and FUELSOFT.

- [ ] **F22d — FORMAT text only (low).**

  FUELDCAY and POTFMOIS (one FORMAT each) and DUFFPROD (two).

- [ ] **F22e — Two or three places (medium).**

  SNAGCLAS, SALVAGE, SNAGPBN, MORTCLAS and CANCALC (two conversions each),
  SIMFIRE (three) and LANDOUT (a conversion and a FORMAT).

- [ ] **F22f — FLAMEADJ and FUELMOVE (medium–high).**

  FLAMEADJ differs in four conversions; FUELMOVE in five conversions and a
  FORMAT.

- [ ] **F22g — DEFULMOD (high).**

  Nine conversions; the metric copy has 124 statements against 114.

- [ ] **F22h — FMORTMLT (special).**

  The imperial section runs on into `ENTRY FMKEY` and the end of FMIN, and
  the metric copy converts its parameters into `PRMS` before `OPNEW`.
  Separate the section boundary first, then merge (two conversions).

- [x] **F23 — REGENT: consolidate calculations within the shared file.**

  Two extractions completed 16 November 2026 in
  [vbase/regent.f90](vbase/regent.f90), each verified separately:
  `REGHCAL`, the small-tree calibration ratio estimator and range trap
  (17 families; BC/ON differ), and `REGCALOUT`, the calibration database
  and CALBSTAT output (18 families). Native scale-factor arrays are passed
  assumed-size. Net change: −772 lines. All 22 US regression sets matched
  original F77, including CALBSTAT output; BC/ON matched the immediate
  baseline. With the trap thresholds changed identically in both trees,
  IE and SO took the trap 7 times with identical outputs. The suggested
  growth-blending block occurs in only three families, each different;
  the remaining repeats are tied to shared labels.

## Additional review opportunities

- [x] **F24 — VARGET/VARPUT: share serialization structure.**

  Completed 16 November 2026. [vbase/varput.f90](vbase/varput.f90) and
  [vbase/varget.f90](vbase/varget.f90) replace the nine copies of each and
  the PG character stubs. `VARIANTCD` selects each payload, which is kept
  verbatim in its own internal procedure, so field order and sizes are
  unchanged. AK's two ESCOMN members and BC's `ZBRSH` were appended to the
  common includes, and their private copies removed (BC's ESHAP member
  order changed). Writers and readers were verified in separate steps: all
  22 US regression sets matched original F77, including restart files,
  and BC/ON matched the immediate baseline. 22 files became two; net
  change: −514 lines.

- [x] **F25 — Acadian support review, without deletion.**

  Decided 16 November 2026: `acd/` is unsupported and is to be retired in a
  separate, authorized step; nothing was deleted. Upstream has never built
  it (no source list at `c2105365`), and it has not compiled upstream since
  `a8f62643` (June 2018) renamed TWIGCOM's `BAU` to `EBAU`; three of its 26
  files still use `BAU`. No test covers it, and FVS-ACD development
  appears to continue separately at the University of Maine.

  Revised 16 November 2026: the preferred direction is now to support ACD
  by following the University of Maine fork and using its build as the
  reference (option A in [acd/README.md](acd/README.md), tracked as F26).
  Nothing is to be retired meanwhile.

- [ ] **F26 — Acadian variant support (option A).**

  Bring ACD into the build by following fvs-modern, the University of
  Maine fork that already builds it, and verify against that fork's ACD
  build. [acd/README.md](acd/README.md) records the evidence and the other
  options. Licensing (the fork is MIT; mFVS is CC0) must be settled before
  any of the fork's source is copied.

- [ ] **F26a — Coordinate with the fvs-modern authors.**

  Confirm which ACD code is current, whether newer equations exist outside
  the repository, what test data exist, and whether their ACD changes can
  be released under CC0.

- [ ] **F26b — Build the reference.**

  Build FVSacd and FVSne (the control) from a pinned fvs-modern commit,
  outside the repository, and record the commit and compiler in the
  parity tools as for original F77.

- [ ] **F26c — Create `tests/FVSacd`.**

  Write decks with alpha species codes (ACD shares 106 of NE's 108 codes in
  a different order), starting from NE's `net01`, plus any University of
  Maine data.

- [ ] **F26d — Control comparison.**

  Run the NE decks in fvs-modern's NE and in mFVS's NE. The fork tracks
  upstream `b183c10f` (November 2025), not `c2105365`, so this measures
  base-code differences before any ACD result is judged.

- [ ] **F26e — Port the ACD routines.**

  Compare fvs-modern's `acd/` with mFVS's statement by statement, bring
  mFVS's copy to the same behaviour (at least `BAU` to `EBAU`), and add
  `VARIANTCD = 'AC'` and `LMETRICVAR = .false.` to its PRGPRM.

- [ ] **F26f — Integrate into the consolidated code.**

  Write `bin/FVSacd_sourceList.txt` against mFVS's shared files (the
  fork's list names pre-consolidation LS, IE, NE-fire and STRP files). In
  each of the 31 shared routines that select on `VARIANTCD`, give `'AC'`
  the branch of the variant whose file the fork uses. Extend the parity
  tools to the three-letter variant name.

- [ ] **F26g — Verify against the reference.**

  Compare every generated file, restart files included, with the fork's
  ACD build. Explain any differences by the F26d control comparison or
  record them.

## Bugs recorded during this audit

All four defects below are present in pinned original F77 `c2105365` and
remain unfixed. Their full descriptions and suggested fixes are in the bug
tracker; they are separate from the consolidation tasks.

| Bug | Finding | Verification and limit |
|---|---|---|
| [40](FOUND_BUGS.md#40-cssn-fuel-model-selection-reads-unassigned-moisture-weights) | CS/SN fuel selection reads an unassigned moisture weight on dry and wet branches. | Actual original/current routines and classifiers reproduced non-finite incoming fuel weights with NaN initialization. Whole-model fire impact was not measured. |
| [41](FOUND_BUGS.md#41-cssn-redcedar-fuel-selection-divides-unweighted-heights-by-tree-density) | CS/SN cedar-height calculation divides unweighted heights by density; splitting the same stand into two records changes fuel-candidate weights. | Actual original/current routines and classifiers reproduced the changed weights. Integrated fire-output impact was not measured. |
| [42](FOUND_BUGS.md#42-bc-snag-volume-calls-the-ten-argument-cftopk-with-nine-arguments) | BC passes nine arguments to the ten-argument CFTOPK, shifting argument types and omitting the final argument. | Compiling actual caller/callee sources together produced argument-type diagnostics in original/current code. Runtime volume effects and crash frequency were not measured. |
| [43](FOUND_BUGS.md#43-on-planted-white-pine-is-omitted-from-pine-crown-cover-in-fuel-selection) | ON planted white pine contributes basal area but no pine crown cover, changing candidate fuel weights in mixed stands. | Actual original, immediate-baseline and shared routines reproduced weights 8/9 of `(1,0)` for species 70 versus `(0,1)` for species 5. Integrated fire-output impact was not measured. |

## Original starting sequence

1. F01 — ESNUTR.
2. F02 — CS/LS/NE VARMRT.
3. F03 — CI/EC/KT/WS BFVOL.
4. F07 — CA/NC DUNN.

All four steps above are complete. Unchecked findings require separate
implementation authorization.

The initial read-only audit's source-list check and `git diff --check`
passed; its temporary defect-verification files were discarded. The first
four authorized consolidations (F01/F02/F03/F07) replace 11 source files
with four shared definitions, reducing this source set from 2,830 to 1,257 lines
(seven fewer files and 1,573 fewer lines). Scientific values and existing
defects are preserved.

Verification of F01/F02/F03/F07 on 15 November 2026 built all 24 variants.
All 22 U.S. regression sets matched original F77; BC/ON retained the documented
pre-existing differences from the failing original keyword readers. Their
manifests and every selected main-tree file were verified unchanged from
baseline `b8894858`; these tasks do not claim new Canadian simulation
parity. The source-list and formatting checks passed. Temporary direct
checks, comparison outputs and build copies were discarded; no new defects
were found during these four consolidations.

The second batch (F04/F05/F06/F08/F09), completed 15 November 2026 against
baseline `96f1abcb`, replaces 18 source/data files with five shared definitions
and three native data includes. This source set shrank from 3,488 to 1,538
lines: ten fewer files and 1,950 fewer source/data lines. Native table values,
dimensions, common-block layout and initialization timing were preserved.

Each bounded migration passed its focused regression and direct checks.
The final full run built all 24 variants; all 22 U.S. variants matched
original F77, including generated databases and restart files. BC's complete
generated outputs matched the immediate baseline after F05 and again after
F09. Its changed components also matched original F77 directly. BC/ON retain
the documented pristine-original comparison failures; this batch does not
claim Canadian pristine-original simulation parity. Source-list, formatting,
native-table and tracker-link checks passed. No model bug fixes were made.
Temporary drivers, outputs and build copies were discarded.

The third batch (F10–F14), completed 15 November 2026 against baseline
`6943c310`, consolidates 13 source files into five shared definitions.
This source set shrank from 5,008 to 2,460 lines: eight fewer files and
2,548 fewer lines. Native coefficient values and species order, public
interfaces, COMMON layouts, initialization timing and diagnostics were
preserved. F11 covers the four eastern variants; western extraction remains
future work.

Each bounded migration passed its focused checks. Across 111,864 direct
cases, the shared components matched original F77 and the immediate baseline;
the checks also detected selected table, mapping or branch mutations for
every affected variant. F13's NaN initialization exposes the retained
undefined reads rather than claiming defined behavior for those paths.
The final full run built all 24 variants. All 22 U.S. regression sets matched
original F77, including generated databases and restart files. BC and ON's
complete generated outputs matched the immediate baseline separately; their
documented pristine-original keyword-reader comparison failures remain.

Source-list, formatting, native-bank and tracker-link checks passed. Bug 43
was reproduced and recorded; bugs 40–41 were reproduced and retained. No
model bug fixes were made. Temporary drivers, outputs and build copies were
discarded; no verification archives or new test infrastructure were retained.

The fourth batch (F15–F20), completed 16 November 2026 against baseline
`1fefe63c`, consolidates 15 source files into six shared definitions. This
source set shrank from 9,680 to 4,556 lines: nine fewer files and 5,124 fewer
lines. Native coefficient banks, species maps and aliases, ENTRY and BALMOD
contracts, ORGANON conditions, diagnostics and their output units were
preserved.

Direct checks ran the baseline and shared routines side by side inside each
affected variant's executable. Each call started from identical memory.
The checks covered every call from the regression decks and, where a deck
reads a tree file, an added CRNMULT deck, plus perturbed trials and argument
sweeps. All 2,323,542 cases matched in results
and COMMON memory, and planted mutations were detected in every affected
variant. Debug-enabled decks matched the baseline in all ten variants, apart
from bug 45's unassigned value; ON's deck omitted CRNMULT because of bug 44.
The final full run built all 24 variants. All 22 U.S. variants matched
original F77, including restart files; BC and ON matched the immediate
baseline, and their documented pristine-original failures remain. Bugs 44–45
were found and recorded unfixed. No model bug fixes were made. Temporary
drivers, outputs and build copies were discarded.

The fifth batch (F21–F25), completed 16 November 2026 against baseline
`94c172ea`, moved 37 keyword handlers and two REGENT calibration blocks
into shared procedures, and replaced 22 serialization files and private
includes with two. Fortran source shrank by a net 3,003 lines; 22 files
were removed and five added. Seven steps (VARPUT, VARGET, FMIN, INITRE, two REGENT steps,
VOLEQNUM) were each verified separately. Every step built all 24 variants;
all 22 US variants matched original F77, including restart files, and BC
and ON matched the immediate baseline.

Statement-identical blocks were extracted mechanically. Each block's jumps,
labels and local names were checked against its host, and only the needed
COMMON files were included. Keyword-coverage decks and a forced-trap run
covered paths the regression decks do not reach. Bugs 46 (ON snag-volume
reports stop with a floating-point trap) and 47 (BC prints undefined
volume-equation codes after an invalid VOLEQNUM) were found and recorded
unfixed. No model bug fixes were made. Temporary trees, decks and outputs
were discarded.
