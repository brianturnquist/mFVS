# mFVS consolidation findings

Fresh source audit: 15 November 2026, checkout `1cf093f3`.

The goal is one definition of each calculation, with variant differences
expressed explicitly in coefficient banks, species mappings or bounded
branches. The findings below come from current source and the 24 build
manifests. F01–F09 were authorized and completed on
15 November 2026. The remaining unchecked findings are proposed work.

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

- [ ] **F10 — FMCBA: IE, KT and BC.**

  [fire/ie/fmcba.f90](fire/ie/fmcba.f90),
  [fire/kt/fmcba.f90](fire/kt/fmcba.f90) and
  [canada/fire/bc/fmcba.f90](canada/fire/bc/fmcba.f90) share the execution
  body after the variant fuel tables. Retain each variant's live-fuel and
  initial-fuel banks around one body.

- [ ] **F11 — FMCROW: eastern variants first.**

  [fire/ls/fmcrow.f90](fire/ls/fmcrow.f90) and
  [canada/fire/on/fmcrow.f90](canada/fire/on/fmcrow.f90) differ only in
  species mapping. [fire/cs/fmcrow.f90](fire/cs/fmcrow.f90) and
  [fire/ne/fmcrow.f90](fire/ne/fmcrow.f90) are also close. Share the tree
  loop and output assignment in separate verified steps. Western copies,
  such as [fire/ie/fmcrow.f90](fire/ie/fmcrow.f90), offer a later extraction
  with explicit eastern/western equation routing.

- [ ] **F12 — FMCFMD: LS and ON.**

  [fire/ls/fmcfmd.f90](fire/ls/fmcfmd.f90) and
  [canada/fire/on/fmcfmd.f90](canada/fire/on/fmcfmd.f90) differ in five
  species-selection cases accommodating ON species 69–72. Retain those
  mappings explicitly in one calculation.

- [ ] **F13 — FMCFMD: CS and SN.**

  [fire/cs/fmcfmd.f90](fire/cs/fmcfmd.f90) and
  [fire/sn/fmcfmd.f90](fire/sn/fmcfmd.f90) differ only in cedar-species
  selection and a zero-density guard. Preserve both differences and the
  defects recorded as bugs 40–41 until fixes are separately authorized.

- [ ] **F14 — FMVINIT: OP and the existing PN/WC shared version.**

  [fire/op/fmvinit.f90](fire/op/fmvinit.f90) mostly matches
  [fire/base/fmvinit.f90](fire/base/fmvinit.f90). Preserve species-24
  assignments, WC's density value and softwood classifications when sharing
  initialization.

## Larger consolidation opportunities

- [ ] **F15 — CWCALC: LS and ON.**

  [ls/cwcalc.f90](ls/cwcalc.f90) and
  [canada/on/cwcalc.f90](canada/on/cwcalc.f90) largely share the eastern
  equation banks and calculation, including one run of 992 matching
  statements after comment/whitespace normalization. Preserve ON's species
  aliases and additional lookup cases in a shared implementation.

- [ ] **F16 — GRINIT: CS, LS and NE.**

  [cs/grinit.f90](cs/grinit.f90), [ls/grinit.f90](ls/grinit.f90) and
  [ne/grinit.f90](ne/grinit.f90) repeat substantial initialization. Share
  the common assignments while preserving variant identifiers, default
  forest codes and NE's `EBAU` initialization.

- [ ] **F17 — CROWN: CA and OC.**

  [oc/crown.f90](oc/crown.f90) largely adds ORGANON handling to
  [ca/crown.f90](ca/crown.f90)'s calculation. Share the ordinary calculation
  with explicit ORGANON gates, preserving the existing conditions and
  mutation order.

- [ ] **F18 — CROWN: PN, WC and OP.**

  [pn/crown.f90](pn/crown.f90), [wc/crown.f90](wc/crown.f90) and
  [op/crown.f90](op/crown.f90) contain large matching calculation blocks.
  Preserve differing coefficient banks, mapping dimensions and ORGANON
  behavior. Treat this separately from F17.

- [ ] **F19 — HTGF: CS, LS and NE.**

  [cs/htgf.f90](cs/htgf.f90), [ls/htgf.f90](ls/htgf.f90) and
  [ne/htgf.f90](ne/htgf.f90) have closely duplicated height-growth drivers.
  Preserve variant equation selection and provide checked adapters for
  the different `BALMOD` argument contracts.

- [ ] **F20 — HTDBH: LS and ON.**

  [ls/htdbh.f90](ls/htdbh.f90) and
  [canada/on/htdbh.f90](canada/on/htdbh.f90) share the equation calculation
  and common coefficient rows. Retain ON's four additional species rows
  and table dimensions.

- [ ] **F21 — INITRE: extract individual shared keyword handlers.**

  [vbase/initre.f90](vbase/initre.f90),
  [metric/vbase/initre.f90](metric/vbase/initre.f90) and
  [canada/bc/initre.f90](canada/bc/initre.f90) total 19,252 lines.
  Volume-equation keyword processing contains hundreds of matching
  statements. Extract individual handlers, preserving unit conversions,
  diagnostics and error-return paths; avoid one wholesale merge.

- [ ] **F22 — FMIN: extract remaining shared keyword handlers.**

  [fire/base/fmin.f90](fire/base/fmin.f90) and
  [metric/fire/base/fmin.f90](metric/fire/base/fmin.f90) still contain
  several matching keyword-processing blocks. Share one handler at a time,
  retaining conversion and report-flag behavior.

- [ ] **F23 — REGENT: consolidate calculations within the shared file.**

  [vbase/regent.f90](vbase/regent.f90) is 18,527 lines and repeats
  substantial calculations among its private variant routines. Begin with
  matching growth-blending, perturbation and diameter-update blocks.
  Preserve random-number consumption and mutation order. Track each
  extraction as a separate verified step under this finding.

## Additional review opportunities

- [ ] **F24 — VARGET/VARPUT: share serialization structure.**

  Nine definitions of each remain; examples are
  [ci/varget.f90](ci/varget.f90), [ci/varput.f90](ci/varput.f90),
  [em/varget.f90](em/varget.f90) and [em/varput.f90](em/varput.f90).
  Share matching serialization structure while preserving binary field
  order, sizes and variant payloads exactly. Migrate readers and writers
  in separately verified steps.

- [ ] **F25 — Acadian support review, without deletion.**

  Current manifests select every remaining main-tree `.f90` file outside
  [acd/](acd/). Acadian files are unselected. Establish whether that model
  should remain supported or preserved before proposing any retirement.
  Completing this task means documenting the support decision; it does not
  authorize deleting code.

## Bugs recorded during this audit

All three defects below are present in pinned original F77 `c2105365` and
remain unfixed. Their full descriptions and suggested fixes are in the bug
tracker; they are separate from the consolidation tasks.

| Bug | Finding | Verification and limit |
|---|---|---|
| [40](FOUND_BUGS.md#40-cssn-fuel-model-selection-reads-unassigned-moisture-weights) | CS/SN fuel selection reads an unassigned moisture weight on dry and wet branches. | Actual original/current routines and classifiers reproduced non-finite incoming fuel weights with NaN initialization. Whole-model fire impact was not measured. |
| [41](FOUND_BUGS.md#41-cssn-redcedar-fuel-selection-divides-unweighted-heights-by-tree-density) | CS/SN cedar-height calculation divides unweighted heights by density; splitting the same stand into two records changes fuel-candidate weights. | Actual original/current routines and classifiers reproduced the changed weights. Integrated fire-output impact was not measured. |
| [42](FOUND_BUGS.md#42-bc-snag-volume-calls-the-ten-argument-cftopk-with-nine-arguments) | BC passes nine arguments to the ten-argument CFTOPK, shifting argument types and omitting the final argument. | Compiling actual caller/callee sources together produced argument-type diagnostics in original/current code. Runtime volume effects and crash frequency were not measured. |

## Original starting sequence

1. F01 — ESNUTR.
2. F02 — CS/LS/NE VARMRT.
3. F03 — CI/EC/KT/WS BFVOL.
4. F07 — CA/NC DUNN.

All four steps above are complete. Further findings require separate
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
