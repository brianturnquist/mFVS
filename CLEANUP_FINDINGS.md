# mFVS consolidation findings

Fresh source audit: 15 November 2026, checkout `1cf093f3`.

The goal is one definition of each calculation, with variant differences
expressed explicitly in coefficient banks, species mappings or bounded
branches. The findings below come from current source and the 24 build
manifests. They are proposed work; implementation has not been authorized.

Excluding include files and NVEL, the manifests select 1,286 Fortran source
files. There are 92 repeated filename families covering 666 files. These
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
not establish simulation parity. All findings remain unchecked below.

## Bounded consolidation candidates

- [ ] **F01 — ESNUTR: AK and shared establishment.**

  [ak/esnutr.f90](ak/esnutr.f90) and [estb/esnutr.f90](estb/esnutr.f90)
  have matching executable logic; declaration ordering and comments differ.
  Use one definition with the existing variant-selected includes.

- [ ] **F02 — VARMRT: CS, LS and NE.**

  [cs/varmrt.f90](cs/varmrt.f90), [ls/varmrt.f90](ls/varmrt.f90) and
  [ne/varmrt.f90](ne/varmrt.f90) differ only in the `VARADJ`
  species-tolerance table. Share the allocator and retain separate
  coefficient banks. Other VARMRT implementations require separate review.

- [ ] **F03 — BFVOL: CI, EC, KT and WS.**

  [ci/bfvol.f90](ci/bfvol.f90), [ec/bfvol.f90](ec/bfvol.f90),
  [kt/bfvol.f90](kt/bfvol.f90) and [ws/bfvol.f90](ws/bfvol.f90)
  share the calculation body. Their `D2HBRK` and `COFBVS` tables differ.
  Preserve each table's species order and extent in a shared calculation.

- [ ] **F04 — LOGS: repeated taper and log calculations.**

  Ten definitions remain. [ie/logs.f90](ie/logs.f90) and
  [wc/logs.f90](wc/logs.f90), and [ne/logs.f90](ne/logs.f90) and
  [so/logs.f90](so/logs.f90), contain matching calculation blocks.
  Extract the common taper/log calculation while retaining species mappings
  and special-case returns. Review the remaining copies separately.

- [ ] **F05 — BRATIO: IE, EM and BC.**

  [ie/bratio.f90](ie/bratio.f90), [em/bratio.f90](em/bratio.f90) and
  [canada/bc/bratio.f90](canada/bc/bratio.f90) share equation selection,
  diameter handling and output bounds. Keep bark coefficients and species
  mappings in variant-specific banks around one calculation.

- [ ] **F06 — SITERANGE: shared lookup structure.**

  Begin with [ci/siterange.f90](ci/siterange.f90),
  [so/siterange.f90](so/siterange.f90) and
  [tt/siterange.f90](tt/siterange.f90), which share the lookup structure.
  Retain separate range tables. Handle [ut/siterange.f90](ut/siterange.f90)'s
  different age-basis tables and [bm/siterange.f90](bm/siterange.f90)'s
  additional caller mode as subsequent bounded steps.

- [ ] **F07 — DUNN: CA and NC.**

  [ca/dunn.f90](ca/dunn.f90) and [nc/dunn.f90](nc/dunn.f90) share the
  interpolation/calculation. Preserve their species adjustment tables in
  one definition.

- [ ] **F08 — ESDLAY: CI, IE and establishment.**

  [ci/esdlay.f90](ci/esdlay.f90), [ie/esdlay.f90](ie/esdlay.f90) and
  [estb/esdlay.f90](estb/esdlay.f90) repeat the delay calculation around
  coefficient banks. Share the calculation while preserving table
  dimensions and species-specific values.

- [ ] **F09 — FMCBLK: IE, KT and BC.**

  [fire/ie/fmcblk.f90](fire/ie/fmcblk.f90),
  [fire/kt/fmcblk.f90](fire/kt/fmcblk.f90) and
  [canada/fire/bc/fmcblk.f90](canada/fire/bc/fmcblk.f90) have matching
  carbon-product tables and different biomass-group mappings. Centralize
  the common tables while preserving initialization before execution begins.

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

## Suggested starting sequence

1. F01 — ESNUTR.
2. F02 — CS/LS/NE VARMRT.
3. F03 — CI/EC/KT/WS BFVOL.
4. F07 — CA/NC DUNN.

These have particularly clear boundaries between shared calculation and
differing data. This ordering is a proposal, not implementation approval.

The audit's source-list check and `git diff --check` passed. No model source
was changed, and no full regression run was performed for this read-only
assessment. Temporary defect-verification files were discarded.
