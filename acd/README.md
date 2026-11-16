# Acadian variant (`acd/`)

Status, evidence and options for running the Acadian variant (ACD) in
mFVS. Compiled 16 November 2026; tasks are tracked as F26 in
[CLEANUP_FINDINGS.md](../CLEANUP_FINDINGS.md).

## Current state

- `acd/` holds 26 routines and `common/PRGPRM.f90` (108 species,
  `VARACD = 'AC'`). No mFVS source list selects them, so nothing here is
  built or tested.
- Upstream FVS has never built ACD: no source list exists at the pinned
  original `c2105365`. The code has not compiled upstream since
  `a8f62643` (June 2018) renamed TWIGCOM's `BAU` to `EBAU`; `badist`,
  `balmod` and `grinit` still use `BAU` (five references).
- The converted files are otherwise syntactically valid, but
  `common/PRGPRM.f90` lacks the `VARIANTCD` and `LMETRICVAR` constants
  that 31 shared mFVS files now use to select variant behaviour.
- ACD's species list shares 106 of NE's 108 codes, in a different order,
  and adds `NC` and `OC`.

## The University of Maine fork

The community fork **fvs-modern**
([advanced-forestry-systems/fvs-modern](https://github.com/advanced-forestry-systems/fvs-modern),
contact Aaron Weiskittel, University of Maine) builds ACD:

- `src-converted/bin/FVSacd_sourceList.txt` builds ACD from the 26 `acd/`
  routines plus LS height/crown-width routines, IE volume routines, NE's
  fire model and the STRP establishment model.
- Its `acd/badist.f90` uses `EBAU`, so the 2018 break is fixed. Its file
  set matches ours; sizes differ uniformly by about 10%, consistent with a
  different conversion style. Contents have not been compared.
- Its README reports that ACD projects New Brunswick basal area without
  bias (R² = 0.88). Calibration is applied as runtime multiplier sets
  under `config/calibrated/`. It has no ACD test decks.
- It tracks upstream FVS at `b183c10f` (6 November 2025), about a year
  older than mFVS's original `c2105365` (28 October 2026). Its base code
  also carries its own modernization changes.
- It is MIT-licensed (© 2024–2026 Weiskittel, Johnson, Marshall); mFVS is
  CC0. Running its build as a reference copies nothing into mFVS; importing
  its source changes needs the authors' agreement to release them under
  CC0.

Published University of Maine work since this code was written includes
spruce budworm modifiers (2019) and refitted diameter and height increment
equations (2022). Whether either is in fvs-modern's ACD is not known.

## Options

**A. Follow fvs-modern and use its build as the reference (preferred).**
Bring mFVS's `acd/` to fvs-modern's behaviour, build ACD from mFVS's shared
code, and require mFVS's ACD outputs to match fvs-modern's ACD build on
agreed decks. This gives ACD the same kind of parity check as the other
variants and follows the group that maintains the model. Risks: the
reference's base code differs from mFVS's (upstream `b183c10f` plus its own
changes), so some differences may not be ACD's; an NE control comparison
(F26d) measures this first. Licensing must be settled before any of its
source is copied.

**B. Revive the upstream snapshot.** Fix `BAU`, add a manifest and the
PRGPRM constants, and compare against original `c2105365` with the same
minimal fixes, as BC/ON's reference already receives link fixes. The parity
method matches the other variants exactly, but the model is the 2018-era
code and may not be what the University of Maine uses.

**C. Ask upstream to adopt ACD.** If USFS FVS builds ACD, mFVS follows
upstream as for every other variant. This depends on others' schedules.

**D. Keep it unsupported and retire it** (the first F25 decision). Simplest,
but it loses the only route to an Acadian variant in mFVS.

## Plan for option A

Tracked as F26a–F26g in [CLEANUP_FINDINGS.md](../CLEANUP_FINDINGS.md):

1. **Coordinate** with the fvs-modern authors: which ACD code is current,
   whether newer equations exist outside the repository, available test
   data, licensing, and how ACD relates to their NE-based Maine variant
   example.
2. **Build the reference** from a pinned fvs-modern commit, outside the
   repository: FVSacd, plus FVSne as a control.
3. **Create `tests/FVSacd`** decks using alpha species codes (NE's `net01`
   is a starting point), plus any University of Maine data.
4. **Run the control comparison:** NE decks in fvs-modern's NE and mFVS's
   NE, to measure base-code differences before judging ACD.
5. **Port** fvs-modern's ACD changes into `acd/` (at least `BAU` to
   `EBAU`) and add `VARIANTCD = 'AC'` and `LMETRICVAR = .false.`.
6. **Integrate:** write `bin/FVSacd_sourceList.txt` against mFVS's
   consolidated files, give `'AC'` the matching branch in every shared
   routine that selects on `VARIANTCD`, and extend the parity tools to
   the three-letter variant name.
7. **Verify** all generated files, restart files included, against the
   reference, with differences explained by the control comparison.
